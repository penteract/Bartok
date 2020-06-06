{-# LANGUAGE OverloadedStrings #-}

--import Control.Monad.Trans.State

import Control.Arrow (second)
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Control.Concurrent.Map as CMap
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (isSuffixOf)
import Data.Maybe
import Data.Text (Text, intercalate, unpack)
import Data.Time (diffUTCTime, getCurrentTime)
import Game.Bartok.Serialize
import Game.Bartok.ServerInterface
import Game.Bartok.Whitelist (allowed)
import Language.Haskell.Interpreter hiding (get)
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment

--type GData = (GameState,Game,Viewer)
type GMap = CMap.Map Text (OngoingGame, MVar ())

html = (hContentType, "text/html")

--initialStoredGame = liftM2 (,) initialGame  (newMVar ())

-- | returns True if it has been more than 10 seconds since the last player action
checktime :: OngoingGame -> IO Bool
checktime gd = do
  now <- getCurrentTime
  return $ diffUTCTime now (_lastAction gd) > realToFrac 10

-- | Returns true if it has been more than an hour
checklongtime :: OngoingGame -> IO Bool
checklongtime gd = do
  now <- getCurrentTime
  return $ diffUTCTime now (_lastAction gd) > realToFrac (60 * 60 * 60)

addGame :: Text -> GMap -> IO ()
addGame gName games = do
  og <- liftM2 (,) initialGame (newMVar ())
  b <- CMap.insertIfAbsent gName og games
  forkIO (sendTimeouts gName games)
  return ()

--use the code below if we can update the version of CMap
--if b then forkIO (sendTimeouts gName games) >> return ()
--    else return ()

sendTimeouts :: Text -> GMap -> IO ()
sendTimeouts gName games = do
  threadDelay (10 * 1000000) -- 10 seconds
  mx <- CMap.lookup gName games
  case mx of
    Nothing -> return ()
    Just (gd, mv) -> do
      t <- checktime gd
      when
        t
        ( do
            -- check that it has been 10 seconds since the last move
            _ <- takeMVar mv -- aquire lock
            mx <- CMap.lookup gName games --need to read the data again now we hold the lock
            case mx of --technically runs into some weird problems if the game can be deleted
              Nothing -> return ()
              Just (gd, _) -> do
                t <- checktime gd
                when
                  t
                  ( do
                      -- check that it has been 10 seconds since the last move
                      lt <- checklongtime gd
                      if not lt
                        then do
                          g <- case readError (handle timeoutReq gd) of
                            Left err -> do
                              putMVar mv ()
                              error err
                            Right (x, Nothing) -> do
                              return $ setState x gd
                            Right (x, Just o) -> return $ setState x o
                          CMap.insert gName (g, mv) games
                          putMVar mv () -- release lock
                        else do
                          CMap.delete gName games -- delete the game if it's boring (nothing for an hour)
                          putMVar mv () -- Now I technically should do insertIfPresent everywhere
                          myThreadId >>= killThread
                  )
        )
  sendTimeouts gName games

cannonisepath :: Middleware
cannonisepath app req resp =
  app req {pathInfo = filter (/= "") (pathInfo req)} resp

app :: GMap -> Application
app games req resp = do
  let m = requestMethod req
  case parseMethod m of
    Right GET -> onGet games req resp
    Right POST -> onPost games req resp
    _ -> resp $ responseLBS methodNotAllowed405 [(hAllow, "GET, POST")] ""

onGet :: GMap -> Application
onGet games req =
  --print (pathInfo req)
  case pathInfo req of
    [] -> load "static/home.html"
    ["doc"] -> ($ redirect308 "/doc/index.html")
    ["static"] -> ($ redirect308 "/")
    [gname] -> playPage gname games req
    -- [gname] -> resp$redirect308 (concat
    --     ["/",unpack gname,"/",B.unpack (rawQueryString req)])
    ["static", x] -> loadstatic x
    ("doc" : path) -> loaddoc (intercalate "/" path)
    [gname, "newRule"] -> newRulePage gname games req
    [gname, "wait"] -> waitPage gname games req
    _ -> ($ err404)

load :: String -> (Response -> a) -> a
load p resp =
  if allowed p
    then resp $ responseFile ok200 [(hContentType, getContentType p)] p Nothing
    else resp $ err404

loaddoc :: Text -> (Response -> a) -> a
loaddoc p = load ("doc/" ++ unpack p)

loadstatic :: Text -> (Response -> a) -> a
loadstatic p = load ("static/" ++ unpack p)

getContentType :: String -> B.ByteString
getContentType p
  | endsWith ".js" = "text/javascript"
  | endsWith ".css" = "text/css"
  | endsWith ".woff" = "font/woff"
  | endsWith ".html" = "text/html"
  | otherwise = "text/html"
  where
    endsWith = (`isSuffixOf` p)

-- homepage :: GMap -> Application
-- homepage games req resp = resp$ responseFile ok200 [html] "home.html" Nothing

istest :: String -> Bool
istest ('T' : 'S' : 'T' : x) = True
istest _ = False

playPage :: Text -> GMap -> Application
playPage gameName games req resp = do
  --let gameName = intercalate "/" (pathInfo req)
  --putStrLn$ show$ gameName
  mx <- CMap.lookup gameName games
  case mx of
    Nothing -> addGame gameName games
    Just a -> return ()
  if istest (unpack gameName)
    then loadstatic "Testing.html" resp
    else loadstatic "play.html" resp

newRulePage :: Text -> GMap -> Application
newRulePage gameName games req resp = do
  --putStrLn$ show$ gameName
  mx <- CMap.lookup gameName games
  case mx of
    Nothing -> addGame gameName games
    Just a -> return ()
  loadstatic "newRule.html" resp

waitPage :: Text -> GMap -> Application
waitPage gameName games req resp = do
  --mx <- CMap.lookup gameName games
  loadstatic "wait.html" resp

--WithGame Monad

type WithGame a = ReaderT (Request, L.ByteString) (StateT OngoingGame IO) a

doWithGame :: WithGame Response -> GMap -> Text -> Application
doWithGame wg games gname req resp = do
  mx <- CMap.lookup gname games
  case mx of
    Nothing -> resp $ responseLBS badRequest400 [] ""
    Just (_, mv) -> do
      _ <- takeMVar mv -- aquire lock
      rb <- lazyRequestBody req
      -- putStrLn "Request Body:"
      -- print$ rb
      mx <- CMap.lookup gname games --need to read the data again now we hold the lock
      case mx of --technically runs into some wierd prblems if the game can be deleted
        Nothing -> resp $ responseLBS badRequest400 [] "" --don't bother releasing the lock if the game has been deleted?
        Just (gd, _) -> do
          (r, gd') <- runStateT (runReaderT wg (req, rb)) gd
          CMap.insert gname (gd', mv) games
          putMVar mv () -- release lock
          resp $ r

doSafeWithGame :: WithGame Response -> GMap -> Text -> Application
doSafeWithGame wg games gname req resp = do
  mx <- CMap.lookup gname games
  case mx of
    Nothing -> resp $ responseLBS badRequest400 [] ""
    Just (gd, _) -> do
      rb <- lazyRequestBody req
      (r, gd') <- runStateT (runReaderT wg (req, rb)) gd
      resp $ r

getGame :: WithGame OngoingGame
getGame = get

getBody :: WithGame L.ByteString
getBody = asks snd

doErr :: MError a -> (a -> WithGame Response) -> WithGame Response
doErr e f = do
  case readError e of
    Left err -> do
      liftIO (print err)
      return $ err422 err
    Right (x, Nothing) -> do
      --liftIO$ print "fine"
      f x
    Right (x, Just o) -> do
      --liftIO$ print "some error"
      --liftIO$ print$  _gameState o
      put o
      f x

onPost :: GMap -> Application
onPost games req resp = do
  --
  let gameName = intercalate "/" (pathInfo req)
  case pathInfo req of
    [gname] -> doWithGame playMove games gname req resp
    [gname, "newRule"] -> doWithGame newRule games gname req resp
    [gname, "poll"] -> doSafeWithGame viewGame games gname req resp
    [gname, "wait"] -> checkStatus gname games req resp
    _ -> do
      print $ pathInfo req
      resp $ err404

-- POST handlers

-- | Handle poll requests
viewGame :: WithGame Response
viewGame = do
  e <- getBody
  let p = L.unpack e
  let (name, (tok, time)) = second (span (/= '\n') . drop 1) $ (span (/= '\n')) p
  game <- getGame
  case reads (drop 1 time) of
    ((n, "") : _) ->
      doErr
        (view name tok n game)
        ( \v ->
            return $ jsonResp (serialize v)
        )
    _ -> return $ err422 "Unable to parse counter"

playMove :: WithGame Response
playMove = do
  e <- getBody
  --liftIO (putStrLn (L.unpack e))
  case unserialize e of
    Just r -> do
      liftIO (putStrLn (show r))
      let p = getName r
      game <- getGame
      doErr
        (handle (Left r) game)
        ( \state -> do
            modify (setState state)
            liftIO getCurrentTime >>= modify . setTime
            --liftIO (putStrLn (show (_players state)))
            game' <- getGame
            doErr
              (view p (getTok r) (getCount r) game')
              ( \v ->
                  return $ jsonResp (serialize v)
              )
        )
    Nothing -> return err404

newRule :: WithGame Response
newRule = do
  b <- getBody
  liftIO $ putStrLn "New Rule: "
  liftIO $ putStrLn (L.unpack b)
  case readNewRule b of
    Just nr -> do
      let available =
            [ "Prelude",
              "Game.Bartok.RuleHelpers",
              "Game.Bartok.BaseGame",
              "Game.Bartok.DataTypes",
              "Game.Bartok.Rules",
              "Game.Bartok.TLib",
              "Game.Bartok.Views"
            ]
          imps =
            ["Prelude"]
              ++ [ i | i <- imports nr, i
                                          `elem` available
                 ]
          qimps =
            (zip imps $ repeat Nothing)
              ++ map
                (second Just)
                ( [ ("Data.Map", "Map"),
                    ("Control.Arrow", "Arrow"),
                    ("Data.List.NonEmpty", "NE"),
                    ("Control.Monad", "Monad"),
                    ("Control.Applicative", "Applicative")
                  ]
                    ++ map (\a -> (a, a)) available
                )
      liftIO $ print $ imps
      liftIO $ putStrLn $ code nr
      f <- liftIO $ runInterpreter $ do
        setImportsQ qimps
        case ruleType nr of
          "ViewRule" -> (,) id <$> interpret (code nr) (as :: ViewRule)
          "Both" -> interpret (code nr) (as :: Rule')
          _ -> flip (,) id <$> interpret (code nr) (as :: Rule)
      case f of
        Left err -> do
          liftIO $ putStrLn $ show err
          return $ jsonResp $ "{\"tag\":\"Error\",\"contents\":" `L.append` (encode . toJSON $ fromErr err) `L.append` "}"
        Right r -> do
          modify $ restartWithNewRule "" r
          return $ jsonResp "{\"tag\":\"Redirect\"}"
    Nothing -> return err400

checkStatus :: Text -> GMap -> Application
checkStatus gameName games req resp = do
  mx <- CMap.lookup gameName games
  resp $ textResp $ case mx of
    Nothing -> "home"
    Just (og, _) -> case getWinner og of
      Just p -> "unfinished"
      Nothing -> "resume"

fromErr (WontCompile errs) = Prelude.unlines (map frghc errs)

frghc (GhcError {errMsg = m}) = m

jsonResp = responseLBS ok200 [(hContentType, "application/json")]

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        (n : _) -> (read n :: Int)
        [] -> 8080
  games <- CMap.empty
  putStrLn ("server running at http://localhost:" ++ show port)
  run port (cannonisepath (app games))

{-
allowedFiles :: IO (String -> Bool)
allowedFiles = do
  docs <- readProcess "find" ["doc", "-type", "f"] ""
  statics <- readProcess "find" ["static", "-type", "f"] ""
  return$ (`elem` lines docs ++ lines statics)-}

redirect308 url = responseLBS permanentRedirect308 [] (L.pack url)

textResp txt = responseLBS ok200 [(hContentType, "text/plain")] (L.pack txt)

err422 err = responseLBS unprocessableEntity422 [] (L.pack err)

err404 =
  responseLBS
    notFound404
    []
    "<html><head><title>404: Page Not Found</title></head>\
    \ <body><h1>Page Not Found</h1></body></html>"

err400 =
  responseLBS
    badRequest400
    []
    "<html><head><title>400: Bad Request</title></head>\
    \ <body><h1>Bad Request</h1></body></html>"
