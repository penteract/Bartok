{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import qualified Control.Concurrent.Map as CMap
import Control.Monad.Extra (void, whenJustM, whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (StateT, get, modify, put, runStateT)
import Data.Aeson (encode, toJSON)
import Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime)
import Game.Bartok.Serialize
  ( code,
    getCount,
    getName,
    getTok,
    imports,
    readNewRule,
    ruleType,
    serialize,
    unserialize,
  )
import Game.Bartok.ServerInterface
  ( MError,
    OngoingGame,
    Rule,
    Rule',
    ViewRule,
    getWinner,
    handle,
    initialGame,
    readError,
    restartWithNewRule,
    setState,
    setTime,
    timeoutReq,
    view,
    _lastAction,
  )
import Game.Bartok.Whitelist (allowed)
import Language.Haskell.Interpreter
  ( GhcError (GhcError),
    InterpreterError (..),
    as,
    errMsg,
    interpret,
    runInterpreter,
    setImportsQ,
  )
import Network.HTTP.Types
  ( StdMethod (GET, POST),
    badRequest400,
    hContentType,
    methodNotAllowed405,
    notFound404,
    ok200,
    parseMethod,
    permanentRedirect308,
    unprocessableEntity422,
  )
import Network.HTTP.Types.Header (hAllow)
import Network.Wai
  ( Application,
    Middleware,
    Request,
    Response,
    lazyRequestBody,
    pathInfo,
    requestMethod,
    responseFile,
    responseLBS,
  )
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import Utils (whenNothingM_)

type GMap = CMap.Map Text (OngoingGame, MVar ())

-- | returns True if it has been more than 10 seconds since the last player action
checktime :: OngoingGame -> IO Bool
checktime gd = do
  now <- getCurrentTime
  pure $ diffUTCTime now (_lastAction gd) > realToFrac @Int 10

-- | Returns true if it has been more than an hour
checklongtime :: OngoingGame -> IO Bool
checklongtime gd = do
  now <- getCurrentTime
  pure $ diffUTCTime now (_lastAction gd) > realToFrac @Int (60 * 60 * 60)

addGame :: Text -> GMap -> IO ()
addGame gName games = do
  og <- (,) <$> initialGame <*> newMVar ()
  void $ CMap.insertIfAbsent gName og games
  void $ forkIO (sendTimeouts gName games)

sendTimeouts :: Text -> GMap -> IO ()
sendTimeouts gName games = do
  threadDelay (10 * 1000000) -- 10 seconds
  whenJustM (gName `CMap.lookup` games) $ \(gd, mv) ->
    whenM (checktime gd) $ do
      -- check that it has been 10 seconds since the last move
      void $ takeMVar mv -- aquire lock
      whenJustM (fmap fst <$> gName `CMap.lookup` games) $ \gd' -> do
        -- reread following lock acquisition
        whenM (checktime gd') $ do
          -- check that it has been 10 seconds since the last move
          lt <- checklongtime gd'
          if not lt
            then do
              g <- case readError (handle timeoutReq gd') of
                Left err -> do
                  putMVar mv ()
                  error err
                Right (x, mo) -> do
                  pure $ setState x $ fromMaybe gd' mo
              void $ CMap.insert gName (g, mv) games
              putMVar mv ()
            else do
              void $ gName `CMap.delete` games -- delete the game if it's boring (nothing for an hour)
              putMVar mv () -- Now I technically should do insertIfPresent everywhere
              myThreadId >>= killThread
  sendTimeouts gName games

cannonisepath :: Middleware
cannonisepath app req resp =
  app req {pathInfo = filter ("" /=) (pathInfo req)} resp

makeApp :: GMap -> Application
makeApp games req resp = do
  let m = requestMethod req
  case parseMethod m of
    Right GET -> onGet games req resp
    Right POST -> onPost games req resp
    _ -> resp $ responseLBS methodNotAllowed405 [(hAllow, "GET, POST")] ""

onGet :: GMap -> Application
onGet games req =
  case pathInfo req of
    [] -> load "static/home.html"
    ["doc"] -> ($ redirect308 "/doc/index.html")
    ["static"] -> ($ redirect308 "/")
    [gname] -> playPage gname games req
    ["static", x] -> loadstatic x
    ("doc" : path) -> loaddoc (T.intercalate "/" path)
    [gname, "newRule"] -> newRulePage gname games req
    [gname, "wait"] -> waitPage gname games req
    _ -> ($ err404)

load :: String -> (Response -> a) -> a
load p resp =
  if allowed p
    then resp $ responseFile ok200 [(hContentType, getContentType p)] p Nothing
    else resp $ err404

loaddoc :: Text -> (Response -> a) -> a
loaddoc p = load $ "doc/" <> T.unpack p

loadstatic :: Text -> (Response -> a) -> a
loadstatic p = load $ "static/" <> T.unpack p

getContentType :: String -> B.ByteString
getContentType p
  | endsWith ".js" = "text/javascript"
  | endsWith ".css" = "text/css"
  | endsWith ".woff" = "font/woff"
  | endsWith ".html" = "text/html"
  | otherwise = "text/html"
  where
    endsWith = (`isSuffixOf` p)

playPage :: Text -> GMap -> Application
playPage gameName games _req resp = do
  whenNothingM_ (gameName `CMap.lookup` games) $
    addGame gameName games
  let pageName :: Text
      pageName = if "TST" `T.isPrefixOf` gameName then "Testing" else "play"
  loadstatic (pageName <> ".html") resp

newRulePage :: Text -> GMap -> Application
newRulePage gameName games _req resp = do
  whenNothingM_ (gameName `CMap.lookup` games) $
    addGame gameName games
  loadstatic "newRule.html" resp

waitPage :: Text -> GMap -> Application
waitPage _gameName _games _req resp = do
  loadstatic "wait.html" resp

type WithGame a = ReaderT (Request, L.ByteString) (StateT OngoingGame IO) a

doWithGame :: WithGame Response -> GMap -> Text -> Application
doWithGame wg games gname req resp =
  CMap.lookup gname games >>= \case
    Nothing -> resp $ responseLBS badRequest400 [] ""
    Just (_, mv) -> do
      void $ takeMVar mv -- aquire lock
      rb <- lazyRequestBody req
      gname `CMap.lookup` games >>= \case
        --need to read the data again now we hold the lock
        Nothing -> resp $ responseLBS badRequest400 [] "" --don't bother releasing the lock if the game has been deleted?
        Just (gd, _) -> do
          (r, gd') <- runStateT (runReaderT wg (req, rb)) gd
          void $ CMap.insert gname (gd', mv) games
          putMVar mv () -- release lock
          resp r

doSafeWithGame :: WithGame Response -> GMap -> Text -> Application
doSafeWithGame wg games gname req resp =
  CMap.lookup gname games >>= \case
    Nothing -> resp $ responseLBS badRequest400 [] ""
    Just (gd, _) -> do
      rb <- lazyRequestBody req
      (r, _) <- runStateT (runReaderT wg (req, rb)) gd
      resp r

getGame :: WithGame OngoingGame
getGame = get

getBody :: WithGame L.ByteString
getBody = asks snd

doErr :: MError a -> (a -> WithGame Response) -> WithGame Response
doErr e f = do
  case readError e of
    Left err -> do
      liftIO (print err)
      pure $ err422 err
    Right (x, Nothing) -> do
      f x
    Right (x, Just o) -> do
      put o
      f x

onPost :: GMap -> Application
onPost games req resp =
  case pathInfo req of
    [gname] -> doWithGame playMove games gname req resp
    [gname, "newRule"] -> doWithGame newRule games gname req resp
    [gname, "poll"] -> doSafeWithGame viewGame games gname req resp
    [gname, "wait"] -> checkStatus gname games req resp
    _ -> do
      print $ pathInfo req
      resp err404

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
            pure $ jsonResp (serialize v)
        )
    _ -> pure $ err422 "Unable to parse counter"

playMove :: WithGame Response
playMove = do
  e <- getBody
  case unserialize e of
    Nothing -> pure err404
    Just r -> do
      liftIO $ print r
      let p = getName r
      game <- getGame
      doErr
        (handle (Left r) game)
        ( \state -> do
            modify $ setState state
            liftIO getCurrentTime >>= modify . setTime
            game' <- getGame
            doErr
              (view p (getTok r) (getCount r) game')
              ( \v ->
                  pure $ jsonResp (serialize v)
              )
        )

newRule :: WithGame Response
newRule = do
  b <- getBody
  liftIO $ putStrLn "New Rule: "
  liftIO $ putStrLn (L.unpack b)
  case readNewRule b of
    Just nr -> do
      let available :: [String]
          available =
            [ "Prelude",
              "Game.Bartok.RuleHelpers",
              "Game.Bartok.BaseGame",
              "Game.Bartok.DataTypes",
              "Game.Bartok.Rules",
              "Game.Bartok.TLib",
              "Game.Bartok.Views"
            ]
          imps = "Prelude" : filter (`elem` available) (imports nr)
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
          "ViewRule" -> (id,) <$> interpret (code nr) (as :: ViewRule)
          "Both" -> interpret (code nr) (as :: Rule')
          _ -> flip (,) id <$> interpret (code nr) (as :: Rule)
      case f of
        Left err -> do
          liftIO $ putStrLn $ show err
          pure $ jsonResp $ "{\"tag\":\"Error\",\"contents\":" `L.append` (encode . toJSON $ fromErr err) `L.append` "}"
        Right r -> do
          modify $ restartWithNewRule "" r
          pure $ jsonResp "{\"tag\":\"Redirect\"}"
    Nothing -> pure err400

checkStatus :: Text -> GMap -> Application
checkStatus gameName games _req resp = do
  mx <- CMap.lookup gameName games
  resp $ textResp $ case mx of
    Nothing -> "home"
    Just (og, _) -> case getWinner og of
      Just _ -> "unfinished"
      Nothing -> "resume"

fromErr :: InterpreterError -> String
fromErr = \case
  GhcException err -> err
  NotAllowed err -> err
  UnknownError err -> err
  WontCompile errs -> f errs
  where
    f = Prelude.unlines . map frghc
    frghc (GhcError {errMsg = m}) = m

jsonResp :: L.ByteString -> Response
jsonResp = responseLBS ok200 [(hContentType, "application/json")]

main :: IO ()
main = do
  args <- getArgs
  let port = fromMaybe 8080 $ read @Int <$> listToMaybe args
  games <- CMap.empty
  putStrLn ("server running at http://localhost:" ++ show port)
  run port $ cannonisepath $ makeApp games

redirect308 :: String -> Response
redirect308 url = responseLBS permanentRedirect308 [] (L.pack url)

textResp :: String -> Response
textResp txt = responseLBS ok200 [(hContentType, "text/plain")] (L.pack txt)

err422 :: String -> Response
err422 err = responseLBS unprocessableEntity422 [] (L.pack err)

err404 :: Response
err404 =
  responseLBS
    notFound404
    []
    "<html><head><title>404: Page Not Found</title></head>\
    \ <body><h1>Page Not Found</h1></body></html>"

err400 :: Response
err400 =
  responseLBS
    badRequest400
    []
    "<html><head><title>400: Bad Request</title></head>\
    \ <body><h1>Bad Request</h1></body></html>"
