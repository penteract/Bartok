{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Data.Text(Text,intercalate,unpack)
import Data.Maybe

import Data.Aeson

import Data.Time(getCurrentTime,diffUTCTime)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types.Header
import Network.HTTP.Types
import Network.HTTP.Types.Status

import qualified Control.Concurrent.Map as CMap
import Control.Concurrent.MVar
import Control.Concurrent
--import Control.Monad.Trans.State
import Control.Monad.State
import Control.Monad.Reader

import Language.Haskell.Interpreter hiding (get)

import Serialize
import ServerInterface

--type GData = (GameState,Game,Viewer)
type GMap = CMap.Map Text (OngoingGame,MVar ())


html = (hContentType,"text/html")
--initialStoredGame = liftM2 (,) initialGame  (newMVar ())


-- | returns True if it has been more than 10 seconds since the last player action
checktime :: OngoingGame -> IO Bool
checktime gd = do
    now <- getCurrentTime
    return$ diffUTCTime now (_lastAction gd) > realToFrac 10

addGame :: Text -> GMap -> IO ()
addGame gName games = do
    og <- liftM2 (,) initialGame  (newMVar ())
    b <- CMap.insertIfAbsent gName og games
    forkIO (sendTimeouts gName games)
    return ()
    --use the code below if we can update the version of CMap
    --if b then forkIO (sendTimeouts gName games) >> return ()
    --    else return ()

sendTimeouts :: Text -> GMap -> IO ()
sendTimeouts gName games = do
    threadDelay (10*1000000) -- 10 seconds
    mx <- CMap.lookup gName games
    case mx of
        Nothing -> return ()
        Just (gd,mv) -> do
            t <- checktime gd
            when t (do -- check that it has been 10 seconds since the last move
                _ <- takeMVar mv -- aquire lock
                mx <- CMap.lookup gName games--need to read the data again now we hold the lock
                case mx of --technically runs into some wierd prblems if the game can be deleted
                    Nothing -> return ()
                    Just (gd,_) -> do
                        t <- checktime gd
                        when t (do-- check that it has been 10 seconds since the last move
                            g <- case readError (handle timeoutReq gd) of
                                    Left err -> do
                                        putMVar mv ()
                                        error err
                                    Right (x,Nothing) -> do
                                         return$ setState x gd
                                    Right (x,Just o) ->  return$ setState x o
                            CMap.insert gName (g,mv) games
                            putMVar mv ())) -- release lock
    sendTimeouts gName games

cannonisepath :: Middleware
cannonisepath app req resp =
    app req{pathInfo=filter (/="") (pathInfo req)} resp


app :: GMap -> Application
app games req resp = do
    let m = requestMethod req
    case parseMethod m of
        Right GET -> onGet games req resp
        Right POST -> onPost games req resp
        _ -> resp$ responseLBS methodNotAllowed405 [(hAllow,"GET, POST")] ""

onGet :: GMap -> Application
onGet games req = do
    --print (pathInfo req)
    case pathInfo req of
        [] -> load "home.html"
        ["doc"] -> ($ redirect308 "/doc/index.html")
        ["static"] -> ($ redirect308 "/")
        [gname] -> playPage gname games req
        -- [gname] -> resp$redirect308 (concat
        --     ["/",unpack gname,"/",B.unpack (rawQueryString req)])
        ["static",x] -> load x
        ("doc":path) -> loaddoc (intercalate "/" path)
        [gname,"newRule"] -> newRulePage gname games req
        [gname,"wait"] -> waitPage gname games req
        _ -> ($ err404)

load :: Text -> (Response -> a) -> a
load p resp = resp$ responseFile ok200 [(hContentType,getContentType p)] ("static/"++unpack p) Nothing

loaddoc :: Text -> (Response -> a) -> a
loaddoc p resp = resp$ responseFile ok200 [] ("doc/"++unpack p) Nothing

getContentType :: Text ->  B.ByteString
getContentType p = fromMaybe "text/html" (lookup p
    [("dejavupc","font/woff"),--Should this be .woff
    ("star.js","text/javascript")])
--GET handlers

-- homepage :: GMap -> Application
-- homepage games req resp = resp$ responseFile ok200 [html] "home.html" Nothing

playPage :: Text -> GMap -> Application
playPage gameName games req resp = do
    --let gameName = intercalate "/" (pathInfo req)
    --putStrLn$ show$ gameName
    mx <- CMap.lookup gameName games
    case mx of
        Nothing -> addGame gameName games
        Just a -> return ()
    load "play.html" resp

newRulePage :: Text -> GMap -> Application
newRulePage gameName games req resp= do
    --putStrLn$ show$ gameName
    mx <- CMap.lookup gameName games
    case mx of
        Nothing -> addGame gameName games
        Just a -> return ()
    load "newRule.html" resp

waitPage :: Text -> GMap -> Application
waitPage gameName games req resp = do
    --mx <- CMap.lookup gameName games
    load "wait.html" resp
--WithGame Monad

type WithGame a = ReaderT (Request,L.ByteString) (StateT OngoingGame IO) a

doWithGame :: WithGame Response -> GMap -> Text -> Application
doWithGame wg games gname req resp = do
    mx <- CMap.lookup gname games
    case mx of
        Nothing -> resp$ responseLBS badRequest400 [] ""
        Just (_,mv) -> do
            _ <- takeMVar mv -- aquire lock
            rb <- lazyRequestBody req
            -- putStrLn "Request Body:"
            -- print$ rb
            mx <- CMap.lookup gname games--need to read the data again now we hold the lock
            case mx of --technically runs into some wierd prblems if the game can be deleted
                Nothing -> resp$ responseLBS badRequest400 [] ""  --don't bother releasing the lock if the game has been deleted?
                Just (gd,_) -> do
                    (r,gd') <- runStateT (runReaderT wg (req,rb)) gd
                    CMap.insert gname (gd',mv) games
                    putMVar mv () -- release lock
                    resp$ r

doSafeWithGame :: WithGame Response -> GMap -> Text -> Application
doSafeWithGame wg games gname req resp = do
    mx <- CMap.lookup gname games
    case mx of
        Nothing -> resp$ responseLBS badRequest400 [] ""
        Just (gd,_) -> do
            rb <- lazyRequestBody req
            (r,gd') <- runStateT (runReaderT wg (req,rb)) gd
            resp$ r

getGame :: WithGame OngoingGame
getGame = get

getBody :: WithGame L.ByteString
getBody = asks snd

doErr :: MError a -> (a -> WithGame Response) -> WithGame Response
doErr e f = do
    case readError e of
        Left err -> do
            --liftIO (print err)
            return$ err422 err
        Right (x,Nothing) -> do
            --liftIO$ print "fine"
            f x
        Right (x,Just o) -> do
            --liftIO$ print "some error"
            --liftIO$ print$  _gameState o
            put o
            f x


onPost :: GMap -> Application
onPost games req resp = do
    print$ pathInfo req
    let gameName = intercalate "/" (pathInfo req)
    case pathInfo req of
        [gname] -> doWithGame playMove games gname req resp
        [gname, "newRule"] -> doWithGame newRule games gname req resp
        [gname, "poll"] -> doSafeWithGame viewGame games gname req resp
        [gname, "wait"] -> checkStatus gname games req resp
        _ -> resp$ err404

-- POST handlers


-- | Handle poll requests
viewGame :: WithGame Response
viewGame = do
    e <- getBody
    let p = L.unpack e
    let (name,tok) = span (/='\n') p
    game <- getGame
    doErr (view name (drop 1 tok) game) (\v ->
       return$ jsonResp (serialize v))

playMove :: WithGame Response
playMove = do
    e <- getBody
    --liftIO (putStrLn (L.unpack e))
    case unserialize e of
        Just r -> do
            liftIO (putStrLn (show r))
            let p = getName r
            game <- getGame
            doErr (handle (Left r) game) (\ state -> do
              modify (setState state)
              liftIO getCurrentTime >>= modify . setTime
              --liftIO (putStrLn (show (_players state)))
              game' <- getGame
              doErr (view p (getTok r) game') (\ v ->
                return$ jsonResp (serialize v)))

        Nothing -> return err404

newRule :: WithGame Response
newRule = do
    b <- getBody
    liftIO$ putStrLn "New Rule: "
    liftIO$ putStrLn (L.unpack b)
    case readNewRule b of
        Just nr -> do
            let imps = ["Prelude"]++[i | i <- imports nr , i `elem`
                    ["RuleHelpers","BaseGame","DataTypes","Rules", "TLib","Views"]]
            liftIO$ print$ imps
            liftIO$ putStrLn$ code nr
            f <- liftIO$ runInterpreter$ do
                           setImports imps
                           case ruleType nr of
                               "ViewRule" -> (,) id <$> interpret (code nr) (as::ViewRule)
                               "Both" -> interpret (code nr) (as::Rule')
                               _ -> flip (,) id <$> interpret (code nr) (as::Rule)
            case f of
                Left err -> do
                    liftIO$ putStrLn$ show err
                    return$ jsonResp$ "{\"tag\":\"Error\",\"contents\":"`L.append`(encode.toJSON$ fromErr err) `L.append`"}"
                Right r -> do
                    modify$ restartWithNewRule "" r
                    return$ jsonResp "{\"tag\":\"Redirect\"}"
        Nothing -> return err400

checkStatus :: Text -> GMap -> Application
checkStatus gameName games req resp = do
    mx <- CMap.lookup gameName games
    resp$ textResp$ case mx of
        Nothing -> "home"
        Just (og,_) -> case getWinner og of
            Just p -> "unfinished"
            Nothing ->  "resume"


fromErr (WontCompile errs) = Prelude.unlines (map frghc errs)

frghc (GhcError{errMsg=m}) = m

jsonResp = responseLBS ok200 [(hContentType,"application/json")]

main :: IO ()
main = do
    args <- getArgs
    let port = case args of
                (n:_) -> (read n ::Int)
                [] -> 8080
    games <- CMap.empty
    putStrLn ("server running at http://localhost:"++show port)
    run port (cannonisepath (app games))


redirect308 url = responseLBS permanentRedirect308 [] (L.pack url)

textResp txt = responseLBS ok200 [(hContentType,"text/plain")] (L.pack txt)

err422 err = responseLBS unprocessableEntity422 [] (L.pack err)

err404 = responseLBS notFound404 []
  "<html><head><title>404: Page Not Found</title></head>\
    \ <body><h1>Page Not Found</h1></body></html>"
err400 = responseLBS badRequest400 []
  "<html><head><title>400: Bad Request</title></head>\
    \ <body><h1>Bad Request</h1></body></html>"
