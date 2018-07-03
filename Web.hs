{-# LANGUAGE OverloadedStrings #-}
import Data.Text(Text,intercalate)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import System.Environment

import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types.Header
import Network.HTTP.Types
import Network.HTTP.Types.Status

import qualified Control.Concurrent.Map as CMap
import Control.Concurrent.MVar
--import Control.Monad.Trans.State
import Control.Monad.State
import Control.Monad.Reader

import Language.Haskell.Interpreter hiding (get)

import DataTypes
import Lib
import Views

import Serialize
import ServerInterface

--type GData = (GameState,Game,Viewer)
type GMap = CMap.Map Text OngoingGame



app :: GMap -> Application
app games req resp = do
    let m = requestMethod req
    case parseMethod m of
        Right GET -> onGet games req resp
        Right POST -> onPost games req resp
        _ -> resp$ responseLBS methodNotAllowed405 [(hAllow,"GET, POST")] ""

onGet :: GMap -> Application
onGet games req resp = do
    (case pathInfo req of
        [] -> homepage
        [gname] -> playPage gname
        [gname,"newRule"] -> newRulePage gname
        _ -> const$const($err404)) games req resp

playPage :: Text -> GMap -> Application
playPage gameName games req resp = do
    --let gameName = intercalate "/" (pathInfo req)
    putStrLn$ show$ gameName
    mx <- CMap.lookup gameName games
    case mx of
        Nothing -> do
            og <- initialGame
            CMap.insertIfAbsent gameName og games
        Just a -> return ()
    resp$ responseFile ok200 [] "index.html" Nothing

newRulePage :: Text -> GMap -> Application
newRulePage gameName games req resp= do
    putStrLn$ show$ gameName
    mx <- CMap.lookup gameName games
    case mx of
        Nothing -> do
            og <- initialGame
            CMap.insertIfAbsent gameName og games
        Just a -> return ()
    resp$ responseFile ok200 [] "newRule.html" Nothing

homepage :: GMap -> Application
homepage games req resp = resp err404

doWithGame :: WithGame Response -> GMap -> Text -> Application
doWithGame wg games gname req resp = do
    mx <- CMap.lookup gname games
    case mx of
        Nothing -> resp$ responseLBS badRequest400 [] ""
        Just gd -> do
            rb <- lazyRequestBody req
            putStrLn "Requst Body:"
            print$ rb
            (r,gd') <- runStateT (runReaderT wg (req,rb)) gd
            CMap.insert gname gd' games
            resp$ r


onPost :: GMap -> Application
onPost games req resp = do
    let gameName = intercalate "/" (pathInfo req)
    case pathInfo req of
        [gname] -> doWithGame playMove games gname req resp
        [gname, "newRule"] -> doWithGame newRule games gname req resp
        _ -> resp$ err404


type WithGame a = ReaderT (Request,L.ByteString) (StateT OngoingGame IO) a

-- getGS :: WithGame GameState
-- getGS = gets (\(st,act,vw)->st)
-- setGS :: GameState -> WithGame ()
-- setGS st = modify (\(_,act,vw)->(st,act,vw))
--
-- getViewer :: WithGame Viewer
-- getViewer = gets (\(st,act,vw)->vw)
--
getGame :: WithGame OngoingGame
getGame = get

getBody :: WithGame L.ByteString
getBody = asks snd

fromErr (WontCompile errs) = Prelude.unlines (map frghc errs)

frghc (GhcError{errMsg=m}) = m

doErr :: MError a -> (a -> WithGame Response) -> WithGame Response
doErr e f = do
    case readError e of
        Left err -> return$ err422 err
        Right (x,Nothing) -> f x
        Right (x,Just o) -> put o >> f x

--putState :: GameState -> WithGame ()
--putState gs = modify (setState gs)

playMove :: WithGame Response
playMove = do
    e <- getBody
    case unserialize e >>= (\e -> (,) e <$> getName e) of
        Just (e,n) -> do
            game <- getGame
            doErr (handle e game) (\ state -> do
             modify (setState state)
             game <- getGame
             doErr (view n game) (\ v ->
                return$ jsonResp (serialize v)))

        Nothing -> return err404

newRule :: WithGame Response
newRule = do
    b <- asks snd
    f <- liftIO$ runInterpreter$ interpret (L.unpack b) (as::Rule)
    case f of
        Left err -> return$ err422 (show err)
        Right r -> do
            modify$ addRule r
            return$ jsonResp "{}"

jsonResp = responseLBS ok200 [(hContentType,"application/json")]

main :: IO ()
main = do
    args <- getArgs
    let port = case args of
                (n:_) -> (read n ::Int)
                [] -> 8080
    games <- CMap.empty
    run port (app games)

err422 err = responseLBS unprocessableEntity422 [] (L.pack err)

err404 = responseLBS notFound404 []
  "<html><head><title>404: Page Not Found</title></head>\
    \ <body><h1>Page Not Found</h1></body></html>"
err400 = responseLBS badRequest400 []
  "<html><head><title>400: Bad Request</title></head>\
    \ <body><h1>Bad Request</h1></body></html>"
