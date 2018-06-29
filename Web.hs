{-# LANGUAGE OverloadedStrings #-}
import Data.Text
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types.Header
import Network.HTTP.Types

import qualified Control.Concurrent.Map as CMap
import Control.Concurrent.MVar

import DataTypes
import Lib

type GMap = CMap.Map Text (GameState,Game)

fix f = f (fix f)

makeNewGame = return$ newGame ["Toby","Angus"]

--TODO(angus) write this function in a separate module
--serialise :: GameView -> L.ByteString
serialise :: GameState -> L.ByteString -- TODO(toby): change when Views is a thing
serialise gs = ""

app :: GMap -> Application
app games req resp = do
    let m = requestMethod req
    case parseMethod m of
        Right GET -> onGet games req resp
        Right POST -> onPost games req resp
        _ -> resp$ responseLBS methodNotAllowed405 [(hAllow,"GET, POST")] ""

onGet :: GMap -> Application
onGet games req resp = do
    let gameName = intercalate "/" (pathInfo req)
    putStrLn$ show$ gameName
    mx <- CMap.lookup gameName games
    case mx of
        Nothing -> do
                gs <- makeNewGame
                CMap.insertIfAbsent gameName (gs,baseAct) games
        Just a -> return ()
    resp$ responseFile ok200 [] "index.html" Nothing

onPost :: GMap -> Application
onPost games req resp = do
    let gameName = intercalate "/" (pathInfo req)
    putStrLn$ show$ gameName
    (gs,g) <- fix (\x -> do
        mx <- CMap.lookup gameName games
        case mx of
            Nothing -> do
                gs <- makeNewGame
                b <- CMap.insertIfAbsent gameName (gs,baseAct) games
                x
            Just a -> return a)

    resp$ responseLBS ok200 [] (serialise gs)

main :: IO ()
main = do
    games <- CMap.empty
    run 8080 (app games)
