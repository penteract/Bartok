--module Tests where

import Lib
import Sample
import qualified Data.Map
import System.IO

testGame = newGame $ map fst [("Angus",[]),("Toby",[]),("Anne",[])]

prGS :: GameState -> String
prGS GS {
       _players = players, -- current player is head of list
       _deck = deck ,
       _pile = pile ,
       _messages = messages,
       _lastMoveLegal = lastMoveLegal ,
       _prevGS = prevGS ,
       _randg = randg ,
       _varMap = varMap
     } = unlines [
       unlines$ map (\(name,cards) -> name ++ ":" ++ map uniCard cards) players,
       "",

       unlines messages,
       show lastMoveLegal,
       ""]

main :: IO ()
main = do
    let g = beginGame testGame
    putStr$ prGS g
    --hPutStr stderr "hello world asdfsdafsdafsdfasdfkljkljkljskajdflkj asdjfklsdajflkjsdafkljskldajfklsdafjkl jaslkfj "
    --putStrLn (show (_deck g))
    --return ()
