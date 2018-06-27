--module Tests where

import Lib
import DataTypes
import Sample
import qualified Data.Map as Map
import System.IO
import Data.List
testGame = newGame $ map fst [("Angus",[]),("Toby",[]),("Anne",[])]

see :: [Card] -> String
see = intersperse ' ' . map uniCard
prGS :: GameState -> String
prGS GS {
       _players = players, -- current player is head of list
       _deck = deck ,
       _pile = pile ,
       _messages = messages,
       _lastMoveLegal = lastMoveLegal ,
       _prevGS = prevGS ,
       _randg = randg ,
       _hands = hands,
       _varMap = varMap
     } = unlines [
       unlines$ map (\name -> name ++ ":" ++ see (maybe [] id (Map.lookup name hands))) players,
       see pile,
       unlines$ reverse messages,
       show lastMoveLegal,
       ""]

main :: IO ()
main = do
    --let g = beginGame testGame
    _ <- testSequence  test2
    return ()

mkc :: Suit ->  Int -> Action
mkc s n = Play (toEnum (n-1),s)

sp :: Int -> Action
sp = mkc Spades
he = mkc Hearts
di = mkc Diamonds
cl = mkc Clubs

mka p c = Action p c ""

toby = mka "Toby"
angus = mka "Angus"
anne = mka "Anne"

test1 = (beginGame testGame,  [Action "Toby" (Draw 2) "", Action "Angus" (Play (Eight,Spades)) ""])-- playing cards not in hand causes error
test2 = (baseAct, beginGame testGame,  [Action "Toby" (Draw 2) "", angus (he 3), angus (sp 8),anne (he 1)])


testSequence :: (Game,GameState, [Event]) -> IO GameState
testSequence (g,state,(e: es)) = do
    let state' = g e state
    putStr$ prGS state'
    testSequence (g,state',es)
testSequence (_,s,_) = return s
