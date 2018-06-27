--module Tests where

import Lib
import Sample
import qualified Data.Map
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
       _varMap = varMap
     } = unlines [
       unlines$ map (\(name,cards) -> name ++ ":" ++ see cards) players,
       see pile,
       unlines messages,
       show lastMoveLegal,
       ""]

main :: IO ()
main = do
    --let g = beginGame testGame
    _ <- uncurry (flip (testSequence baseAct)) test2
    return ()
    --putStr$ prGS g
    --let g' = baseAct (Action "Toby" (Draw 2) "") g
    --putStr$ prGS g'
    --hPutStr stderr "hello world asdfsdafsdafsdfasdfkljkljkljskajdflkj asdjfklsdajflkjsdafkljskldajfklsdafjkl jaslkfj "
    --putStrLn (show (_deck g))
    --return ()

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

test1 = (beginGame testGame,  [Action "Toby" (Draw 2) "", Action "Angus" (Play (Eight,Spades)) ""])
test2 = (beginGame testGame,  [Action "Toby" (Draw 2) "", angus (he 3)])


testSequence :: Game -> [Event] -> GameState -> IO GameState
testSequence g (e: es) state = do
    let state' = g e state
    putStr$ prGS state'
    testSequence g es state'
testSequence _ _ s = return s
