--module Tests where

import Lib
import Game.Bartok.DataTypes
import Game.Bartok.Sample
import qualified Data.Map as Map
import System.IO
import Data.List
import Control.Lens
import System.Random
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty(NonEmpty((:|)) )

testGame = newGame $ "Angus" : ["Toby","Anne"]

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
       unlines$ reverse messages,
       unlines$ map (\name -> name ++ ":" ++ see (maybe [] id (Map.lookup name hands))) players,
       see$ NE.toList pile,
       show lastMoveLegal,
       ""]

main :: IO ()
main = do
    --let g = beginGame testGame
    pst test1
    pst test2
    pst test7s
    --gs <- testSequence test1 True
    --putStrLn "asdfasdf asdf  sdaf asdf a sdf"
    return ()

--pst =  (>>= putStr.prGS) . flip testSequence False
pst t = testSequence t False >>= putStr.prGS

mkc :: Suit ->  Int -> Action
mkc s n = Play (toEnum n,s)

sp :: Int -> Action
sp = mkc Spades
he = mkc Hearts
di = mkc Diamonds
cl = mkc Clubs

unPlay (Play c) = c

mka p c = Action p c ""

toby = mka "Toby"
angus = mka "Angus"
anne = mka "Anne"

test1 = (baseAct, beginGame testGame,  [Action "Toby" (Draw 2) "", Action "Angus" (Play (Eight,Spades)) ""])
test2 = (baseAct, beginGame testGame,  [Action "Toby" (Draw 2) "", angus (he 11), angus (sp 11),anne (di 11),
                                       toby$ he 1, angus (Draw 1)])


newTestGame ::  [(String,[Card])] -> GameState
newTestGame hands = ((pile /\ deck) %~ (\(_,y:ys) -> (y:|[],ys)))
           GS { _deck = [c |c <- [minBound..] , not (c `elem` (hands>>=snd)) ]
              , _pile = undefined -- put a card so it's happy for now
              , _messages = []
              , _lastMoveLegal = True
              , _randg = mkStdGen 0
              , _varMap = Map.empty
              , _players = map fst hands  --[("Angus",[]),("Toby",[]),("Anne",[])]
              , _hands = Map.fromList hands
              , _prevGS = Nothing
              }


test7s = (r7 baseAct,newTestGame [("Toby",[(Seven,Clubs),(Three,Hearts)]),
                                  ("Angus",map unPlay [cl 3, sp 7, he 9]),
                                  ("Anne",map unPlay [di 6, sp 9, cl 2])
                                  ],
            [toby$ cl 7, angus$ cl 3, anne$ sp 9, angus$ sp 7, Action "Anne" (Draw 4) "thank you vry much", Action "Anne" (Draw 4) "thank you very much",
            toby$ Draw 1, angus$ Draw 1, anne$ Draw 1005 ])

testSequence :: (Game,GameState, [Event]) -> Bool -> IO GameState
testSequence (g,state,(e: es)) sh = do
    let state' = g e state
    if sh then putStr$ prGS state' else return ()
    testSequence (g,state',es) sh
testSequence (_,s,_) _ = return s



testg = (draw 5 "Angus" . draw 5 "rolf" . draw 5 "Toby" $ newGame ["Angus","rolf","Toby"])
