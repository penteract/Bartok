{-# LANGUAGE TemplateHaskell #-}
-- | Sanitisation of user input and check that rules behave well.
module ServerInterface(
   handle, view,
   addRule, addRule', addViewRule, setState,
   OngoingGame, MError, readError, initialGame,
   Rule, GameState, getName)
 where


import Control.Lens ((^?),(^.),(.~),(%~),_Just,at,contains,ix,makeLenses)
import Control.Monad (ap,when)
import Control.Monad.Except (throwError,unless)
import Control.Monad.Trans.State (StateT,gets,put,runStateT)

import Control.Arrow((***))
import Data.List (intercalate,nub,permutations)
import qualified Data.Map as Map (member)
import Data.Maybe (isJust)
import qualified Data.Set as Set (empty,fromList,insert,notMember)
import Data.Set (Set)

import DataTypes
import Lib hiding (when)
import Views
import Serialize

type Error = String
--type MError a = Either Error a
type MError a = StateT (Maybe OngoingGame) (Either Error) a

data OngoingGame = OG {
    _gameState :: GameState ,
    _rules :: [(String,Rule)] ,
    _viewRules :: [(String,ViewRule)]
}
makeLenses ''OngoingGame

instance Show OngoingGame where
  show og = show (og ^. gameState) ++ '\n': intercalate ", " (map fst (og^.rules)) ++ '\n': intercalate ", " (map fst (og^.viewRules))

--TODO(angus): add comments

initialGame :: IO OngoingGame
initialGame = return$ OG (newGame []) [("base",id)] [("base",id)]

readError :: MError a -> Either String (a, Maybe OngoingGame)
readError s = runStateT s Nothing

-- data Event = Action PlayerIndex Action String | Timeout | PlayerJoin Name deriving (Show,Eq)
handle :: ActionReq -> OngoingGame -> MError GameState
handle ar og =
   case ar of
      ReqPlay p i m -> do
        unless (nameExists p og)
            (throwError $ "Player "++p++" is not a member of this game.")
        case og^?gameState.hands.at p._Just.ix i of
            Just c -> carryOut (Action p (Play c) m) og
            Nothing -> throwError $ "Player "++p++" does not have "++show (i+1)++" card(s) in hand."
      ReqDraw p n m -> do
        unless (nameExists p og)
                (throwError $ "Player "++p++" is not a member of this game.")
        carryOut (Action p (Draw n) m) og
      ReqJoin n -> if nameExists n og then return (og^.gameState) -- (throwError $ "Player "++n++" is already a member of this game.")
                       else carryOut (PlayerJoin n) og
  -- return $ foldr ($) baseAct (og^.rules) e (og^.gameState)

-- Remove rules left to admin
carryOut :: Event -> OngoingGame -> MError GameState
carryOut e og = return . snd $
                    foldr (\(n,r) (g,gs) ->
                              let gs' = r g e (og^.gameState)
                              in if checkGSokay gs'
                                then (r g,gs')
                                else (g,broadcast ("Rule "++n++" malfunctioned and was not applied for this event.") gs))
                          (baseAct, og^.gameState)
                          (og^.rules)

timeout :: OngoingGame -> MError GameState
timeout = carryOut Timeout

nameExists :: Name -> OngoingGame -> Bool
nameExists n og = n `elem` og^.gameState.seats

allUniques :: (Ord a) => [a] -> Bool
allUniques l = allUniques' l Set.empty
allUniques' :: (Ord a) => [a] -> Set a -> Bool
allUniques' [] s = True
allUniques' (x:xs) s = x `Set.notMember` s && allUniques' xs (Set.insert x s)

checkGSokay :: GameState -> Bool
checkGSokay gs = (allUniques (gs^.players)) -- each player appears only once
              && (Set.fromList (gs^.players) == Set.fromList (gs^.seats)) -- players is permutation of seats
              && all (`Map.member` (gs^.hands)) (gs^.players) -- each player has a hand

--only restriction on GameViews is that they can't misrepresent the # of players
checkGVokay :: GameView -> GameState -> Bool
checkGVokay gv gs = length (gv^.handsV) == length (gs^.seats)

view :: PlayerIndex -> OngoingGame -> MError GameView
view p og = do
  unless (nameExists p og)
      (throwError $ "Player "++p++" is not a member of this game.")
  getView p og

-- can still run into problems if views do weird things to the messages!
-- because the "malfunction" messages are passed in at the end and
-- we don't check whether these cause further malfunctions in the other viewRules
getView :: PlayerIndex -> OngoingGame -> MError GameView
getView p og = let (v,gs') = foldr (\(n,vr) (v,gs') ->
                                      let gv = vr v p (og^.gameState)
                                      in if checkGVokay gv gs'
                                           then (vr v,gs')
                                           else (v,broadcast ("ViewRule "++n++" malfunctioned for player "++p++" and was not applied for them.") gs'))
                                   (defaultView,og^.gameState) -- change this so gs
                                   (og^.viewRules) in
                put (Just$ (gameState .~ gs') og) >> return (v p gs')

setState :: GameState -> OngoingGame -> OngoingGame
setState s = gameState .~ s

addRule' :: String -> Rule' -> OngoingGame -> OngoingGame
addRule' n (r,vr) = (rules /\ viewRules) %~ (((n,r):) *** ((n,vr):))

addRule :: String -> Rule -> OngoingGame -> OngoingGame
addRule n r = rules %~ ((n,r):)

addViewRule :: String -> ViewRule -> OngoingGame -> OngoingGame
addViewRule n vr = viewRules %~ ((n,vr):)
