{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
-- | Sanitisation of user input and check that rules behave well.
module ServerInterface(
   handle, view,
   addRule, addRule', addViewRule, setState,
   OngoingGame(..), MError, readError, initialGame,
   Rule, GameState, timeoutReq)
 where


import Control.Lens ((^?),(^.),(.~),(%~), (&),_Just,at,contains,ix,makeLenses)
import Control.Monad (ap,when)
import Control.Monad.Except (throwError,unless)
import Control.Monad.Trans.State (StateT,gets,put,runStateT,modify)

import Control.Arrow((***))
import Data.List (intercalate,nub,permutations)
import qualified Data.Map as Map (keysSet,member)
import Data.Maybe (isJust)
import qualified Data.Set as Set (empty,fromList,insert,notMember)
import Data.Set (Set)

import DataTypes hiding (seats)
import BaseGame hiding (when)
import Views
import Serialize
import Rules

type Error = String
--type MError a = Either Error a
type MError a = StateT (Maybe OngoingGame) (Either Error) a


data OngoingGame = OG {
    _gameState :: GameState ,
    _rules :: [(String,Rule)] ,
    _viewRules :: [(String,ViewRule)],
    _seats :: [(Name,Token)]
    --TODO(toby) figure out datetimes _lastEvent :: DateTime
}
makeLenses ''OngoingGame

instance Show OngoingGame where
  show og = show (og ^. gameState) ++ '\n': intercalate ", " (map fst (og^.rules)) ++ '\n': intercalate ", " (map fst (og^.viewRules))

--TODO(angus): make sure rules can't stop players being added

initialGame :: IO OngoingGame
initialGame = return$ OG (newGame []) (defaultRulesNamed++[("base",id)]) [("base",id)] []
-- initialGame = return$ addRule' "Snap" gSnap (OG (newGame []) [] [])

readError :: MError a -> Either String (a, Maybe OngoingGame)
readError s = runStateT s Nothing

-- data Event = Action PlayerIndex Action String | Timeout | PlayerJoin Name deriving (Show,Eq)

timeoutReq :: Either ActionReq Event
timeoutReq = Right Timeout

handle :: Either ActionReq Event -> OngoingGame -> MError GameState
handle ar og =
   let nameCheck p = unless (nameExists p og)
          (throwError $ "Player "++p++" is not a member of this game.")
       tokenCheck p t = unless (tokenMatches p t og)
          (throwError $ "Token given does not match that of player "++p++".")
       checks p t = nameCheck p >> tokenCheck p t in
   case ar of
      Left (ReqPlay p t i m) -> do
        checks p t
        case og^?gameState.hands.at p._Just.ix i of
            Just c -> carryOut (Action p (Play c) m) og
            Nothing -> throwError $ "Player "++p++" does not have "++show (i+1)++" card(s) in hand."
      Left (ReqDraw p t n m) -> do
        checks p t
        carryOut (Action p (Draw n) m) og
      Left (ReqJoin n tok) -> if nameExists n og then return (og^.gameState) -- (throwError $ "Player "++n++" is already a member of this game.")
                       else let neighbs = case map fst (og^.seats) of l@(x:xs) -> Just (x,last l); _ -> Nothing in
                            put (Just $ og & seats %~ ((n,tok):)) >> carryOut (PlayerJoin n neighbs) og
      Right Timeout -> carryOut Timeout og
      Right _ -> error "shold only send timeouts"
  -- return $ foldr ($) baseAct (og^.rules) e (og^.gameState)

-- Remove rules left to admin
carryOut :: Event -> OngoingGame -> MError GameState
carryOut e og = return . snd $
                    foldr (\(n,r) (g,gs) ->
                              let gs' = r g e (og^.gameState)
                              in if checkGSokay (og & gameState .~gs')
                                then (r g,gs')
                                else (g,broadcast ("Rule "++n++" malfunctioned and was not applied for this event.") gs))
                          (baseAct, og^.gameState)
                          (og^.rules)

timeout :: OngoingGame -> MError GameState
timeout = carryOut Timeout

nameExists :: Name -> OngoingGame -> Bool
nameExists n og = n `elem` map fst (og^.seats)

tokenMatches :: Name -> Token -> OngoingGame -> Bool
tokenMatches n t og = (n,t) `elem` og^.seats

allUniques :: (Ord a) => [a] -> Bool
allUniques l = allUniques' l Set.empty
allUniques' :: (Ord a) => [a] -> Set a -> Bool
allUniques' [] s = True
allUniques' (x:xs) s = x `Set.notMember` s && allUniques' xs (Set.insert x s)

checkGSokay :: OngoingGame -> Bool
checkGSokay og =let gs = og ^. gameState in
              (allUniques (gs^.players)) -- each player appears only once
              -- && (Set.fromList (gs^.players) == Set.fromList (map fst $ og^.seats)) -- players is permutation of seats
              && all (`Map.member` (gs^.hands)) (gs^.players) -- each player has a hand

--only restriction on GameViews is that they can't misrepresent the # of players
checkGVokay :: GameView -> OngoingGame -> Bool
checkGVokay gv og = Map.keysSet (gv^.handsV) == Set.fromList (map fst $ og^.seats)

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
                                      in if checkGVokay gv (og & gameState .~ gs')
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
