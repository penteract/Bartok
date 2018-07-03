{-# LANGUAGE TemplateHaskell #-}
-- | Sanitisation of user input and check that rules behave well.
module ServerInterface(
   handle, view,
   addRule, addRule', addViewRule, setState,
   OngoingGame, MError, readError, initialGame,
   Rule, GameState, getName)
 where


import Control.Lens ((^?),(^.),(.~),(%~),_Just,at,contains,ix,makeLenses)
import Control.Monad (when)
import Control.Monad.Except (throwError,unless)
import Control.Monad.Trans.State (StateT,runStateT)

import Control.Arrow((***))
import Data.List (nub,permutations)
import qualified Data.Map as Map (member)
import Data.Maybe (isJust)
import qualified Data.Set as Set (empty,fromList,insert,member)
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
    _rules :: [Rule] ,
    _viewRules :: [ViewRule]
}
makeLenses ''OngoingGame

--TODO(angus): perform ongoing checks
--TODO(angus): add comments

initialGame :: IO OngoingGame
initialGame = return$ OG (newGame ["Toby"]) [] []

readError :: MError a -> Either String (a, Maybe OngoingGame)
readError s = runStateT s Nothing

-- data Event = Action PlayerIndex Action String | Timeout | PlayerJoin Name deriving (Show,Eq)
handle :: ActionReq -> OngoingGame -> MError GameState
handle ar og = do
  e <- case ar of
        ReqPlay p i m -> do
          unless (nameExists p og)
              (throwError $ "Player "++p++" is not a member of this game.")
          case og^?gameState.hands.at p._Just.ix i of
              Just c -> return $ Action p (Play c) m
              Nothing -> throwError $ "Player "++p++" does not have "++show (i+1)++" card(s) in hand."
        ReqDraw p n m -> do
          unless (nameExists p og)
                  (throwError $ "Player "++p++" is not a member of this game.")
          return $ Action p (Draw n) m
        ReqJoin n -> if nameExists n og then (throwError $ "Player "++n++" is already a member of this game.")
                         else return $ PlayerJoin n -- TODO: (Toby) can change this again if you like
  carryOut e og
  -- return $ foldr ($) baseAct (og^.rules) e (og^.gameState)

-- TODO: doesn't yet chuck out the bad rule
carryOut :: Event -> OngoingGame -> MError GameState
carryOut e og = return . snd $
                    foldr (\r (g,gs) ->
                              let gs' = r g e gs
                              in if checkGSokay gs' then (r g,gs') else (g,gs))
                          (baseAct,og^.gameState)
                          (og^.rules)

timeout :: OngoingGame -> MError GameState
timeout og = return$ foldr ($) baseAct (og^.rules) Timeout (og ^. gameState)

nameExists :: Name -> OngoingGame -> Bool
nameExists n og = n `elem` og^.gameState.seats

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates l = hasDuplicates' l Set.empty
hasDuplicates' :: (Ord a) => [a] -> Set a -> Bool
hasDuplicates' [] s = False
hasDuplicates' (x:xs) s = x `Set.member` s || hasDuplicates' xs (Set.insert x s)

checkGSokay :: GameState -> Bool
checkGSokay gs = (not$ hasDuplicates (gs^.players)) -- each player appears only once
              && (Set.fromList (gs^.players) == Set.fromList (gs^.seats)) -- players is permutation of seats
              && all (flip Map.member (gs^.hands)) (gs^.players) -- each player has a hand

view :: PlayerIndex -> OngoingGame -> MError GameView
view p og = do
  unless (nameExists p og)
      (throwError $ "Player "++p++" is not a member of this game.")
  return $ foldr ($) defaultView (og^.viewRules) p (og^.gameState)

setState :: GameState -> OngoingGame -> OngoingGame
setState s = gameState .~ s

addRule' :: Rule' -> OngoingGame -> OngoingGame
addRule' (r,vr) = (rules /\ viewRules) %~ ((r:) *** (vr:))

addRule :: Rule -> OngoingGame -> OngoingGame
addRule = (rules %~) . (:)

addViewRule :: ViewRule -> OngoingGame -> OngoingGame
addViewRule = (viewRules %~) . (:)
