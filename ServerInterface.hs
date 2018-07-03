{-# LANGUAGE TemplateHaskell #-}

module ServerInterface where

import DataTypes
import Lib hiding (when)
import Views
import Control.Lens
import Control.Monad (when)
import Control.Monad.Except
import Control.Arrow((***))
import Data.List (nub,permutations)
import qualified Data.Map as Map (member)

type Error = String
type MError a = Either Error a

data OngoingGame = OG {
    _gameState :: GameState ,
    _rules :: [Rule] ,
    _viewRules :: [ViewRule]
}
makeLenses ''OngoingGame

-- data Event = Action PlayerIndex Action String | Timeout | PlayerJoin Name deriving (Show,Eq)
handle :: Event -> OngoingGame -> MError OngoingGame
handle e og = do
    case e of
        Action p a s -> unless (nameExists p og)
                            (throwError $ "Player "++p++" is not a member of this game.")
        Timeout -> return ()
        PlayerJoin n -> when (nameExists n og)
                            (throwError $ "Player "++n++" is already taken, please choose a different name.")
    return $ og & gameState %~ foldr ($) baseAct (og^.rules) e

nameExists :: Name -> OngoingGame -> Bool
nameExists n og = n `elem` og^.gameState.seats

hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates l = l == nub l

checkGSokay :: GameState -> Bool
checkGSokay gs = (not$ hasDuplicates (gs^.players)) -- each player appears only once
              && ((gs^.players) `elem` (permutations (gs^.seats))) -- players is permutation of seats
              && all (flip Map.member (gs^.hands)) (gs^.players)

view :: PlayerIndex -> OngoingGame -> MError GameView
view p og = do
  unless (nameExists p og)
      (throwError $ "Player "++p++" is not a member of this game.")
  return $ foldr ($) defaultView (og^.viewRules) p (og^.gameState)

addRule' :: Rule' -> OngoingGame -> OngoingGame
addRule' (r,vr) = (rules /\ viewRules) %~ ((r:) *** (vr:))

addRule :: Rule -> OngoingGame -> OngoingGame
addRule = (rules %~) . (:)

addViewRule :: ViewRule -> OngoingGame -> OngoingGame
addViewRule = (viewRules %~) . (:)
