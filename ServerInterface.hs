{-# LANGUAGE TemplateHaskell #-}
-- | Sanitisation of user input and check that rules behave well.
module ServerInterface(
   handle, view,
   addRule, addRule', addViewRule, setState,
   OngoingGame, MError, readError, initialGame,
   Rule, GameState, getName)
 where

import DataTypes
import Lib hiding (when)
import Views
import Control.Lens hiding(view)
import Control.Monad (when)
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.Trans.State

import Control.Arrow((***))
import Data.List (nub,permutations)
import qualified Data.Map as Map (member)

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
handle :: Event -> OngoingGame -> MError GameState
handle e og =
    case e of
        Action p a s ->do
            unless (nameExists p og)
                    (throwError $ "Player "++p++" is not a member of this game.")
            return $ foldr ($) baseAct (og^.rules) e (og ^. gameState)
        Timeout -> return $ foldr ($) baseAct (og^.rules) e (og ^. gameState)
        PlayerJoin n -> if (nameExists n og) then return$ _gameState og
                            else return $ foldr ($) baseAct (og^.rules) e (og ^. gameState)


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

setState :: GameState -> OngoingGame -> OngoingGame
setState s = gameState .~ s

addRule' :: Rule' -> OngoingGame -> OngoingGame
addRule' (r,vr) = (rules /\ viewRules) %~ ((r:) *** (vr:))

addRule :: Rule -> OngoingGame -> OngoingGame
addRule = (rules %~) . (:)

addViewRule :: ViewRule -> OngoingGame -> OngoingGame
addViewRule = (viewRules %~) . (:)
