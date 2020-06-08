{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Sanitisation of user input and check that rules behave well.
module Game.Bartok.ServerInterface
  ( handle,
    view,
    addRule,
    addRule',
    addViewRule,
    restartWithNewRule,
    setState,
    setTime,
    getWinner,
    OngoingGame (..),
    MError,
    readError,
    initialGame,
    Rule,
    ViewRule,
    Rule',
    GameState,
    timeoutReq,
  )
where

import Control.Arrow ((&&&))
import Control.Lens (at, contains, ix, makeLenses, (%~), (&), (.~), (<<.~), (^.), (^?), _Just)
import Control.Monad (ap)
import Control.Monad.Except (MonadError, throwError, unless)
import Control.Monad.Trans.State (StateT, put, runStateT)
import Data.Bifunctor (bimap)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map (member)
import Data.Maybe (fromJust)
import qualified Data.Set as Set (empty, fromList)
import Data.Set (Set)
import Data.Time (UTCTime, getCurrentTime)
import Game.Bartok.BaseGame (baseAct, broadcast)
import Game.Bartok.DataTypes
  ( Action (..),
    CardView,
    Event (..),
    GameState,
    Name,
    Rule,
    Rule',
    ViewRule,
    hands,
    handsV,
    messages,
    newGame,
    players,
    winner,
  )
import Game.Bartok.Rules (defaultRulesNamed)
import Game.Bartok.Serialize (ActionReq (..), ClientPacket (..), Token)
import Game.Bartok.Views (GameView, defaultView)
import Utils (applyWhen, (/\))

type Error = String

type MError a = StateT (Maybe OngoingGame) (Either Error) a

data OngoingGame = OG
  { _gameState :: GameState,
    _rules :: [(String, Rule)],
    _viewRules :: [(String, ViewRule)],
    _seats :: [(Name, Token)],
    _lastAction :: UTCTime,
    _counter :: Int,
    _countmsgs :: [[String]]
  }

makeLenses ''OngoingGame

instance Show OngoingGame where
  show og = show (og ^. gameState) ++ '\n' : intercalate ", " (map fst (og ^. rules)) ++ '\n' : intercalate ", " (map fst (og ^. viewRules))

--TODO(angus): make sure rules can't stop players being added

initialGame :: IO OngoingGame
initialGame = do
  t <- getCurrentTime
  pure $ OG (newGame []) (defaultRulesNamed ++ [("base", id)]) [("base", id)] [] t 0 []

readError :: MError a -> Either String (a, Maybe OngoingGame)
readError s = runStateT s Nothing

timeoutReq :: Either ActionReq Event
timeoutReq = Right Timeout

nameCheck :: MonadError String m => Name -> OngoingGame -> m ()
nameCheck p og =
  unless
    (nameExists p og)
    (throwError $ "Player " ++ p ++ " is not a member of this game.")

tokenCheck :: MonadError String m => Name -> Token -> OngoingGame -> m ()
tokenCheck p t og =
  unless
    (tokenMatches p t og)
    (throwError $ "Token given does not match that of player " ++ p ++ ".")

checks :: Name -> Token -> OngoingGame -> MError ()
checks p t og = do
  nameCheck p og
  tokenCheck p t og

handle :: Either ActionReq Event -> OngoingGame -> MError GameState
handle ar og =
  case ar of
    Left (ReqPlay p t _ i m) -> do
      checks p t og
      case og ^? gameState . hands . at p . _Just . ix i of
        Just c -> carryOut (Action p (Play c) m) og
        Nothing -> throwError $ "Player " ++ p ++ " does not have " ++ show (i + 1) ++ " card(s) in hand."
    Left (ReqDraw p t _ n m) -> do
      checks p t og
      carryOut (Action p (Draw n) m) og
    Left (ReqJoin n tok _) ->
      if nameExists n og
        then pure (og ^. gameState) -- (throwError $ "Player "++n++" is already a member of this game.")
        else do
          let neighbs =
                (NE.head &&& NE.last)
                  <$> NE.nonEmpty (map fst $ og ^. seats)
          put $ Just $ og & seats %~ ((n, tok) :)
          carryOut (PlayerJoin n neighbs) og
    Left (ReqLeave n _ _) ->
      if nameExists n og
        then put (Just $ og & seats %~ (filter ((/= n) . fst))) >> carryOut (PlayerLeave n) og
        else pure (og ^. gameState)
    Right Timeout -> carryOut Timeout og
    Right _ -> error "should only send timeouts"

-- Remove rules left to admin
carryOut :: Event -> OngoingGame -> MError GameState
carryOut e og =
  pure . snd $
    foldr
      ( \(n, r) (g, gs) ->
          let gs' = r g e (og ^. gameState)
           in if checkGSokay (og & gameState .~ gs')
                then (r g, gs')
                else (g, broadcast ("Rule " ++ n ++ " malfunctioned and was not applied for this event.") gs)
      )
      (baseAct, og ^. gameState)
      (og ^. rules)

nameExists :: Name -> OngoingGame -> Bool
nameExists n og = n `elem` map fst (og ^. seats)

tokenMatches :: Name -> Token -> OngoingGame -> Bool
tokenMatches n t og = (n, t) `elem` og ^. seats

allUniques :: (Ord a) => [a] -> Bool
allUniques = flip (foldr f $ const True) Set.empty
  where
    f :: (Ord a) => a -> (Set a -> Bool) -> Set a -> Bool
    f a b s =
      let (b', s') = s & contains a <<.~ True
       in not b' && b s'

checkGSokay :: OngoingGame -> Bool
checkGSokay og =
  let gs = og ^. gameState
   in (allUniques $ gs ^. players) -- each player appears only once
  -- && (Set.fromList (gs^.players) == Set.fromList (map fst $ og^.seats)) -- players is permutation of seats
        && all (`Map.member` (gs ^. hands)) (gs ^. players) -- each player has a hand

--only restriction on GameViews is that they can't misrepresent the # of players
checkGVokay :: GameView -> OngoingGame -> Bool
checkGVokay gv og = Set.fromList (map fst $ gv ^. handsV) == Set.fromList (map fst $ og ^. seats)

view :: Name -> Token -> Int -> OngoingGame -> MError ClientPacket
view p t count og = do
  checks p t og
  let delta = (og ^. counter) - count
  if delta <= 0
    then pure NoNewData
    else case og ^. gameState . winner of
      Nothing -> do
        v <- getView p (og & gameState . messages .~ (concat . take delta $ og ^. countmsgs))
        let inOrder :: [(Name, [CardView])] -> [(Name, [CardView])]
            inOrder hs = foldr ((:) . ap (,) (fromJust . flip lookup hs) . fst) [] (og ^. seats) -- put hands in seat order
            newHands :: [(Name, [CardView])] -> [(Name, [CardView])]
            newHands = uncurry (flip (++)) . span ((p /=) . fst) -- cycle so that p is at the front
        pure $ NewData (og ^. counter) (v & handsV %~ newHands . inOrder)
      Just q ->
        pure $ Redirect $
          if q == p
            then "newRule/?name=" <> p
            else "wait/?name=" <> p

-- can still run into problems if views do weird things to the messages!
-- because the "malfunction" messages are passed in at the end and
-- we don't check whether these cause further malfunctions in the other viewRules
getView :: Name -> OngoingGame -> MError GameView
getView p og =
  let (v, gs) =
        foldr
          ( \(n, vr) (v', gs') ->
              let gv = vr v' p (og ^. gameState)
               in (v', gs')
                    & applyWhen
                      (checkGVokay gv og)
                      (bimap vr . broadcast $ "ViewRule " ++ n ++ " malfunctioned for player " ++ p ++ " and was not applied for them.")
          )
          (defaultView, og ^. gameState) -- change this so gs
          (og ^. viewRules)
   in pure $ v p gs

-- Don't modify state in order to preserve the correctness of the counter

setState :: GameState -> OngoingGame -> OngoingGame
setState s =
  (gameState .~ (s & messages .~ [])) . (counter %~ (+ 1))
    . (countmsgs %~ ((s ^. messages) :))

setTime :: UTCTime -> OngoingGame -> OngoingGame
setTime t = lastAction .~ t

getWinner :: OngoingGame -> Maybe Name
getWinner = (^. gameState . winner)

addRule' :: String -> Rule' -> OngoingGame -> OngoingGame
addRule' n (r, vr) = (rules /\ viewRules) %~ bimap ((n, r) :) ((n, vr) :)

addRule :: String -> Rule -> OngoingGame -> OngoingGame
addRule n r = rules %~ ((n, r) :)

restartWithNewRule :: String -> Rule' -> OngoingGame -> OngoingGame
restartWithNewRule n r =
  addRule' n r
    . (gameState .~ newGame [])
    . (seats .~ [])
    . (counter .~ 0)

-- if more stuff is added to OngoingGame,
-- think about resetting it here

addViewRule :: String -> ViewRule -> OngoingGame -> OngoingGame
addViewRule n vr = viewRules %~ ((n, vr) :)
