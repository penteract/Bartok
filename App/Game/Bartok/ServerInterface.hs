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

import Control.Arrow ((***))
import Control.Lens (at, contains, ix, makeLenses, (%~), (&), (.~), (^.), (^?), _Just)
import Control.Monad (ap, when)
import Control.Monad.Except (throwError, unless)
import Control.Monad.Trans.State (StateT, gets, modify, put, runStateT)
import Data.List (intercalate, nub, permutations)
import qualified Data.Map as Map (keysSet, lookup, member)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set (empty, fromList, insert, notMember)
import Data.Set (Set)
import Data.Time
import Game.Bartok.BaseGame hiding (when)
import Game.Bartok.DataTypes hiding (seats)
import Game.Bartok.Rules
import Game.Bartok.Sample
import Game.Bartok.Serialize
import Game.Bartok.Views

type Error = String

--type MError a = Either Error a
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
  return $ OG (newGame []) (defaultRulesNamed ++ [("base", id)]) [("base", id)] [] t 0 []

-- initialGame = return$ addRule' "Snap" gSnap (OG (newGame []) [] [])

readError :: MError a -> Either String (a, Maybe OngoingGame)
readError s = runStateT s Nothing

-- data Event = Action Name Action String | Timeout | PlayerJoin Name deriving (Show,Eq)

timeoutReq :: Either ActionReq Event
timeoutReq = Right Timeout

nameCheck p og =
  unless
    (nameExists p og)
    (throwError $ "Player " ++ p ++ " is not a member of this game.")

tokenCheck p t og =
  unless
    (tokenMatches p t og)
    (throwError $ "Token given does not match that of player " ++ p ++ ".")

checks :: Name -> Token -> OngoingGame -> MError ()
checks p t og = nameCheck p og >> tokenCheck p t og

handle :: Either ActionReq Event -> OngoingGame -> MError GameState
handle ar og =
  case ar of
    Left (ReqPlay p t c i m) -> do
      checks p t og
      case og ^? gameState . hands . at p . _Just . ix i of
        Just c -> carryOut (Action p (Play c) m) og
        Nothing -> throwError $ "Player " ++ p ++ " does not have " ++ show (i + 1) ++ " card(s) in hand."
    Left (ReqDraw p t c n m) -> do
      checks p t og
      carryOut (Action p (Draw n) m) og
    Left (ReqJoin n tok c) ->
      if nameExists n og
        then return (og ^. gameState) -- (throwError $ "Player "++n++" is already a member of this game.")
        else
          let neighbs = case map fst (og ^. seats) of l@(x : xs) -> Just (x, last l); _ -> Nothing
           in put (Just $ og & seats %~ ((n, tok) :)) >> carryOut (PlayerJoin n neighbs) og
    Left (ReqLeave n tok c) ->
      if nameExists n og
        then put (Just $ og & seats %~ (filter ((/= n) . fst))) >> carryOut (PlayerLeave n) og
        else return (og ^. gameState)
    Right Timeout -> carryOut Timeout og
    Right _ -> error "should only send timeouts"

-- return $ foldr ($) baseAct (og^.rules) e (og^.gameState)

-- Remove rules left to admin
carryOut :: Event -> OngoingGame -> MError GameState
carryOut e og =
  return . snd $
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
allUniques l = allUniques' l Set.empty

allUniques' :: (Ord a) => [a] -> Set a -> Bool
allUniques' [] s = True
allUniques' (x : xs) s = x `Set.notMember` s && allUniques' xs (Set.insert x s)

checkGSokay :: OngoingGame -> Bool
checkGSokay og =
  let gs = og ^. gameState
   in (allUniques (gs ^. players)) -- each player appears only once
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
    then return NoNewData
    else case og ^. gameState . winner of
      Nothing -> do
        v <- getView p (og & gameState . messages .~ (concat . take delta $ og ^. countmsgs))
        let inOrder hs = foldr ((:) . ap (,) (fromJust . flip lookup hs) . fst) [] (og ^. seats) -- put hands in seat order
        let newHands = uncurry (flip (++)) . span ((p /=) . fst) -- cycle so that p is at the front
        return $ NewData (og ^. counter) (v & handsV %~ newHands . inOrder)
      Just q ->
        if q == p
          then return $ Redirect ("newRule/?name=" ++ p)
          else return $ Redirect ("wait/?name=" ++ p)

-- can still run into problems if views do weird things to the messages!
-- because the "malfunction" messages are passed in at the end and
-- we don't check whether these cause further malfunctions in the other viewRules
getView :: Name -> OngoingGame -> MError GameView
getView p og =
  let (v, gs') =
        foldr
          ( \(n, vr) (v, gs') ->
              let gv = vr v p (og ^. gameState)
               in if checkGVokay gv og
                    then (vr v, gs')
                    else (v, broadcast ("ViewRule " ++ n ++ " malfunctioned for player " ++ p ++ " and was not applied for them.") gs')
          )
          (defaultView, og ^. gameState) -- change this so gs
          (og ^. viewRules)
   in return (v p gs')

-- put (Just$ (gameState .~ gs') og) >> return (v p gs')
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
addRule' n (r, vr) = (rules /\ viewRules) %~ (((n, r) :) *** ((n, vr) :))

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
