{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : TLib
-- Description : An alternative to RuleHelpers; uses more higher order functions and fewer lenses
--
-- This presents operations with more arguments than strictly necessary so that they can be used interchangeably
module Game.Bartok.TLib
  ( -- * The fundamental type
    GEGSto,

    -- * class
    Ruleable (..),

    -- * users
    -- These functions give access to the arguments in a composable manner
    withAction,
    withMessage,
    withCard,
    withPlayer,
    withHand,
    with,

    -- * Operators
    when,
    whether,
    onNextTurn,
    uponDoUntil,
    (.:),
    __,

    -- * Tests
    isAction,
    isLegal,
    isTurn,
    actionIs,
    cardIs,
    boolVar,
    said,
    (~&~),
    not_,

    -- * getters
    getVar,
    getTop,
    state,

    -- * Helper Functions
    --  These should help with common patterns
    mkFailMsg,
    mustSay,
    unnec,
    sometimesSay,
    mustDo,
    mustSayPenalty,
    sometimesSayPenalty,
    unnecPenalty,
    modifyPlayers,
    modifyMessage,
    penalty,

    -- * From DataTypes:
    Step,
    Game,
    Rule,
    GameState,
    Event (..),
    Action (..),
    Name,

    -- ** cards
    Card,
    CardView (..),
    GameView (..),
    suit,
    rank,
    Suit,
    Rank,
    Viewer,
    ViewRule,
    readVar,
    setVar,
    modifyVar,
    nextTurn,
    draw,
    broadcast,
    (%),
    mapAllCards,
  )
where

import Control.Applicative (liftA2)
import Control.Lens (none)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Game.Bartok.BaseGame (broadcast, draw, getHand, illegal, nextTurn, (%))
import Game.Bartok.DataTypes
  ( Action (..),
    Card,
    CardView (..),
    Event (..),
    Game,
    GameState,
    GameView (..),
    Name,
    Rank,
    Rule,
    Step,
    Suit,
    VarName,
    ViewRule,
    Viewer,
    modifyVar,
    rank,
    readVar,
    setVar,
    suit,
    _deck,
    _hands,
    _lastMoveLegal,
    _messages,
    _pile,
    _players,
  )
import Game.Bartok.RuleHelpers (findInMs, reconstitute, regexProcess, split)
import Text.Regex (matchRegex, matchRegexAll, mkRegexWithOpts)

--  Almost a Monad.

-- | Avoids having to write out extremely long types.
--
--   Note that @GEGSto GameState@ is 'Rule'.
type GEGSto a =
  -- | The inner ruleset
  Game ->
  -- | The event under consideration
  Event ->
  -- | The previous state of the game
  GameState ->
  a

-- | A class for types that are 'smaller' than 'Rule' so that there are different options for how to convert.
class Ruleable a where
  -- | Perform an action after the inner ruleset.
  doAfter :: a -> Rule

  -- | Perform an action before the inner ruleset.
  doBefore :: a -> Rule

  -- | Perform an action and ignore the inner ruleset.
  --   This should be used rarely.
  doOnly :: a -> Rule
-- ^ Be careful when composing these.
--  @doBefore a . doBefore b == doBefore (b . a)@
--  @doAfter a . doBefore b == doBefore (a . b)@
--
--  Check the source code of TSample to see how to use these.
--
--  Also be careful when both reading and writing variables. See 'TSample.rAce'
--  for an example of how to set and unset variables to get a oneshot effect
--  without missing

instance Ruleable Step where
  doAfter s act e gs = s $ act e gs
  doBefore s act e gs = act e $ s gs
  doOnly s _ _ gs = s gs

instance Ruleable Game where
  doAfter act1 act2 e = act1 e . act2 e
  doBefore act1 act2 e = act2 e . act1 e
  doOnly = const

-- | Returns True if the event is an Action.
isAction :: GEGSto Bool
isAction _ (Action _ _ _) _ = True
isAction _ _ _ = False

-- | Returns True if the event is a Play action and the card satisfies the condition.
cardIs ::
  -- | The condition
  (Card -> Bool) ->
  GEGSto Bool
cardIs f _ (Action _ (Play c) _) _ = f c
cardIs _ _ _ _ = False

-- | Returns True if the event is an action and is legal under the inner ruleset.
isLegal :: GEGSto Bool
isLegal act e gs = isAction act e gs && (_lastMoveLegal (act e gs))

-- | Returns True if the event is an action which satisfies the condition.
actionIs :: (Action -> Bool) -> GEGSto Bool
actionIs q _ (Action _ a _) _ = q a
actionIs _ _ _ _ = False

-- | Returns True if the event is an action and the next Turn should be taken by the player submitting it.
isTurn :: GEGSto Bool
isTurn _ (Action p _ _) gs = p == head (_players gs)
isTurn _ _ _ = False

-- | Returns True if the value of the named variable is not 0.
boolVar :: VarName -> GEGSto Bool
boolVar v _ _ gs = readVar v gs /= 0

-- | Combines 2 tests. Returns True if both do.
(~&~) :: GEGSto Bool -> GEGSto Bool -> GEGSto Bool
(~&~) = liftA2 $ liftA2 $ liftA2 (&&)

-- | Negates a test. Be aware that @(not_ (actionIs (==(Draw 2))))@ is different from  @(actionIs (/=(Draw 2)))@.
--   The first example would return True for events that are not actions.
not_ :: GEGSto Bool -> GEGSto Bool
not_ = fmap $ fmap $ fmap not

-- | Given a condition, implement a rule only when that condition holds.
when :: (GEGSto Bool) -> Rule -> Rule
when f r act e gs = if f act e gs then r act e gs else act e gs

-- when f r = whether f r (\ _ _ ->id)

-- This could be given the more general type @(GEGSto Bool) -> GEGSto a -> GEGSto a -> GEGSto a@

-- | Given a condition, do one thing when the condition holds and another when it doesn't.
whether ::
  -- | @if@
  (GEGSto Bool) ->
  -- | @then@
  Rule ->
  -- | @else@
  Rule ->
  Rule
whether f r1 r2 act e gs = if f act e gs then r1 act e gs else r2 act e gs

-- whether = liftA2$liftA2$liftA2$ if'

-- | Given a fresh variable name, when a condition is met, run an action on the following turn.
onNextTurn ::
  -- | a fresh varName
  VarName ->
  -- | the condition which must be met
  (GEGSto Bool) ->
  -- | what to do on the next turn
  Rule ->
  Rule
onNextTurn v f r =
  when f (doAfter $ setVar v 1)
    . when (isTurn ~&~ boolVar v) ((doAfter (setVar v 0)) . r)

-- | Upon a condition being satisfied, implement a rule until a second condition is satisfied.
uponDoUntil :: VarName -> GEGSto Bool -> Rule -> GEGSto Bool -> Rule
uponDoUntil v upon r untl =
  when upon (doAfter $ setVar v 1)
    . when untl (doAfter $ setVar v 0)
    . when (boolVar v) r

-- testPlayer :: (String -> Bool) -> GEGSto Bool
-- testPlayer f act e@(Action p _ _) gs = f p
-- testPlayer _ = false
--
--
-- withPlayer :: GEGSto (Maybe String)

-- | When an action happens, do something with it.
withAction :: (Action -> Rule) -> Rule
withAction f act e@(Action _ a _) gs = f a act e gs
withAction _ act e gs = act e gs

-- | When an card is played, do something with it.
withCard :: (Card -> Rule) -> Rule
withCard f act e@(Action _ (Play c) _) gs = f c act e gs
withCard _ act e gs = act e gs

-- | When an action happens, do something with the message that gets sent.
withMessage :: (String -> Rule) -> Rule
withMessage f act e@(Action _ _ m) gs = f m act e gs
withMessage _ act e gs = act e gs

-- | When an action happens, do something with the player that made it.
withPlayer :: (Name -> Rule) -> Rule
withPlayer f act e@(Action p _ _) gs = f p act e gs
withPlayer _ act e gs = act e gs

-- | When an action hapens, do something with the hand of the player that made it.
withHand :: ([Card] -> Rule) -> Rule
withHand f = withPlayer $ \p -> with state $ \gs -> f (maybe [] id $ getHand p gs)

-- | Build a rule (or similar that depends on an extracted value (if you know monads, this is bind @(>>=)@).
-- This differs from 'withAction' etc in that it is not conditional on the type of event
--
--  e.g. @with ('getVar' "n") (\ n -> doSomethingInvolvingN)@
--
--  e.g.
--
-- > with ('getVar' "n") (\ n -> doSomethingInvolvingN)
with ::
  -- | The extractor
  GEGSto a ->
  -- | The thing to do with the value
  (a -> GEGSto b) ->
  GEGSto b
with getter f act e gs = f (getter act e gs) act e gs

-- | Retrive a variable from the GameState.
getVar :: String -> GEGSto Int
getVar s _ _ gs = readVar s gs

-- | Look at the top card of the pile
getTop :: GEGSto Card
getTop _ _ gs = NE.head $ _pile gs

-- | Modify all cards in the game
mapAllCards :: (Card -> Card) -> GameState -> GameState
mapAllCards f gs =
  let (p :| ps) = _pile gs
      hss = _hands gs
      ds = _deck gs
   in gs {_pile = f p :| map f ps, _hands = Map.map (map f) hss, _deck = map f ds}

-- | Award a penalty to the acting player without preventing their action.
penalty :: Int -> String -> Game
penalty n reason (Action p _ _) = draw n p . broadcast ("{} receives penalty {}: {}" % p % show n % reason)
penalty _ _ _ = id -- this case probably shouldn't happen, so could add debug message

-- | Any action except the specified one is an illegal move.
--   When the specified action occurs, run the second argument.
mustDo :: Action -> Rule -> Rule
mustDo act whenDone =
  when isAction $
    whether
      (actionIs (== act))
      whenDone
      (doOnly $ illegal 1 ("failure to {}" % show act))

-- | Transform the list of players (affects turn order).
modifyPlayers :: ([Name] -> [Name]) -> Step
modifyPlayers f g = g {_players = f (_players g)}

-- | Transform the message that gets sent
modifyMessage :: (String -> String) -> Rule
modifyMessage f act (Action p a m) gs = act (Action p a $ f m) gs
modifyMessage _ act e gs = act e gs

-- (monadic 'return')

-- | Abbreviation for @\\ _ _ _ ->@
__ :: a -> GEGSto a
__ = const . const . const

-- | Extract the state argument.
state :: GEGSto GameState
state _ _ gs = gs

-- | lift an action (monadic 'fmap')
(.:) :: (a -> b) -> GEGSto a -> GEGSto b
(.:) f g a b c = f (g a b c)

-- | Tests if a player said something matching a given regex (case insenstive).
--  Like most string processing functions in this project, this separates input messages based on semicolons.
said :: String -> GEGSto Bool
said s _ (Action _ _ m) _ = s `findInMs` m
said _ _ _ _ = False

-- | The penalty message produced by @mustSay s@
mkFailMsg :: String -> String
mkFailMsg s = "failure to say '{}'" % s

-- | penalize if 's' is not said
mustSay :: String -> Rule
mustSay s = mustSayPenalty s (mkFailMsg s)

-- | Like 'mustSay', but with custom penalty message
mustSayPenalty :: String -> String -> Rule
mustSayPenalty s failMsg = when (not_ $ said s) (doBefore (penalty 1 failMsg))

-- | If the condition holds, require that they say  the message; otherwise, penalize them if they say it
sometimesSay :: String -> GEGSto Bool -> Rule
sometimesSay s cond = sometimesSayPenalty s cond (mkFailMsg s) ("unnecessarily saying '{}'" % s)

sometimesSayPenalty :: String -> GEGSto Bool -> String -> String -> Rule
sometimesSayPenalty s cond failMsg unnecMsg =
  whether
    cond
    (mustSayPenalty s failMsg)
    (when (said s) (doBefore $ penalty 1 unnecMsg))

--sometimesSay s cond = unnec s . when cond (mustSay s)

checkMatch :: String -> String -> Bool
checkMatch pattern = isJust . matchRegex r
  where
    r = mkRegexWithOpts pattern True False

-- | Apply a function to 'rest' and 'x' for each possible 'x' in a list where rest is the list with 'x' removed
withoutEach :: ([a] -> a -> b) -> [a] -> [b]
withoutEach f = \case
  [] -> []
  (x : xs) -> map (uncurry f) $ go [] x xs
  where
    go :: [a] -> a -> [a] -> [([a], a)]
    go ys y = \case
      [] -> [(reverse ys, y)]
      (z : zs) -> (reverse ys ++ zs, y) : go (y : ys) z zs

-- | Penalize for unnecessarily saying something.
--   This works by testing if the absence of any matching component of a player's message would cause a penalty containing 'failmsg'.
--   If not, the component is assumed to be unnecessary.
unnec :: String -> Rule
unnec s = unnecPenalty s ("unnecessarily saying '{}'" %)

unnecPenalty :: String -> (String -> String) -> Rule
unnecPenalty s penaltyMsg act e@(Action p a m) gs =
  let ms = split m
      r = regexProcess s
   in --lm = length $ _messages gs in
      foldr
        (.)
        id
        ( withoutEach
            ( \ms' x ->
                when (__ $ isJust $ matchRegexAll r x) $
                  when
                    (__ $ none (checkMatch $ "receives penalty.*{}" % mkFailMsg x) (_messages $ act (Action p a (reconstitute ms')) gs))
                    (doBefore $ penalty 1 $ penaltyMsg x)
            )
            ms
        )
        act
        e
        gs
unnecPenalty _ _ act e gs = act e gs
