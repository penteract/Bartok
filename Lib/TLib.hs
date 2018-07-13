{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : TLib
Description : An alternative to RuleHelpers; uses more higher order functions and fewer lenses

This approaches monads, but we aren't using those.
This presents operations with
-}
module TLib(
    -- * The fundamental type
    GEGSto,
    -- *users
    -- These functions give access to the arguments in a composable manner
    withAction,withMessage,with,

    -- *Operators
    when, whether, onNextTurn, uponDoUntil,
    (.:),__,
    -- * Tests
    isAction,isLegal,isTurn,
    actionIs,cardIs,boolVar,said,
    (~&~),not_,
    -- * getters
    getVar,state,
    -- *Helper Functions
    --  These should help with common patterns
    unnec,mustSay,mustDo,sometimesSay,
    modifyPlayers,
    -- * class
    Ruleable(..),
    -- * From DataTypes:
    Step,Game,Rule,
    GameState,Event(..),Action(..),
    PlayerIndex,
    -- ** cards
    Card, CardView(..),GameView(..),
    suit,rank,Suit(..),Rank(..),
    Viewer, ViewRule,
    readVar,setVar,modifyVar,
    nextTurn,draw,broadcast,(%))
 where

import DataTypes
import Control.Applicative
import Data.Maybe(isJust)
import BaseGame(nextTurn,draw,broadcast,(%),illegal)
import RuleHelpers(findInMs,split,regexProcess,reconstitute)
import Text.Regex(matchRegexAll,matchRegex, mkRegexWithOpts)

--import qualified BaseGame

type GEGSto a =
       Game -- ^ The inner ruleset
    -> Event -- ^ The event under consideration
    -> GameState -- ^ The previous state of the game
    -> a

-- | A class for types that are 'smaller' than `Rule` so that there are different options for how to convert.
class Ruleable a where
    doAfter :: a -> Rule -- ^ Perform an action after the inner ruleset.
    doBefore :: a -> Rule -- ^ Perform an action before the inner ruleset.
    doOnly :: a -> Rule -- ^ Perform an action and ignore the inner ruleset.
                        --   This should be used rarely.

instance Ruleable Step where
    doAfter s act e gs  = s $ act e gs
    doBefore s act e gs = act e $ s gs
    doOnly s act e gs   = s gs

instance Ruleable Game where
    doAfter act1 act2 e = act1 e . act2 e
    doBefore act1 act2 e = act2 e . act1 e
    doOnly = const

-- | returns True if the event is an Action
isAction :: GEGSto Bool
isAction _ (Action _ _ _) _ = True
isAction _ _ _ = False

-- | returns True if the event is a Play action and the card satisfies the condition
cardIs :: (Card -> Bool) -- ^The condition
            -> GEGSto Bool
cardIs f _ (Action _ (Play c) _) _= f c
cardIs _ _ _ _ = False

-- | returns True if the event is an action and is legal under the inner ruleset
isLegal :: GEGSto Bool
isLegal act e gs = isAction act e gs && (_lastMoveLegal (act e gs))


actionIs :: (Action -> Bool) -> GEGSto Bool
actionIs q _ (Action _ a _) _ = q a
actionIs _ _ _ _ = False

isTurn :: GEGSto Bool
isTurn act (Action p a m) gs = p == head (_players gs)
isTurn _ _ _ = False

boolVar :: VarName -> GEGSto Bool
boolVar v act e gs = readVar v gs /= 0

(~&~) :: GEGSto Bool -> GEGSto Bool -> GEGSto Bool
(~&~) = liftA2(liftA2(liftA2 (&&)))

not_ :: GEGSto Bool -> GEGSto Bool
not_ = fmap$fmap$fmap not

(^^^&^^^) :: (Applicative f1,Applicative f2, Applicative f3) => f1 (f2 (f3 Bool)) -> f1 (f2 (f3 Bool)) -> f1 (f2 (f3 Bool))
(^^^&^^^) = liftA2(liftA2(liftA2 (&&)))

when :: (GEGSto Bool) -> Rule -> Rule
when f r act e gs = if f act e gs then r act e gs else act e gs
-- when f r = whether f r id


whether :: (GEGSto Bool) -> Rule -> Rule -> Rule
whether f r1 r2 act e gs = if f act e gs then r1 act e gs else r2 act e gs

-- whether = liftA2$liftA2$liftA2$ if'

-- | Given a fresh variable name, when a condition is met, run an action on the following turn
onNextTurn :: VarName -- ^ a fresh varName
          -> (GEGSto Bool) -- ^ the condition which must be met
          -> Rule -- ^ what to do on the next turn
          -> Rule
onNextTurn v f r = when f (doAfter$ setVar v 1)
                 . when (isTurn ~&~ boolVar v) ((doAfter (setVar v 0)) . r)

-- Upon a condition being satisfied, implement a rule until a second condition is satisfied
uponDoUntil :: VarName -> GEGSto Bool -> Rule -> GEGSto Bool -> Rule
uponDoUntil v upon r untl =  when upon (doAfter$ setVar v 1)
                 . when untl (doAfter$ setVar v 0)
                 . when (boolVar v) r

-- testPlayer :: (String -> Bool) -> GEGSto Bool
-- testPlayer f act e@(Action p _ _) gs = f p
-- testPlayer _ = false
--
--
-- withPlayer :: GEGSto (Maybe String)

-- | When an action happens, do something with it
withAction :: (Action -> Rule) -> Rule
withAction f act e@(Action _ a _) gs = f a act e gs
withAction f act e gs = act e gs

-- | When an action happens, do something with the message that gets sent
withMessage :: (String -> Rule) -> Rule
withMessage f act e@(Action _ _ m) gs = f m act e gs
withMessage f act e gs = act e gs

with :: GEGSto a -> (a -> GEGSto b) -> GEGSto b
with getter f act e gs = f (getter act e gs) act e gs

getVar :: String -> GEGSto Int
getVar s act e gs = readVar s gs

said :: String -> GEGSto Bool
said s act (Action _ _ m) gs = s `findInMs` m
said _ _ _ _ = False

-- | Award a penalty to a player without preventing their action
penalty :: Int -> String -> Game
penalty n reason (Action p a m) = draw n p . broadcast ("{} receives penalty {}: {}"%p%show n%reason)
penalty _ _ _ = id


--illegal _ _ _ = id

failmsg s = "failure to say '{}'"%s

-- | penalize if `s` is not said
mustSay s = when (not_$ said s) (doBefore (penalty 1 (failmsg s)))

-- | If the condition holds, require that they say  the message; otherwise, penalize them if they say it
sometimesSay :: String -> GEGSto Bool -> Rule
sometimesSay s cond = whether cond (mustSay s)
                                   (when (said s) (doBefore$ penalty 1 ("unnecessarily saying '{}'"%s)))

--sometimesSay s cond = unnec s . when cond (mustSay s)


-- | Any action except the specified one is an illegal move
--   when the specified action occurs, run the second argument
mustDo :: Action -> Rule -> Rule
mustDo act whenDone = when isAction$
    whether (actionIs (==act))
      whenDone
      (doOnly$ illegal 1 ("failure to {}"%show act))

modifyPlayers :: ([Name] ->[Name])-> Step
modifyPlayers f g = g{_players = f (_players g)}



-- | abbreviation for \ _ _ _ ->
__ :: a -> GEGSto a
__ = const.const.const

-- |
state :: GEGSto GameState
state _ _ gs = gs


-- could try Control.DotDotDot
-- | lift an action
(.:) :: (a -> b) -> GEGSto a -> GEGSto b
(.:) f g a b c = f (g a b c)


checkMatch pattern input = isJust$ matchRegex (mkRegexWithOpts pattern True False) input


-- | Apply a function to `rest` and `x` for each possible `x` in a list where rest is the list with `x` removed
withoutEach :: ([a] -> a -> b) -> [a] -> [b]
withoutEach f [] = []
withoutEach f (x:xs) = withoutEach' f ([],x,xs)
    where
        withoutEach' f t@(xs,x,[]) = [f (reverse xs) x]
        withoutEach' f t@(xs,x,y:ys) = f (reverse xs ++ ys) x : withoutEach' f (x:xs,y,ys)

-- | penalize for unnecessarily saying something
unnec :: String -> Rule
unnec s act e@(Action p a m) gs =
            let ms = split m
                r = regexProcess s in
                --lm = length $ _messages gs in
            foldr (.) id (withoutEach (\ms' x -> when (__$ isJust $ matchRegexAll r x)$
              when (__$not$ any (checkMatch$ "receives penalty.*{}"%failmsg x) (_messages$ act (Action p a (reconstitute ms')) gs))
                  (doBefore$ penalty 1 ("unnecessarily saying '{}'"%x))) ms)
              act e gs
unnec _ act e gs = act e gs
