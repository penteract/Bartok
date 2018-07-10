{-# LANGUAGE FlexibleInstances #-}
module TLib where

import DataTypes
import Control.Applicative
import Data.Maybe(isJust)
import BaseGame(nextTurn,draw,broadcast,(%),illegal)
import RuleHelpers(findIn,split,regexProcess,reconstitute)
import Text.Regex(matchRegexAll,matchRegex, mkRegexWithOpts)

import qualified BaseGame

type VarName = String
type GEGSto a = Game -> Event -> GameState -> a

class Ruleable a where
    doAfter :: a -> Rule
    doBefore :: a -> Rule
    doOnly :: a -> Rule

instance Ruleable Step where
    doAfter s act e gs  = s $ act e gs
    doBefore s act e gs = act e $ s gs
    doOnly s act e gs   = s gs

instance Ruleable Game where
    doAfter act1 act2 e = act1 e . act2 e
    doBefore act1 act2 e = act2 e . act1 e
    doOnly = const

cardIs :: (Card -> Bool) -> Game -> Event -> GameState -> Bool
cardIs f _ (Action _ (Play c) _) _= f c
cardIs _ _ _ _ = False

isLegal :: Game -> Event -> GameState -> Bool
isLegal act e gs = isAction act e gs && (_lastMoveLegal (act e gs))

isAction :: Game -> Event -> GameState -> Bool
isAction _ (Action _ _ _) _ = True
isAction _ _ _ = False

actionIs :: (Action -> Bool) -> Game -> Event -> GameState -> Bool
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

when :: (Game -> Event -> GameState -> Bool) -> Rule -> Rule
when f r act e gs = if f act e gs then r act e gs else act e gs
-- when f r = whether f r id


whether :: (Game -> Event -> GameState -> Bool) -> Rule -> Rule -> Rule
whether f r1 r2 act e gs = if f act e gs then r1 act e gs else r2 act e gs

-- whether = liftA2$liftA2$liftA2$ if'

onNextTurn :: VarName -> (Game -> Event -> GameState -> Bool) -> Rule -> Rule
onNextTurn v f r = when f (doAfter$ setVar v 1)
                 . when (isTurn ~&~ boolVar v) ((doAfter (setVar v 0)) . r)


uponDoUntil :: VarName -> GEGSto Bool -> Rule -> GEGSto (Bool) -> Rule
uponDoUntil v upon r untl =  when upon (doAfter$ setVar v 1)
                 . when untl (doAfter$ setVar v 1)
                 . when (boolVar v) r

-- testPlayer :: (String -> Bool) -> GEGSto Bool
-- testPlayer f act e@(Action p _ _) gs = f p
-- testPlayer _ = false
--
--
-- withPlayer :: GEGSto (Maybe String)

withAction :: (Action -> Rule) -> Rule
withAction f act e@(Action _ a _) gs = f a act e gs
withAction f act e gs = act e gs

withMessage :: (String -> Rule) -> Rule
withMessage f act e@(Action _ _ m) gs = f m act e gs
withMessage f act e gs = act e gs

with :: GEGSto a -> (a -> GEGSto b) -> GEGSto b
with getter f act e gs = f (getter act e gs) act e gs

getVar :: String -> GEGSto Int
getVar s act e gs = readVar s gs

said :: String -> GEGSto Bool
said s act (Action _ _ m) gs = s `findIn` m
said _ _ _ _ = False

-- | Award a penalty to a player without preventing their action
penalty :: Int -> String -> Game
penalty n reason (Action p a m) = draw n p . broadcast ("{} receives penalty {}: {}"%p%show n%reason)
penalty _ _ _ = id


--illegal _ _ _ = id

failmsg s = "failure to say: '{}'"%s

-- | penalise if `s` is not said
mustSay s = when (not_$ said s) (doBefore (penalty 1 (failmsg s)))

-- | If the condition holds, require that they say  the message; otherwise, penalise them if they say it
sometimesSay :: String -> GEGSto Bool -> Rule
sometimesSay s cond = whether cond (mustSay s)
                                   (when (said s) (doBefore$ penalty 1 ("unnecessarily saying '{}'"%s)))


-- | Any action except the specified one is an illegal move
--   when the specified action occurs, run the second argument
mustDo :: Action -> Rule -> Rule
mustDo act whenDone = when isAction$
    whether (actionIs (==act))
      whenDone
      (doOnly$ illegal 1 ("failure to {}"%show act))


r8 :: Rule
r8 = when (isLegal ~&~ cardIs ((==Eight) . rank)) (doAfter nextTurn)

modifyPlayers :: ([Name] ->[Name])-> Step
modifyPlayers f g = g{_players = f (_players g)}

reverseDirection :: GameState -> GameState
reverseDirection = modifyPlayers (\(c:cs) ->c:reverse cs)

--rq = when (isLegal ~&~ cardIs ((==Queen) . rank)) (doBefore reverseDirection)

-- | This seems better - it does not check if the move is legal before reversing direction
rq :: Rule
rq = when (cardIs ((==Queen) . rank))
      (doBefore reverseDirection
        . when (not_ isLegal) (doAfter reverseDirection))

rSpade = flip (foldr ($))  [sometimesSay ("{} of Spades"%show c) (cardIs (==(c,Spades)))  | c <- [Ace .. King]]

-- | abbreviation for \ _ _ _ ->
__ :: a -> GEGSto a
__ = const.const.const

state :: GEGSto GameState
state _ _ gs = gs


-- could try Control.DotDotDot
(.:) :: (a -> b) -> GEGSto a -> GEGSto b
(.:) f g a b c = f (g a b c)


checkMatch pattern input = isJust$ matchRegex (mkRegexWithOpts pattern True False) input

-- | penalise for unnecessarily saying something
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


withoutEach :: ([a] -> a -> b) -> [a] -> [b]
withoutEach f [] = []
withoutEach f (x:xs) = withoutEach' f ([],x,xs)

withoutEach' :: ([a] -> a -> b)-> ([a],a,[a]) -> [b]
withoutEach' f t@(xs,x,[]) = [f (reverse xs) x]
withoutEach' f t@(xs,x,y:ys) = f (reverse xs ++ ys) x : withoutEach' f (x:xs,y,ys)


a >|< b = "(" ++ a ++")|(" ++ b ++ ")"

r7 :: Rule
r7 = unnec ("thank you(( very)* very much)?" >|< "have a( very)* nice day")
   . with (getVar "r7") (\nsevens ->
       when (isLegal ~&~ isSeven) (
           doAfter (modifyVar "r7" (+1))
         . mustSay ("have a{} nice day"%veries nsevens) )
       . when (boolVar "r7" ~&~ isTurn ~&~ not_ isSeven) (mustDo (Draw (2*nsevens)) (
           doAfter (setVar "r7" 0)
         . mustSay (thanks nsevens)))
     )
     where isSeven = cardIs ((==Seven) . rank)
           thanks 1 = "thank you"
           thanks n = "thank you{} much"%veries (n-1)
           veries n = concat$ replicate n " very"


--r7 = when (isLegal ~&~ cardIs ((==Seven) . rank))  onSeven . when (boolVar "r7" ~&~ isTurn)
