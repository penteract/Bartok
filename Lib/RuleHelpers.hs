{-# LANGUAGE Trustworthy #-}

module RuleHelpers
  ( module RuleHelpers
  , BaseGame.nextTurn , BaseGame.doNothing , BaseGame.illegal
  , BaseGame.broadcast , BaseGame.broadcastp , BaseGame.penalty
  , BaseGame.isTurn , BaseGame.win , BaseGame.draw
) where

import Control.Arrow (first,second)
import Control.Lens ((^.))
import Control.Monad (ap,join)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Text.Regex(Regex,matchRegex,matchRegexAll,mkRegex,mkRegexWithOpts,splitRegex)

import DataTypes
import BaseGame


when :: (a -> Bool) -> Rule -> a -> Rule
when q r x = if q x then r else id

with :: (Event -> GameState -> a) -> (a -> Rule) -> Rule
with get f g e gs = f (get e gs) g e gs
--with = flip(flip.((liftM2 (=<<).flip).).flip)
--with = (((.)(curry.join.(uncurry.)).flip).) . flip(.) . uncurry
--with = ((((curry.(uncurry =<<)).).flip).).flip(.).uncurry

with' :: (Event -> GameState -> a) -> (a -> Game) -> Game
with' get f e gs = f (get e gs) e gs

when' :: (a -> Bool) -> Game -> a -> Game
when' q act x = if q x then act else const id

-- given an incomplete game, returns a rule which does the action described by the game after doing (i.e. as late as possible)
doAfter :: Game -> Rule
doAfter act1 act2 e = act1 e . act2 e

doBefore :: Game -> Rule
doBefore act1 act2 e = act2 e . act1 e

doOnly :: Game -> Rule
doOnly = const

-- | player's next action must be the given one
-- how do I make require actions for something other than a single

-- splits a message into semi-colon separated parts with whitespace stripped
-- splitm :: String -> [String]
-- splitm = map (unpack . strip . pack) . endBy ";"

regexProcess :: String -> Regex
regexProcess s = mkRegexWithOpts ("^[[:space:]]*"++s++"[[:space:]]*$") True False

split :: String -> [String]
split = splitRegex (mkRegex ";")
reconstitute :: [String] -> String
reconstitute = intercalate ";"


-- tells if a string is one semi-colon delimited segment of another
-- ignore (segment-)leading/ending whitespace, case-insensitive
findIn :: String -> String -> Bool
--findIn = liftM2 flip (((.).elem).) ((.endBy ";").map) (CI.mk.strip.pack)
findIn target msg = any  (isJust. matchRegex (regexProcess target)) (split msg)
  --process target `elem` map process (endBy ";" msg)

-- remove up to one semi-colon delimited segment equal to target string
-- ignore (segment-)leading/ending whitespace, case-insensitive
-- note: returned string will lack (segment-)leading/ending whitespace
removeIn :: String -> String -> (Bool,String)
removeIn = (first isJust .) . removeIn'

removeIn' :: String -> String -> (Maybe String,String)
removeIn' target msgs = (\(a,b,c) -> (a,reconstitute (b++c))) $
                            removeIn'' (regexProcess target) [] (split msgs)
removeIn'' :: Regex -> [String] -> [String] -> (Maybe String,[String],[String])
removeIn'' r ss [] = (Nothing,reverse ss,[])
removeIn'' r ss (s':ss') = if isJust $ matchRegexAll r s' then (fmap (\(_,b,_,_)->b) (matchRegexAll r s'),reverse ss,ss') else removeIn'' r (s':ss) ss'
-- UNSAFE HEAD AAAAH

removeAll :: String -> String -> (Int,String)
removeAll target msgs = second reconstitute $
                           removeAll' (regexProcess target) (0,[]) (split msgs)
removeAll' :: Regex -> (Int,[String]) -> [String] -> (Int,[String])
removeAll' r (n,ss) [] = (n,reverse ss)
removeAll' r (n,ss) (s':ss') = if isJust $ matchRegex r s' then removeAll' r (n+1,ss) ss' else removeAll' r (n,s':ss) ss'

--happens on legal move; not penalised afterwards
mustSay :: String -> Game
mustSay s (Action p a m) = if s `findIn` m then doNothing else penalty 1 ("failure to say: '{}'"%s) p
mustSay s _ = doNothing

-- like mustSay but also consumes the desired String
-- note that this means it also returns the modified event
mustSay' :: String -> Event -> (Event,Step)
mustSay' s e@(Action p a m) = let (b,m') = removeIn s m in
                                if b then (Action p a m',doNothing)
                                     else (e,penalty 1 ("failure to say: '{}'"%s) p)
mustSay' s e = (e,doNothing)

said :: String -> String -> Bool
said = findIn

-- n is penalty
-- s is banned string
-- pm is penalty message to display when found
-- banPhrase :: Int -> String -> String -> Rule
-- banPhrase n pm s = banPhrase' n pm (\m e gs -> s `findIn` m)

-- tests "on the way out" if you said the phrase
banPhrase :: Int -> String -> ((PlayerIndex,Action,String) -> GameState -> Bool) -> Rule
banPhrase n pm f = onAction (\t@(p,_,_) -> ((join (ap (if' . f t) (penalty n pm p)) .) .))

-- possibly a mustSay component could be extracted
require :: (PlayerIndex, Action, String) -> (Bool -> Game) -> Rule
require (p, a, m) f = onAction (\(p',a',m') -> if p==p'
    then if a == a' && (m `findIn` m') then (doAfter (f True))
      else doAfter (f False) . (doOnly$ illegal 1 ("failure to {}{}"%show a%(if null m then "" else " and say '{}'"%m)))
    else id )

onPlay :: (Card -> Rule) -> Rule
onPlay f act e@(Action p (Play c) m) gs = f c act e gs
onPlay f act e gs = act e gs

onLegalCard :: (Card -> Game) -> Rule
onLegalCard f act e@(Action p (Play c) m) s = let s' = act e s in
    if s' ^. lastMoveLegal then f c e s' else s'
onLegalCard f act a s = act a s

onAction :: ((PlayerIndex,Action,String) -> Rule) -> Rule
onAction f act e@(Action p a m) = f (p,a,m) act e
onAction f act e = act e

onDraw :: ((PlayerIndex,Int) -> Rule)-> Rule
onDraw f = onAction (\e -> case e of
    (p,Draw n,m) -> f (p,n)
    _ -> id)
