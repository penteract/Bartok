{-# LANGUAGE LambdaCase #-}

module Game.Bartok.RuleHelpers
  ( module Game.Bartok.RuleHelpers,
    BaseGame.nextTurn,
    BaseGame.doNothing,
    BaseGame.illegal,
    BaseGame.broadcast,
    BaseGame.broadcastp,
    BaseGame.penalty,
    BaseGame.isTurn,
    BaseGame.win,
    BaseGame.draw,
  )
where

import Control.Lens ((^.))
import Data.Bifunctor (first, second)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Game.Bartok.BaseGame
import qualified Game.Bartok.BaseGame as BaseGame
import Game.Bartok.DataTypes
import Text.Regex (Regex, matchRegex, matchRegexAll, mkRegex, mkRegexWithOpts, splitRegex)
import Utils (applyUnless, applyWhen)

when :: (a -> Bool) -> Rule -> a -> Rule
when q r x = if q x then r else id

with :: (Event -> GameState -> a) -> (a -> Rule) -> Rule
with get f g e gs = f (get e gs) g e gs

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

regexProcess :: String -> Regex
regexProcess s = mkRegexWithOpts ("^[[:space:]]*" ++ s ++ "[[:space:]]*$") True False

split :: String -> [String]
split = splitRegex (mkRegex ";")

reconstitute :: [String] -> String
reconstitute = intercalate ";"

-- tells if a string is one semi-colon delimited segment of another
-- ignore (segment-)leading/ending whitespace, case-insensitive
findInMs :: String -> String -> Bool
findInMs target = any (isJust . matchRegex (regexProcess target)) . split

--process target `elem` map process (endBy ";" msg)

-- remove up to one semi-colon delimited segment equal to target string
-- ignore (segment-)leading/ending whitespace, case-insensitive
-- note: returned string will lack (segment-)leading/ending whitespace
removeIn :: String -> String -> (Bool, String)
removeIn = (first isJust .) . removeIn'

removeIn' :: String -> String -> (Maybe String, String)
removeIn' target msgs =
  (\(a, b, c) -> (a, reconstitute (b ++ c))) $
    removeIn'' (regexProcess target) [] (split msgs)

removeIn'' :: Regex -> [String] -> [String] -> (Maybe String, [String], [String])
removeIn'' _ ss [] = (Nothing, reverse ss, [])
removeIn'' r ss (s' : ss') = if isJust $ matchRegexAll r s' then (fmap (\(_, b, _, _) -> b) (matchRegexAll r s'), reverse ss, ss') else removeIn'' r (s' : ss) ss'

removeAll :: String -> String -> ([String], String)
removeAll target msgs = second reconstitute $ removeAll' (regexProcess target) ([], []) (split msgs)

removeAllN :: String -> String -> (Int, String)
removeAllN = (first length .) . removeAll

removeAll' :: Regex -> ([String], [String]) -> [String] -> ([String], [String])
removeAll' _ (ss, ss') [] = (reverse ss, reverse ss')
removeAll' r (ss, ss') (s : ss'') = if isJust $ matchRegex r s then removeAll' r (s : ss, ss') ss'' else removeAll' r (ss, s : ss') ss''

-- happens on legal move; not penalized afterwards
mustSay :: String -> Game
mustSay s = \case
  Action p _ m ->
    applyUnless (s `findInMs` m) $
      penalty 1 ("failure to say: '{}'" % s) p
  _ -> doNothing

-- like mustSay but also consumes the desired String
-- note that this means it also returns the modified event
mustSay' :: String -> Event -> (Event, Step)
mustSay' s e@(Action p a m) =
  let (b, m') = removeIn s m
   in if b
        then (Action p a m', doNothing)
        else (e, penalty 1 ("failure to say: '{}'" % s) p)
mustSay' _ e = (e, doNothing)

said :: String -> String -> Bool
said = findInMs

-- n is penalty
-- s is banned string
-- pm is penalty message to display when found

-- tests "on the way out" if you said the phrase
-- this seems to be poorly named
banPhrase :: Int -> String -> ((Name, Action, String) -> GameState -> Bool) -> Rule
banPhrase n pm f = onAction (\t@(p, _, _) act e gs -> if f t (act e gs) then penalty n pm p (act e gs) else act e gs)

-- possibly a mustSay component could be extracted
require :: (Name, Action, String) -> (Bool -> Game) -> Rule
require (p, a, m) f =
  onAction
    ( \(p', a', m') ->
        applyWhen (p == p') $
          if a == a' && (m `findInMs` m')
            then doAfter (f True)
            else
              doAfter (f False)
                . (doOnly $ illegal 1 $ "failure to {}{}" % show a % (if null m then "" else " and say '{}'" % m))
    )

onPlay :: (Card -> Rule) -> Rule
onPlay f act e@(Action _ (Play c) _) gs = f c act e gs
onPlay _ act e gs = act e gs

onLegalCard :: (Card -> Game) -> Rule
onLegalCard f act e@(Action _ (Play c) _) s =
  let s' = act e s
   in if s' ^. lastMoveLegal then f c e s' else s'
onLegalCard _ act a s = act a s

onAction :: ((Name, Action, String) -> Rule) -> Rule
onAction f act e@(Action p a m) = f (p, a, m) act e
onAction _ act e = act e

onDraw :: ((Name, Int) -> Rule) -> Rule
onDraw f =
  onAction
    ( \e -> case e of
        (p, Draw n, _) -> f (p, n)
        _ -> id
    )
