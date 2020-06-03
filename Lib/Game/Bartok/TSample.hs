{-|
Module      : TSample
Description : Sample rules using Tlib.

Looking at the source code is recommended
-}
module Game.Bartok.TSample(r7,r8,rq,rSpade)
 where
import Game.Bartok.TLib
import Game.Bartok.Views(mapHands)
import Game.Bartok.DataTypes(_deck, shuffleDeck)

{-
Note that when submitting a rule, the game server expects an expression.
To allow definitions and type signatures, use @let@ e.g.

> let r8 :: rule
>     r8 = when (isLegal ~&~ cardIs ((==Eight) . rank)) (doAfter nextTurn)
>   in r8

(also remember to tick Tlib and untick RuleHelpers)
-}

-- When a card is legal and is an eight, then skip a player.
r8 :: Rule
r8 = when (isLegal ~&~ cardIs ((==Eight) . rank)) (doAfter nextTurn)

reverseDirection :: GameState -> GameState
reverseDirection = modifyPlayers (\(c:cs) ->c:reverse cs)

-- When a card is legal and is a Queen, reverse direction.
-- This needs to use 'doBefore' rather than 'doAfter' since the inner ruleset is
-- responsible for advancing the turn, so we need to reverse the order before
-- the inner ruleset runs.
--
-- Note that using doBefore wouldn't work with r8, because the inner ruleset checks
-- who's turn it is, so if a turn was skipped before the card was played, the move would become illegal
rq' :: Rule
rq' = when (isLegal ~&~ cardIs ((==Queen) . rank))
      (doBefore reverseDirection)

-- This is an improvement to rq'. It is possible that playing a queen may become illegal if the
-- turn order is reversed (that cannot happen under standard rules, but imagine
-- someone has already added a rule requiring that you say the name of the next player),
-- in which case the direction would be reversed, but the card would not be played.
-- This is better - it does not check if the move is legal before reversing direction
-- and if the move was illegal, then it undoes the reversal.
rq :: Rule
rq = when (cardIs ((==Queen) . rank))
      (doBefore reverseDirection
        . when (not_ isLegal) (doAfter reverseDirection))

-- Penalize players that do not name the card when a spade is played.
-- @\ game -> foldr ($) game rules@ combines a list of @rules@ into a single rule.
-- 'sometimesSay' means that players will be penalized if they name a spade which is not played.
rSpade :: Rule
rSpade inner = foldr ($) inner  [sometimesSay ("{} of Spades"%show r) (cardIs (==(r,Spades)))  | r <- [Ace .. King]]

-- When a 3 is played, treat all cards of that suit as though they were one higher.
-- This is implemented by internally incrementing everything of that suit once,
-- then reducing them in the view.
r3 :: Rule
r3 = when (isLegal ~&~ cardIs ((==Three) . rank))
    (withCard (\(r,s) -> doAfter (modifyVar (show s) (+1) .
                              mapAllCards (\(r',s') -> (if s == s' then toEnum ((fromEnum r' `mod` 14) + 1) else r', s')) )))

-- Note that this does not mess with messages, giving people some ability to keep track
r3V :: ViewRule
r3V = (\ v n gs ->
  let f (CardFace (r,s)) = CardFace (toEnum (((fromEnum r - 1 - readVar (show s) gs) `mod` 14) + 1), s)
      f x = x in
    case (v n gs) of
      GV hs p d m -> mapHands (map f) (const $ const (GV hs (map f p) (map f d) m)) undefined undefined
    )



rBadgerN :: Int -> Rule
rBadgerN n = when isLegal $
             sometimesSayPenalty ("that's{} the badger" % almosts)
                           (cardIs (\c -> suit c == Diamonds && abs (fromEnum (rank c) - 9) == n))
                           ("Failure to{} identify the wildlife" % almosts)
                           ("Incorrectly{} identifying wildlife" % almosts)
                 where almosts = concat$ replicate n " almost"

rBadger :: Rule
rBadger inner = foldr rBadgerN inner [0..2]


rNoHTML :: Rule
rNoHTML = withMessage $ \m ->
    when (__$ sanitise m /= m) $
      doBefore (penalty 1 "Hacking") . modifyMessage sanitise
  where sanitise = (>>= sanChar)
        sanChar '<' = "&lt;"
        sanChar '>' = "&gt;"
        sanChar c = [c]

-- TODO: allow changing to a specific named suit
rAce = withCard (\ c -> when (__ (rank c == Ace) ~&~ isLegal) (doAfter (setVar "rAce" 1)))
 .
 when (boolVar "rAce") (withCard (\c ->
   with getTop (\top ->
     let t2        = (rank top, suit c)
         swapTop d = if d==t2 then top else if d==top then t2 else d
      in (doBefore$ mapAllCards swapTop)
         . (doAfter$ mapAllCards swapTop)
     )
   ) . when isLegal (doAfter (setVar "rAce" 0))
 )
-- map1 ; checkIfLegal ; innerAct ; map2


-- Warning: unnec doesn't play nice with this
rSnap = when (not_$ boolVar "extra_deck")
    (doBefore $ shuffleDeck
      . setVar "extra_deck" 1
      . (\gs -> gs{_deck = _deck gs ++ [minBound..]}))
  . with getTop (\c ->
      when ((cardIs (== c)) ~&~ (said "snap!")) $
        withPlayer $ \p -> doBefore (modifyPlayers (makeFirst p)))
  where
    makeFirst p ps = let (l,r) = break (==p) ps in r++l

a >|< b = "(" ++ a ++"|" ++ b ++ ")"

r7 :: Rule
r7 = unnecPenalty ("thank you(( very)* very much)?" >|< "have a( very)* nice day") (const "Excessive politeness")
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
