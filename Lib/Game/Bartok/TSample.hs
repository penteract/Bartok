{-|
Module      : TSample
Description : Sample rules using Tlib.

Looking at the source code is recommended
-}
module Game.Bartok.TSample(r7,r8,rq,rSpade)
 where
import Game.Bartok.TLib
import Game.Bartok.Views(mapHands)


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


reverseDirection :: GameState -> GameState
reverseDirection = modifyPlayers (\(c:cs) ->c:reverse cs)


-- This seems better - it does not check if the move is legal before reversing direction
rq :: Rule
rq = when (cardIs ((==Queen) . rank))
      (doBefore reverseDirection
        . when (not_ isLegal) (doAfter reverseDirection))

rSpade :: Rule
rSpade = flip (foldr ($))  [sometimesSay ("{} of Spades"%show c) (cardIs (==(c,Spades)))  | c <- [Ace .. King]]

-- | When a 3 is played, treat all cards of that suit as though they were one higher.
--   This is implemented by internally incrementing everything of that suit once,
--   then reducing them in the view.
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


r8 :: Rule
r8 = when (isLegal ~&~ cardIs ((==Eight) . rank)) (doAfter nextTurn)

rBadgerN :: Int -> Rule
rBadgerN n = when isLegal $
             sometimesSayPenalty ("that's{} the badger" % almosts)
                           (cardIs (\c -> suit c == Diamonds && abs (fromEnum (rank c) - 9) == n))
                           ("Failure to{} identify the wildlife" % almosts)
                           ("Incorrectly{} identifying wildlife" % almosts)
                 where almosts = concat$ replicate n " almost"

rBadger :: Rule
rBadger = flip (foldr ($)) [rBadgerN n | n <- [0..2]]


rNoHTML :: Rule
rNoHTML = withMessage $ \m ->
    when (__$ sanitise m /= m) $
      doBefore (penalty 1 "Hacking") . modifyMessage sanitise
  where sanitise = (>>= sanChar)
        sanChar '<' = "&lt;"
        sanChar '>' = "&gt;"
        sanChar c = [c]
