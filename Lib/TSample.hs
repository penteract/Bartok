{-|
Module      : TSample
Description : Sample rules using Tlib.

Looking at the source code is reccomended
-}
module TSample(r7,r8,rq,rSpade)
 where
import TLib


a >|< b = "(" ++ a ++"|" ++ b ++ ")"

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


reverseDirection :: GameState -> GameState
reverseDirection = modifyPlayers (\(c:cs) ->c:reverse cs)


-- | This seems better - it does not check if the move is legal before reversing direction
rq :: Rule
rq = when (cardIs ((==Queen) . rank))
      (doBefore reverseDirection
        . when (not_ isLegal) (doAfter reverseDirection))

rSpade :: Rule
rSpade = flip (foldr ($))  [sometimesSay ("{} of Spades"%show c) (cardIs (==(c,Spades)))  | c <- [Ace .. King]]


r8 :: Rule
r8 = when (isLegal ~&~ cardIs ((==Eight) . rank)) (doAfter nextTurn)
