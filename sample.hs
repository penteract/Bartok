module Sample where
import Lib


r8 :: Rule
r8 = onLegalCard$ when ((==Eight).rank) nextTurn
--r8 = onLegalCard$ when ((==Eight).rank) nextTurn

reverseDirection = undefined

rq :: Rule
rq = onLegalCard$ when ((==Queen).rank) reverseDirection

r7 :: Rule
--r7 = undefined --good luck

r7 = (prev7 (count7s (\n -> mustdraw (2*n)))) . on7 mustsay "have a nice day"


--counts the unresolved 7s
count7s :: GameState -> Int

r7 = onPlayCard (\c -> _ count7s
    (if rank c == Seven then run7
        else (\n -> when (>0) (require ((Draw (2*n)),
            "thank you"++ (if n>1 then " "++ join (replicate (n-1) "very ") ++ "much" else ""))))
        ))


run7 :: Int -> Rule
