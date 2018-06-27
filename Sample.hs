module Sample where
import Lib

--main = return ()

r8 :: Rule
r8 = onLegalCard$ when ((==Eight).rank) nextTurn
--r8 = onLegalCard$ when ((==Eight).rank) nextTurn

reverseDirection = undefined

rq :: Rule
rq = onLegalCard$ when ((==Queen).rank) reverseDirection



mustdo7 :: (Int->(Action,String))
mustdo7 n = ((Draw (2*n)), "thank you"++ (if n>1 then " "++ concat (replicate (n-1) "very ") ++ "much" else ""))

--counts the unresolved 7s
count7s :: GameState -> Int
count7s = undefined



--r7 :: Rule
--r7 = undefined --good luck
--r7 = (prev7 (count7s (\n -> mustdraw (2*n)))) . on7 mustsay "have a nice day"
-- r7 = onPlay (\c -> with count7s
--     (if rank c == Seven then run7
--         else (\n -> when (>0) (require (mustdo7 n))
--         )))


run7 :: Int -> Rule
run7 = undefined
