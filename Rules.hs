module Rules where

import Control.Lens((%~),(^.),_2,at)
import Control.Applicative

import DataTypes
import Lib

r8 :: Rule
r8 = onLegalCard (\card event ->
    if (rank card == Eight) then nextTurn else doNothing)


reverseDirection :: Step
reverseDirection = players %~ reverse

rq :: Rule --reverse direction on q, may have problems if reversing direction makes a move become illegal
rq act e gs = onLegalCard (\ card event gs'->
                if (rank card == Queen)
                    then act e (reverseDirection gs)
                    else gs'
                ) act e gs

rlast :: Rule
rlast = onLegalCard
            (\card e'@(Action p _ m) gs'->
                if maybe False ((==1).length) (gs'^.hands.at p) && not (said m "last card")
                    then legalPenalty 1 ("failure to declare \"last card\"") p gs'
                    else gs' )

-- TODO: make it order sensitive (Mao should only be sayable last)
-- rMao :: Rule
-- rMao act e gs = (onAction (\(p,a,m) act' e' gs'->
--                     if said m "mao" && (act' e' gs')^.winner /= Just p
--                         then penalty 3 ("Lying, cheating, stealing, deceiving, taking the name of the Chairman in vain.") e' gs'
--                         else act' e' gs' )
--               . onLegalCard
--                     (\card e'@(Action p _ m) gs'->
--                         if gs'^.winner == Just p && not (said m "mao")
--                             then penalty 1 ("Failure to declare Mao!") e gs
--                             else gs' )) act e gs

rMao :: Rule
rMao  = onAction (\(p,a,m) act e gs->
            let next = act e gs
                won = next^.winner == (Just p)
                saidmao = said m "mao" in
            case (saidmao, won) of
                (True,False) -> legalPenalty 4 "Lying, cheating, deceiving, taking the name of the Chairman in vain."
                    p next
                (False,True) -> act e
                    (legalPenalty 1 "Failure to declare Mao!" p gs)
                _ -> next)

(^.^) = liftA2 (.)

(^^.^^) = liftA2 (^.^)

ifSaid :: String -> Game -> Game
ifSaid s g e@(Action p a m) = if s `findIn` m then  g e
    else doNothing
ifSaid s g _ = doNothing

getState :: Event -> GameState -> GameState
getState = flip const

rMao'' :: Rule
rMao''  = onAction (\(p,a,m)->
            with (,) (\(e,gs) ->
                doAfter (with' (\_ gs -> _winner gs ==Just p)
                  (when' id (mustSay "mao")
                       ^^.^^
                   when' not
                    (ifSaid "mao"
                        (penalty 3 ("Lying, cheating, stealing, deceiving, taking the name of the Chairman in vain.")))
                    )
                  )
                )
            )


r7' :: Rule
r7' =  onAction (\(p,a,m) act e gs ->
         let count7 = readVar "sevens" gs
             veries = concat $ replicate count7 " very"
             veriesmuch = concat (replicate (count7-1) " very")
                             ++if count7 > 0 then " much" else ""
             (b',m') = removeIn ("Have a"++veries++" nice day") m
             (b'',m'') = removeIn ("Thank you"++veriesmuch) m
             (i',_) = removeAll "Have a( very)* nice day" m
             (i'',_) = removeAll "Thank you( very)*( much)?" m
             bePolite i = let b1 = i == 1 -- should bid good day
                              b2 = i == 2 -- should thank
                              f b = if b then 1 else 0 -- eg. bid pens = i' - f b1
                              pens = i' + i'' - f (b1 || b2)
                              saying
                                  | b1 = ("Have a"++veries++" nice day")
                                  | b2 = ("Thank you"++veriesmuch)
                                  | otherwise = ""
                              reqSpeak = saying /= "" in
                            if pens > 0 then legalPenalty pens "Excessive politeness" p else doNothing
                            . if reqSpeak then mustSay saying e else doNothing
             gs' = act e gs in
         case a of
             (Draw n) | count7 > 0 , isTurn p gs ->
                 if n == 2*count7
                   then bePolite 2 . setVar "sevens" 0 $ gs'
                   else bePolite 2 $ penalty 1 ("Failure to draw "++show (2*count7)++" cards.") e gs
             (Play c) | gs'^.lastMoveLegal, rank c == Seven ->
                 bePolite 1 . modifyVar "sevens" (+1) $ gs'
             (Play c) | gs'^.lastMoveLegal, count7 > 0 -> -- rank c /= Seven
                 bePolite 2 $ penalty 1 ("Failure to draw "++show (2*count7)++" cards.") e gs
             _ -> bePolite 0 gs' )

defaultRules = [rlast,r8,rq,rMao]
defaultRulesNamed = [("r7'",r7'),("rLastCard",rlast),("r8",r8),("rq",rq),("rMao",rMao)]