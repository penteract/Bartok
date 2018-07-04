module Rules where

import Control.Lens((%~),(^.),_2,at)

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
rMao :: Rule
rMao act e gs = (onAction (\(p,a,m) g ->
                    if said m "mao" && (act e gs)^.winner /= Just p
                        then penalty 3 ("Lying, cheating, stealing, deceiving, taking the name of the Chairman in vain.")
                        else g )
              . onLegalCard
                    (\card e'@(Action p _ m) gs'->
                        if gs'^.winner == Just p && not (said m "mao")
                            then penalty 1 ("Failure to declare Mao!") e gs
                            else gs' )) act e gs

defaultRules = [rlast,r8,rq,rMao]
defaultRulesNamed = [("rLastCard",rlast),("r8",r8),("rq",rq),("rMao",rMao)]
