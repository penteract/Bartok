module Rules where

import Control.Lens((%~),(^.),(^?),(&),_1,_2,_Just,at,ix)
import Control.Applicative
import Data.Maybe
import qualified Data.Map as Map (toList,insertWith,keys,findWithDefault,lookup)
import Control.Arrow ((***),second)
import Control.Monad (ap,join)

import DataTypes
import Lib

r8 :: Rule
r8 = onLegalCard (\card event ->
    if (rank card == Eight) then nextTurn else doNothing)


reverseDirection :: Step
reverseDirection = players %~ (\ (c:cs) -> c: reverse cs)

rq :: Rule --reverse direction on q, may have problems if reversing direction makes a move become illegal
rq act e gs = onLegalCard (\ card event gs'->
                if (rank card == Queen)
                    then act e (reverseDirection gs)
                    else gs'
                ) act e gs

rlast :: Rule
rlast = onLegalCard
            (\card e'@(Action p _ m) gs'->
                if maybe False ((==1).length) (gs'^.hands.at p) && not ("last card" `findIn` m)
                    then legalPenalty 1 "failure to declare \"last card\"" p gs'
                    else gs' )
      . banPhrase 1 "False \"last card\" pronouncement" (\(p,a,m) gs -> maybe False ((/=1).length) (gs^.hands.at p) && ("last card" `findIn` m))

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
         let --f b = if b then 1 else 0 (this is fromEnum)
             count7 = readVar "sevens" gs
             veries = concat $ replicate count7 " very"
             veries' = concat $ replicate (count7-1) " very"
             bidm = ("Have a" ++ veries ++ " nice day")
             thankm = "Thank you" ++ veries' ++ if count7 > 0 then " much" else ""
             i = fst $ removeAll "(Have a( very)* nice day)|(Thank you( very)*( very much)?)" m
             bePolite :: Maybe String -> Step
             bePolite c = let pens = i - fromEnum (isJust c) in
                            (if pens > 0 then legalPenalty pens "Excessive politeness" p else doNothing)
                              . (case c of
                                  Just s -> mustSay s e
                                  Nothing -> doNothing)
             gs' = act e gs in
         case a of
             (Draw n) | count7 > 0 , isTurn p gs ->
                 if n == 2*count7
                   then bePolite (Just thankm) . setVar "sevens" 0 $ gs'
                   else bePolite (Just thankm) $ penalty 1 ("Failure to draw "++show (2*count7)++" cards.") e gs
             (Play c) | gs'^.lastMoveLegal, rank c == Seven ->
                 bePolite (Just bidm) . modifyVar "sevens" (+1) $ gs'
             (Play c) | gs'^.lastMoveLegal, count7 > 0 -> -- rank c /= Seven
                 bePolite (Just thankm) $ penalty 1 ("Failure to draw "++show (2*count7)++" cards.") e gs
             _ -> bePolite Nothing gs' )

-- turn order when one+ hand(s) [is/are] empty
-- multiple winners
gSnap :: Rule'
gSnap = (
        onAction (\(p,a,m) act e gs -> if readVar "snapdeal" gs == 0
           then broadcast "Snap! dealing"
              . ((deck /\ hands) %~ (\(d,hs) ->
                     ([], foldr (\(c,p)-> Map.insertWith (++) p [c]) hs
                                (zip d (cycle (Map.keys hs)))) ) )
              . setVar "snapdeal" 1 $ gs
           else act e gs) .
         onAction(\(p,a,m) _ e gs ->
            let pickUpDeck p s = (\gs' -> case filter (null.snd) (Map.toList $ gs'^.hands) of
                                     [] -> gs'
                                     (p':_) -> win (fst p') gs')
                                 . setVar "snapped" 0
                                 . (join $ ap (if'.not.null.(^.deck))
                                              (broadcast (p++" picks up the deck for "++s)
                                              . (players %~ (uncurry (flip (++)) . span (/=p)))
                                              . ((deck /\ hands.at p._Just) %~ (\(d,h)-> ([],h++d)))))
                nextTurn' gs = gs & (players %~ (uncurry (++) . span (\p -> Just [] /= Map.lookup p (gs^.hands)) . (\(p:ps)->ps++[p]))) in
            case a of
                (Draw n) ->
                    if not (null (gs^.deck)) && gs^?deck.ix 0._1 == gs^?deck.ix 1._1
                        then if readVar "snapped" gs == length (gs^.players) - 1
                            then setVar "snapped" 0
                                 . pickUpDeck p "snapping last"
                                 $ gs
                            else broadcast (p++" snapped!") . modifyVar "snapped" (+1) $ gs
                        else pickUpDeck p "snapping badly" gs
                (Play c) | readVar "snapped" gs > 0 -> pickUpDeck p "attempting to play with a snap in session" gs
                (Play c) ->
                    if isTurn p gs
                        then nextTurn' . broadcastp p m . broadcast (p++" plays the "++[uniCard c]) . (deck %~ (c:)) . cardFromHand' p c $ gs
                        else pickUpDeck p "playing out of turn" gs )
         ,
         (\v p gs -> GV {
             _handsV = uncurry (flip (++)) . span ((/=p).fst) -- put p to the front
                     . map (second (\h -> case h of [] -> []; _ -> [CardBack])) -- turn Cards into CardViews
                     . map (ap (,) (flip (Map.findWithDefault []) (gs^.hands))) -- tuple each player with their hand
                     $ (gs^.seats) ,
             _pileV = [CardBack] ,
             _deckV = case gs^.deck of
                          [] -> []
                          (c:cs) -> CardFace c : map (const CardBack) cs ,
             _messagesV = gs^.messages })
         )
gSnap' :: (Game,Viewer)
gSnap' = gameFromRule gSnap

gameFromRule :: (Rule,ViewRule)->(Game,Viewer)
gameFromRule = ($ undefined) *** ($ undefined)

defaultRules = [r7',rlast,r8,rq,rMao]
defaultRulesNamed = [("r7'",r7'),("rLastCard",rlast),("r8",r8),("rq",rq),("rMao",rMao)]
