{-# LANGUAGE MultiWayIf #-}

module Sample where

import BaseGame
import RuleHelpers
import DataTypes
import Views
import Control.Lens
import qualified Data.List.NonEmpty as NE
import Control.Monad
import Control.Arrow (first,second)
import Data.Map.Lazy as Map (insertWith,toList,keys,lookup,findWithDefault)
import Data.Maybe (fromJust,isJust)

--all definitions work*
r8 :: Rule
--r8 = onLegalCard$ when' ((==Eight).rank) (fromStep nextTurn)
--r8 = onLegalCard (\card event gs -> if (rank card == Eight) then nextTurn gs else gs)
r8 = onLegalCard (\card event ->
    if rank card == Eight then nextTurn else doNothing)


--r8 = onLegalCard$ when ((==Eight).rank) nextTurn

rbase = const baseAct

ruleset = r8 (rq baseAct)


reverseDirection :: Step
reverseDirection = players %~ reverse

rq :: Rule --reverse direction on q, may have problems if reversing direction makes a move become illegal
-- rq act e gs = (onLegalCard$ when' ((==Queen).rank)
--                   (\e' gs' -> act e (reverseDirection gs))) act e gs
rq act e gs = onLegalCard (\ card event gs'->
                if (rank card == Queen)
                    then act e (reverseDirection gs)
                    else gs'
                ) act e gs


-- rq act e@(Action p (Play c) m) gs = if (rank c == Queen) then
--     if _lastMoveLegal (act e gs) then act e (reverseDirection gs)
--         else act e gs
--         else act e gs
-- rq act e gs = act e gs

--r8 = onLegalCard (\card event -> if (rank card == Queen) then reverseDirection else doNothing)

mustdo7 :: (Int->(Bool->Game)-> Rule)
mustdo7 n f = with (const$ head .(^.players)) (\p ->
              require (p,(Draw (2*n)), "thank you"++ (if n>1 then " "++ concat (replicate (n-1) "very ") ++ "much" else "")) f)

--counts the unresolved 7s
count7s :: GameState -> Int
count7s = readVar "sevens"

r7 :: Rule
r7 = with (const count7s) (\n -> if  n > 0
    then onDraw (\_->(mustdo7 n) (when' id (const$setVar "sevens" 0)))
       . onPlay (\c -> if rank c == Seven
        then onLegalCard$ (\c event ->
                 modifyVar "sevens" (+1)
               . mustSay ("have a "++join(replicate n "very ")++"nice day") event)
        else (mustdo7 n) (when' id (const$ broadcast "r7 unexpected")))
    else onLegalCard$ when' ((==Seven).rank)
             (\ e -> (modifyVar "sevens" (+1))
                   . (mustSay "have a nice day") e))

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
                           if pens > 0 then penalty pens "Excessive politeness" p else doNothing
                           . if reqSpeak then mustSay saying e else doNothing
            gs' = act e gs in
        case a of
            (Draw n) | count7 > 0 , isTurn p gs ->
                if n == 2*count7
                  then bePolite 2 . setVar "sevens" 0 $ gs'
                  else bePolite 2 $ illegal 1 ("Failure to draw "++show (2*count7)++" cards.") e gs
            (Play c) | gs'^.lastMoveLegal, rank c == Seven ->
                bePolite 1 . modifyVar "sevens" (+1) $ gs'
            (Play c) | gs'^.lastMoveLegal, count7 > 0 -> -- rank c /= Seven
                bePolite 2 $ illegal 1 ("Failure to draw "++show (2*count7)++" cards.") e gs
            _ -> bePolite 0 gs' )

rC = onPlay (\ _ act e@(Action p (Play c) m) gs->
        let gs' = act e gs in
        if rank c == Knight
          then case removeIn' "(Clubs)|(Diamonds)|(Hearts)|(Spades)" m & first (>>= runParser parseSuit) of
                   (Just s,m') -> setVar "newSuit" (fromEnum s + 1) $ act (Action p (Play c) m') gs
                   (Nothing,_) -> if gs'^.lastMoveLegal then illegal 1 "Failure to specify new suit" e gs else gs'
          else gs') .
      onPlay (\ _ act e@(Action p (Play c) m) gs->
         let gs' = act e gs
             i = readVar "newSuit" gs'
             ns = toEnum $ i - 1
             altgs = gs & pile.ix 0._2 .~ ns
             altgs' = act e altgs
             restore = altgs' & pile.ix 1 .~ fromJust (gs ^? pile.ix 0)
             restore' = altgs' & pile.ix 0 .~ fromJust (gs ^? pile.ix 0) in
         if i > 0 && altgs'^.lastMoveLegal && altgs ^? pile . ix 0 == altgs' ^? pile . ix 1
             then setVar "newSuit" 0 restore
         else if i > 0 && not (altgs'^.lastMoveLegal) && altgs ^? pile . ix 0 == altgs' ^? pile . ix 0
             then restore'
         else gs')

                -- else onLegalCard (\card e->
                --     if (rank card == Seven)
                --       then modifyVar "sevens" (+1) . mustSay "have a nice day" e
                --       else doNothing)) act e gs

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

nextTurnDo :: String -> (Game -> Event -> GameState -> Bool) -> Rule -> Rule
nextTurnDo s f r = (\act e gs -> (if f act e gs then setVar s 1 else id) (act e gs))
                   . (\act e gs -> if readVar s gs == 1 && gs^?players.ix 0 == eventPlayer e
                                     then setVar s 0 $ r act e gs
                                     else act e gs)

nextTurnDoUntil :: String -> (Game -> Event -> GameState -> Bool) -> (Game -> Event -> GameState -> (GameState,Bool)) -> Rule
nextTurnDoUntil s f r = (\act e gs -> (if f act e gs then setVar s 1 else id) (act e gs))
                       . (\act e gs -> if readVar s gs == 1 && gs^?players.ix 0 == eventPlayer e
                                       then case r act e gs of
                                           (g,True) -> setVar s 0 g
                                           (g,False) -> g
                                       else act e gs)

-- let's try writing "upon a 6, any suit is valid"
r6 = nextTurnDo "r6"
                (\ _ e _ -> case e of
                              (Action _ (Play c) _) | rank c == Six -> True
                              _ -> False) $
                onPlay
                    (\c act e gs ->
                        let gs' = act e gs
                            altgs = gs & pile . ix 0 . _2 .~ suit c
                            altgs' = act e altgs
                            restoregs' = altgs' & pile . ix 1 . _2 .~ fromJust (gs^?pile.ix 0._2) in
                        if Just (suit c) /= gs ^? pile . ix 0 . _2 && not (gs'^.lastMoveLegal) && altgs' ^. lastMoveLegal && (altgs^?pile.ix 0) == (altgs'^?pile.ix 1)
                        then restoregs'
                        else gs')

run7 :: Int -> Rule
run7 = undefined
