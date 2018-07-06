{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import Control.Arrow (first,second)
import Control.Lens ((.~),(%~),(^.),(^?),(&),(<>~),_1,at,ix)
import Control.Monad (ap,join,liftM2,liftM3)
import Data.List (delete,intercalate)
import qualified Data.List.NonEmpty as NE ((<|),head)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map (adjust,insert,mapAccum)
import Data.Text (pack,unpack,strip)

import Text.Regex(Regex,matchRegex,matchRegexAll,mkRegex,mkRegexWithOpts,splitRegex)
import Data.Maybe(isJust)

import DataTypes

doNothing :: Step
doNothing = id

fromStep :: Step -> Game
fromStep = const

if' :: Bool -> a -> a -> a
if' b a a' = if b then a else a'

if'' :: Bool -> (a->a) -> (a->a)
if'' b a = if' b a id

(%) :: String -> String -> String
(%) ('{':'}':s) x = x++s
(%) ('\\':c:s) x = c:(s%x)
(%) (c:s) xs = c:(s%xs)
(%) "" _ = error "not enough '{}' in string"

(%%) s = (s%) . (:[]). uniCard

--TODO(angus): rename these

penalty ::Int -> String -> Game
penalty n reason e@(Action p a m) =
    draw n p .
    broadcast ("{} recieves penalty {}: {}"%p%show n%reason) .
    sayAct e .
    (case a of
        Play c -> broadcast (p++" tries to play {}"%%c)
        Draw n -> broadcast (p++" tries to draw {}"%show n)).
    (lastMoveLegal .~ False)


--a penalty which does not end the turn
legalPenalty ::Int -> String -> PlayerIndex -> Step
legalPenalty n reason p =
    draw n p .
    broadcast ("{} recieves penalty {}: {}"%p%show n%reason)

broadcastp :: PlayerIndex -> String -> Step
broadcastp p m = if'' (not $ null m) (broadcast (p++": "++m))
sayAct :: Game
sayAct e@(Action p a m) = broadcastp p m
sayAct _ = id

addPlayer :: Name -> Step
addPlayer n = draw 5 n . (hands %~ Map.insert n [])
            . (players /\ seats %~ addToSeat n)

addToSeat n (ps,ss) = if length ss>1 then
    (ps++[n],
        (\(a,b:bs)->a++b:n:bs)
        (break (liftM2 (||) (==head ps) (==last ps)) ss))
        else (ps ++ [n],ss ++ [n])


-- addToSeat n (ps,ss) = if length ss>1 then
--     (ps++[n],
--         (\(a,b:bs)->a++b:n:bs)
--         (span (\c -> (c/=head ps) && (c/=last ps)) ss))
--         else (ps ++ [n],ss ++ [n])


play::PlayerIndex -> Event -> Card -> Step
play p e c = (\ gs -> if getHand p gs == Just [] then win p gs else gs ) -- check for winning
    . sayAct e
    . broadcast ("{} plays {}"%p%%c)
    . (lastMoveLegal .~ True)
    . nextTurn
    . (cardToPile c)
    . (cardFromHand' p c)

baseAct :: Game
baseAct e@(Action p a m) gs
    | (Draw n)<-a
        = ( sayAct e
          . broadcast (p++" draws "++show n++" cards.")
          . draw n p
          . if'' (inTurn && n > 0) (nextTurn . (lastMoveLegal .~ True))
          ) gs
    | (Play c)<-a, Just True /= fmap (c`elem`) (getHand p gs) =
          penalty 1 (p++" attempted invalid play of "++show c) e gs
    | (Play c)<-a =
         (if not inTurn
              then (penalty 1 "Playing out of turn" e)
              else if not (suit c == suit (NE.head (gs^.pile)) || rank c == rank (NE.head (gs^.pile)))
                                          then penalty 1 "Bad card" e
                                          else play p e c)
                     gs
    where inTurn = p == (head $ gs ^. players)
baseAct Timeout gs = let activePlayer = head $ gs^.players in
    ( broadcast ("Penalize "++activePlayer++" 1 card for failure to play within a reasonable amount of time")
    . draw 1 activePlayer ) gs
baseAct (PlayerJoin n) gs = addPlayer n gs

beginGame :: Step
beginGame = undefined -- ap (foldr (draw 5)) (^. players) . shuffleDeck -- ap (foldr (draw 5 . fst)) (^. players) . shuffleDeck

broadcast :: String -> Step
broadcast = (messages %~).(:)

draw :: Int -> PlayerIndex -> Step
draw n p = foldl (.) id (replicate n (draw1 p))


getHand :: PlayerIndex -> GameState -> Maybe Hand
getHand p gs = gs^.hands.at p


-- a card will "disappear" if the player isn't valid
draw1 :: PlayerIndex -> Step
--draw1 p = uncurry (((hands . at p) %~) . fmap . (:)) . cardFromDeck
draw1 p = (\(c,gs) -> ((hands . at p) %~ fmap (c:)) gs) . cardFromDeck
  -- uncurry ((ix p %~) . (:)) . cardFromDeck  -- withHand p (\h -> first (:h) $ cardFromDeck gs) gs

-- shuffleDeck :: GameState -> GameState
-- shuffleDeck gs = let (gen1,gen2) = split (gs^.randg) in
--     (deck %~ (\x-> shuffle' x (length x) gen1) ) . (randg .~ gen2) $ gs


taxes :: Step
-- taxes gs = gs & deck /\ hands %~
--                uncurry ((. Map.mapAccum ((. (splitAt =<< (-) handSizeThreshhold . length)) . first . (++)) [])
--                    . first . (++))
taxes gs = gs & (deck /\ hands) %~ (\(d,h)-> first (d++) --first (d++) does nothing if the deck is empty
                (Map.mapAccum (\l h'->first (++l) (splitAt (length h'-handSizeThreshhold) h')) [] h) )
  where handSizeThreshhold = cardCount `div` (playerCount+1)
        playerCount = length (gs^.players)
        cardCount = length (gs^.deck) + length (gs^.pile) + (foldr ((+).length) 0 (gs^.hands))

cardFromDeck :: GameState -> (Card,GameState)
cardFromDeck gs = case gs ^. deck of
                    (c:cs) -> (c, gs & deck .~ cs)
                    [] -> let (newPile,restofpile) = (\(x:|xs) -> (x:|[],xs)) (gs^.pile) in
                          let gs' = ( broadcast "Shuffling pile into deck, applying taxes..."
                                    . shuffleDeck
                                    . (deck <>~ restofpile) . (pile .~ newPile)
                                    . taxes ) gs in
                          case gs' ^. deck of
                              (c:cs) -> (c, gs' & deck .~ cs)
                              _ -> error "No cards in deck after taxes & shuffling pile into deck"

cardToPile :: Card -> Step
cardToPile c = pile %~ (c NE.<|)

-- also checks whether any such card was removed from hand
-- cardFromHand :: PlayerIndex -> Card -> GameState -> (Bool,GameState)
-- cardFromHand p c gs = second (flip (set players) gs) $
--     foldr (ap (ap ((.) . first . (||)) . ((second . (:)) .) . ap ((.) . (,) . fst) (flip (`if''` delete c) . snd)) ((p ==) . fst))
--     -- foldr (\x y -> let b = ((p==).fst $ x) in (first (b||) . second ((fst x,if'' b (delete c) (snd x)):)) y)
--     -- (\x@(p',h) (b,l) ->
--     --       if (p==p'&&c`elem`h) then (True,(p,delete c h):l) else (b,x:l) )
--           (False,[])
--           (gs^.players)
  -- let gs' = (gs & players.each %~ (((ap .) . liftM2 if') ((p==).fst) (second (delete c)) id)) in undefined
  -- (\(p',h) -> if (p==p') then (p',delete c h) else (p',h))) in undefined

--doesn't tell you whether any card was removed
cardFromHand' :: PlayerIndex -> Card -> Step
cardFromHand' p c = hands %~ Map.adjust (delete c) p

isTurn :: PlayerIndex -> GameState -> Bool
isTurn p gs = p == (gs^.players.ix 0)

-- precond: requires at least one player
nextTurn :: Step -- perhaps nextTurn should also set lastMoveLegal .~ True
nextTurn = players %~ (\(x:xs)->xs++[x])

when :: (a -> Bool) -> Rule -> a -> Rule
when q r x = if q x then r else id

with :: (Event -> GameState -> a) -> (a -> Rule) -> Rule
with get f g e gs = f (get e gs) g e gs
--with = flip(flip.((liftM2 (=<<).flip).).flip)
--with = (((.)(curry.join.(uncurry.)).flip).) . flip(.) . uncurry
--with = ((((curry.(uncurry =<<)).).flip).).flip(.).uncurry

with' :: (Event -> GameState -> a) -> (a -> Game) -> Game
with' get f e gs = f (get e gs) e gs

when' :: (a -> Bool) -> Game -> a -> Game
when' q act x = if q x then act else const id




--penalty :: String -> PlayerIndex -> GameState -> GameState
--penalty s p = broadcast ("Penalty :"++p++s) . draw1 p . (lastMoveLegal .~ False)

--wasLegal :: Event -> GameState -> GameState

-- given an incomplete game, returns a rule which does the action described by the game after doing (i.e. as late as possible)
doAfter :: Game -> Rule
doAfter act1 act2 e = act1 e . act2 e

doBefore :: Game -> Rule
doBefore act1 act2 e = act2 e . act1 e

doOnly :: Game -> Rule
doOnly = const

-- | incomplete
win :: PlayerIndex -> Step
win p = broadcast (p++" wins the game!") . (winner .~ Just p)

-- | player's next action must be the given one
-- how do I make require actions for something other than a single

-- splits a message into semi-colon separated parts with whitespace stripped
-- splitm :: String -> [String]
-- splitm = map (unpack . strip . pack) . endBy ";"

regexProcess :: String -> String
regexProcess s = "^[[:space:]]*"++s++"[[:space:]]*$"
-- tells if a string is one semi-colon delimited segment of another
-- ignore (segment-)leading/ending whitespace, case-insensitive
findIn :: String -> String -> Bool
--findIn = liftM2 flip (((.).elem).) ((.endBy ";").map) (CI.mk.strip.pack)
findIn target msg = any  ((Nothing /=) . matchRegex (mkRegexWithOpts (regexProcess target) True False)) (splitRegex (mkRegex ";") msg)
  --process target `elem` map process (endBy ";" msg)

-- remove up to one semi-colon delimited segment equal to target string
-- ignore (segment-)leading/ending whitespace, case-insensitive
-- note: returned string will lack (segment-)leading/ending whitespace
removeIn :: String -> String -> (Bool,String)
removeIn = (first isJust .) . removeIn'

removeIn' :: String -> String -> (Maybe String,String)
removeIn' target msgs = (\(a,b,c) -> (a,intercalate ";" (b++c))) $
                            removeIn'' (mkRegexWithOpts (regexProcess target) True False) [] (splitRegex (mkRegex ";") msgs)
removeIn'' :: Regex -> [String] -> [String] -> (Maybe String,[String],[String])
removeIn'' r ss [] = (Nothing,reverse ss,[])
removeIn'' r ss (s':ss') = if isJust $ matchRegexAll r s' then (fmap (\(_,b,_,_)->b) (matchRegexAll r s'),reverse ss,ss') else removeIn'' r (s':ss) ss'
-- UNSAFE HEAD AAAAH

removeAll :: String -> String -> (Int,String)
removeAll target msgs = second (intercalate ";") $
                           removeAll' (mkRegexWithOpts (regexProcess target) True False) (0,[]) (splitRegex (mkRegex ";") msgs)
removeAll' :: Regex -> (Int,[String]) -> [String] -> (Int,[String])
removeAll' r (n,ss) [] = (n,reverse ss)
removeAll' r (n,ss) (s':ss') = if isJust $ matchRegex r s' then removeAll' r (n+1,ss) ss' else removeAll' r (n,s':ss) ss'

--happens on legal move; not penalised afterwards
mustSay :: String -> Game
mustSay s (Action p a m) = if s `findIn` m then doNothing else legalPenalty 1 ("failure to say: '{}'"%s) p
mustSay s _ = doNothing

-- like mustSay but also consumes the desired String
-- note that this means it also returns the modified event
mustSay' :: String -> Event -> (Event,Step)
mustSay' s e@(Action p a m) = let (b,m') = removeIn s m in
                                if b then (Action p a m',doNothing)
                                     else (e,legalPenalty 1 ("failure to say: '{}'"%s) p)
mustSay' s e = (e,doNothing)

said :: String -> String -> Bool
said = findIn

-- n is penalty
-- s is banned string
-- pm is penalty message to display when found
-- banPhrase :: Int -> String -> String -> Rule
-- banPhrase n pm s = banPhrase' n pm (\m e gs -> s `findIn` m)

-- tests "on the way out" if you said the phrase
banPhrase :: Int -> String -> ((PlayerIndex,Action,String) -> GameState -> Bool) -> Rule
banPhrase n pm f = onAction (\t@(p,_,_) -> ((join (ap (if' . f t) (legalPenalty n pm p)) .) .))

-- possibly a mustSay component could be extracted
require :: (PlayerIndex, Action, String) -> (Bool -> Game) -> Rule
require (p, a, m) f = onAction (\(p',a',m') -> if p==p'
    then if a == a' && (m `findIn` m') then (doAfter (f True))
      else doAfter (f False) . (doOnly$ penalty 1 ("failure to {}{}"%show a%(if null m then "" else " and say '{}'"%m)))
    else id )

onPlay :: (Card -> Rule) -> Rule
onPlay f act e@(Action p (Play c) m) gs = f c act e gs
onPlay f act e gs = act e gs

onLegalCard :: (Card -> Game) -> Rule
onLegalCard f act e@(Action p (Play c) m) s = let s' = act e s in
    if s' ^. lastMoveLegal then f c e s' else s'
onLegalCard f act a s = act a s

onAction :: ((PlayerIndex,Action,String) -> Rule) -> Rule
onAction f act e@(Action p a m) = f (p,a,m) act e
onAction f act e = act e

onDraw :: ((PlayerIndex,Int) -> Rule)-> Rule
onDraw f = onAction (\e -> case e of
    (p,Draw n,m) -> f (p,n)
    _ -> id)