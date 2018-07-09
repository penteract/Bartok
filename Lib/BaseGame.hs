{-# LANGUAGE ScopedTypeVariables #-}
module BaseGame where

import Control.Arrow (first)
import Control.Lens ((.~),(%~),(%%~),(^.),(^?),(&),(<>~),_1,at,each,ix)
import Control.Monad (liftM2,liftM3)
import Data.List (delete)
import qualified Data.List.NonEmpty as NE ((<|),head)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map (adjust,insert,mapAccum)
import Data.Text (pack,unpack,strip)

import DataTypes

doNothing :: Step
doNothing = id

fromStep :: Step -> Game
fromStep = const

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
    where inTurn = isTurn p gs
baseAct Timeout gs = let activePlayer = head $ gs^.players in
    ( broadcast ("Penalize "++activePlayer++" 1 card for failure to play within a reasonable amount of time")
    . draw 1 activePlayer ) gs
baseAct (PlayerJoin n) gs = addPlayer n gs

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


-- taxes takes cards from each hand down to handSizeTreshhold and puts them into the deck
-- cards are removed "newest first"
taxes :: Step
taxes gs = uncurry (deck <>~) . ( (hands . each) %%~ (splitAt =<< subtract handSizeThreshhold . length)) $ gs
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

cardFromHand' :: PlayerIndex -> Card -> Step
cardFromHand' p c = hands %~ Map.adjust (delete c) p

isTurn :: PlayerIndex -> GameState -> Bool
isTurn p gs = p == (gs^.players.ix 0)

-- precond: requires at least one player
nextTurn :: Step -- perhaps nextTurn should also set lastMoveLegal .~ True
nextTurn = players %~ (\(x:xs)->xs++[x])

-- | incomplete
win :: PlayerIndex -> Step
win p = broadcast (p++" wins the game!") . (winner .~ Just p)
