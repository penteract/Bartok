{-|
Module      : BaseGame
Description : Basic functions needed to set up a simple game

This contains functions for a simple game.
-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
--TODO : list exports explicitly
module BaseGame where

import Control.Arrow (first,second)
import Control.Lens ((.~),(%~),(%%~),(^.),(^?),(&),(<>~),_1,at,each,ix)
import Control.Monad (liftM2,liftM3)
import Data.List (delete)
import qualified Data.List.NonEmpty as NE ((<|),head)
--import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map (adjust,insert,mapAccum,delete,lookup)
import Data.Text (pack,unpack,strip)
import Data.Maybe (fromJust,isJust,fromMaybe)

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

appendl :: NonEmpty a -> [a] -> NonEmpty a
appendl (x :| xs) l = x :| (xs ++ l)
-- | Returns a game where the move has not occured and a penalty has been given.
-- Should be used with 'doOnly'
illegal ::Int -> String -> Game
illegal n reason e@(Action p a m) =
    draw n p .
    broadcast ("{} recieves penalty {}: {}"%p%show n%reason) .
    sayAct e .
    (case a of
        Play c -> broadcast (p++" tries to play {}"%%c)
        Draw n -> broadcast (p++" tries to draw {}"%show n)).
    (lastMoveLegal .~ False)


-- | A penalty which does not end the turn
penalty ::Int -> String -> Name -> Step
penalty n reason p =
    if n > 0 then
        (winner %~ (\mw -> if mw == Just p then Nothing else mw))
        . draw n p
        . broadcast ("{} receives penalty {}: {}"%p%show n%reason)
    else doNothing

-- | Broadcast a message while identifying the player it came from.
broadcastp :: Name -> String -> Step
broadcastp p m = if'' (not $ null m) (broadcast (p++": "++m))

-- | Broadcast the message a player sent with their action.
sayAct :: Game
sayAct e@(Action p a m) = broadcastp p m
sayAct _ = id

-- | Add a player to the game
addPlayer :: Name -> Maybe (Name,Name) -> Step
addPlayer n mps = draw 5 n . (hands %~ Map.insert n [])
                  . (players %~ addToSeat n mps)

addToSeat :: Name -> Maybe (Name,Name) -> [Name] -> [Name]
addToSeat n mps ps = case mps of
                         Just (pl,pr) | ps /= [pl] -> (\(a,b:bs)->a++b:n:bs) (break (liftM2 (||) (==pl) (==pr)) ps)
                         _ -> n:ps

remPlayer :: Name -> Step
remPlayer n =   (hands %~ Map.delete n) .  (players %~ filter (/=n)) .
    (\ k -> (pile %~ flip appendl (fromMaybe [] $ Map.lookup n (k ^. hands))) k)
-- addToSeat n (ps,ss) = if length ss>1 then
--     (ps++[n],
--         (\(a,b:bs)->a++b:n:bs)
--         (span (\c -> (c/=head ps) && (c/=last ps)) ss))
--         else (ps ++ [n],ss ++ [n])


-- | Play a move and tidy up the game state.
play::Name -> Event -> Card -> Step
play p e c = (\ gs -> if getHand p gs == Just [] then win p gs else gs ) -- check for winning
    . sayAct e
    . broadcast ("{} plays {}"%p%%c)
    . (lastMoveLegal .~ True)
    . nextTurn
    . (cardToPile c)
    . (cardFromHand' p c)

-- | The default behaviour.
-- Draws always succeed, a card may be played if it is in the player's hand, it is the player's turn and it matches either the suit or rank of the previous card.
-- Timeouts give one penalty card to the player whose turn it is.
baseAct :: Game
baseAct e@(Action p a m) gs
    | (Draw n)<-a
        = ( sayAct e
          . broadcast (p++" draws "++show n++" cards.")
          . draw n p
          . if'' (inTurn && n > 0) (nextTurn . (lastMoveLegal .~ True))
          ) gs
    | (Play c)<-a, Just True /= fmap (c`elem`) (getHand p gs) =
          illegal 1 (p++" attempted invalid play of "++show c) e gs
    | (Play c)<-a =
         (if not inTurn
              then (illegal 1 "Playing out of turn" e)
              else if not (suit c == suit (NE.head (gs^.pile)) || rank c == rank (NE.head (gs^.pile)))
                                          then illegal 1 "Bad card" e
                                          else play p e c)
                     gs
    where inTurn = isTurn p gs
baseAct Timeout gs = case gs^.players of
    [] -> gs -- If there aren't any players,don't penalize anyone for timeouts
    (activePlayer:_) -> broadcast ("Penalize "++activePlayer++" 1 card for failure to play within a reasonable amount of time")
        . draw 1 activePlayer $ gs
baseAct (PlayerJoin n mps) gs = broadcast ("Player "++n++" joined the game!") . addPlayer n mps $ gs
baseAct (PlayerLeave n) gs = broadcast ("Player "++n++" left the game!") . remPlayer n $ gs

-- | Add a message to the list of messages. Will be seen by all players by default.
broadcast :: String -> Step
broadcast = (messages %~).(:)

-- | Move n cards from the deck to a player's hand
draw :: Int -> Name -> Step
draw n p = foldl (.) id (replicate n (draw1 p))

-- | Get the hand of the current player.
getHand :: Name -> GameState -> Maybe Hand
getHand p gs = gs^.hands.at p


-- a card will "disappear" if the player isn't valid
-- | Move a single card from the deck to a player's hand.
draw1 :: Name -> Step
--draw1 p = uncurry (((hands . at p) %~) . fmap . (:)) . cardFromDeck
draw1 p = (\(c,gs) -> ((hands . at p) %~ fmap (++[c])) gs) . cardFromDeck
  -- uncurry ((ix p %~) . (:)) . cardFromDeck  -- withHand p (\h -> first (:h) $ cardFromDeck gs) gs

-- shuffleDeck :: GameState -> GameState
-- shuffleDeck gs = let (gen1,gen2) = split (gs^.randg) in
--     (deck %~ (\x-> shuffle' x (length x) gen1) ) . (randg .~ gen2) $ gs


-- taxes takes cards from each hand down to handSizeTreshhold and puts them into the deck
-- cards are removed "newest first"
-- | If any players have more than their share of cards, give return them to the deck (so that players can always draw cards and be given penalties).
taxes :: Step
taxes gs = uncurry (deck <>~) . ( (hands . each) %%~ (splitAt =<< subtract handSizeThreshhold . length)) $ gs
  where handSizeThreshhold = cardCount `div` (playerCount+1)
        playerCount = length (gs^.players)
        cardCount = length (gs^.deck) + length (gs^.pile) + (foldr ((+).length) 0 (gs^.hands))

-- | Remove the top card of the deck shuffling the pile back in and applying taxes if necessary.
cardFromDeck :: GameState -> (Card,GameState)
cardFromDeck gs = case gs ^. deck of
                    (c:cs) -> (c, gs & deck .~ cs)
                    [] -> let gs' = ( broadcast "Shuffling pile into deck, applying taxes..."
                                    . shuffleDeck
                                    . uncurry (deck <>~) . (pile %%~ (\(x:|xs) -> (xs,x:|[])) )
                                    . taxes ) gs in
                          case gs' ^. deck of
                              (c:cs) -> (c, gs' & deck .~ cs)
                              _ -> error "No cards in deck after taxes & shuffling pile into deck"

-- | Put a card on the pile
cardToPile :: Card -> Step
cardToPile c = pile %~ (c NE.<|)

-- | Remove a card from a players hand.
cardFromHand' :: Name -> Card -> Step
cardFromHand' p c = hands %~ Map.adjust (delete c) p

-- | Test if it is a player's turn.
isTurn :: Name -> GameState -> Bool
isTurn p gs = p == (gs^.players.ix 0)

-- precond: requires at least one player
-- | Advance the turn order by one.
nextTurn :: Step -- perhaps nextTurn should also set lastMoveLegal .~ True
nextTurn = players %~ (\(x:xs)->xs++[x])

-- | incomplete?
win :: Name -> Step
win p = -- broadcast (p++" wins the game!") .
        (winner .~ Just p)
