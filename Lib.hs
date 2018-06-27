{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Arrow (first,second,(***))
import Control.Lens
import Control.Monad --(join,liftM,liftM2)
--import Control.Monad.Random
import Control.Monad.Trans.State
import Data.Char (toLower,isSpace)
import Data.List (isPrefixOf,stripPrefix,delete)
import qualified Data.Map as Map (Map,insert,findWithDefault,empty,fromList,map,adjust)
import Data.Maybe (listToMaybe)
import System.Random.Shuffle (shuffle')
import System.Random

import DataTypes



--type TState = GameState -> GameState (better to just write GameState->GameState everywhere)

if' :: Bool -> a -> a -> a
if' b a a' = if b then a else a'

if'' :: Bool -> (a->a) -> (a->a)
if'' b a = if' b a id

if2 :: (a->Bool) -> (a->b) -> (a->b) -> a -> b
if2 = liftM3 if' -- if2 b a a' x = if (b x) then (a x) else (a' x)

if2' :: (a->Bool) -> (a->a) -> a -> a
if2' = flip flip id . if2 -- if2' b a = if2 b a id

baseAct :: Game
baseAct e@(Action p a m) gs
    | (Draw n)<-a
        = ( broadcast (p++" draws "++show n++" cards.")
          . if'' (not $ null m) (broadcast (p++": "++m))
          . draw n p
          . if'' inTurn (nextTurn undefined . (lastMoveLegal .~ True))
          ) gs
    | (Play c)<-a, Just True /= fmap (c`elem`) (getHand p gs) =
          error (p++" attempted invalid play of "++show c)
          -- note: combines two sources of errors (invalid player // card not in valid player's hand)
    | (Play c)<-a
        = let goodCard = null (gs^.pile) || suit c == suit (head (gs^.pile)) || rank c == rank (head (gs^.pile))
              pens = (fromEnum inTurn) + (fromEnum goodCard) in
          ( if' (inTurn && goodCard) ( broadcast (p++" plays "++[uniCard c]) . (lastMoveLegal .~ True) . nextTurn undefined . (cardFromHand' p c) . (cardToPile c) ) (
                broadcast (p++" tries to play "++(if goodCard then "" else "bad card ")
                    ++[uniCard c]++(if inTurn then "" else " out of turn")
                    ++", draws "++show pens++" penalty card"++(if pens > 1 then "s" else ""))
                . (lastMoveLegal .~ False)
                . draw pens p )
            . if'' (not $ null m) (broadcast (p++": "++m))
          ) gs
    where inTurn = (p == gs ^. players . _head )
baseAct Timeout gs = let activePlayer = gs^.players._head in
    ( broadcast ("Penalize "++activePlayer++" 1 card for failure to play within a reasonable amount of time")
    . draw 1 activePlayer ) gs

beginGame :: GameState -> GameState
beginGame = undefined . shuffleDeck -- ap (foldr (draw 5 . fst)) (^. players) . shuffleDeck

--     -- | (Action p (Play c) m)<-e , p == fst (head $ gs ^. players) , suit c == suit (head $ gs ^. pile) || rank c == rank $ gs ^. pile = broadcast (p++" plays "++show c) $ undefined -- need to play the card
--     | (Action p (Play c) m)<-e , p == fst (head $ gs ^. players) = broadcast (p++" tries to play bad card "++show c++", draws 1 card as penalty.") $ draw 1 p gs -- also use m
--     -- | (Action p (Play c) m)<-e , suit c == suit $ gs ^. pile || rank c == rank = broadcast (p++" tries to play "++show c++" out of turn, receives 1 card penalty.") $ draw 1 p gs
--     | (Action p (Play c) m)<-e = broadcast (p++" tries to play bad card "++show c++ "out of turn, receives 2 penalty cards.") $ draw 2 p gs -- also use m
-- --baseAct (Action (p,Play i,m)) g = broadcast m .

broadcast :: String -> GameState -> GameState
broadcast = (messages %~).(:)

draw :: Int -> PlayerIndex -> GameState -> GameState
draw n p = foldl (.) id (replicate n (draw1 p))


getHand :: PlayerIndex -> GameState -> Maybe Hand
getHand p gs = gs^.hands.at p

{-
fromHand :: CardIndex -> Hand -> Maybe Card
--fromHand i h = if i>=0 and i<length h then Just h!!i else Nothing
fromHand 0 (x:xs) = Just x
fromHand n (x:xs) = fromHand (n-1) xs
-}

--does nothing if player is invalid
-- Angus: not quite sure what it's supposed to do but I think this does it!
-- not as efficient as can be, should "break" once the player is found
-- withHand :: PlayerIndex -> (Hand -> (Hand,GameState)) -> GameState -> GameState
-- withHand p f gs = uncurry (players .~) $
--     foldr (\(n,h) (l,gs') ->
--         if p == n then let (h',gs'') = f h in ((n,h'):l,gs'')
--             else ((n,h):l,gs')
--           ) ([],gs) (gs^.players)

-- a card will "disappear" if the player isn't valid
draw1 :: PlayerIndex -> GameState -> GameState
draw1 p = uncurry (((hands . at p) %~) . fmap . (:)) . cardFromDeck
  -- uncurry ((ix p %~) . (:)) . cardFromDeck  -- withHand p (\h -> first (:h) $ cardFromDeck gs) gs

shuffleDeck :: GameState -> GameState
shuffleDeck gs = let (gen1,gen2) = split (gs^.randg) in
    (deck %~ (\x-> shuffle' x (length x) gen1) ) . (randg .~ gen2) $ gs

taxes :: GameState -> GameState
-- taxes gs = (hands %~ Map.map (take handSizeThreshhold))
--            $ (deck <>~ foldMap (drop handSizeThreshhold) (gs^.hands)) gs
taxes gs = (hands %~ Map.map (take handSizeThreshhold))
         . ((deck <>~) =<< foldMap (drop handSizeThreshhold) . (^. hands))
         $ gs
  where handSizeThreshhold = cardCount `div` (playerCount+1)
        playerCount = length (gs^.players)
        cardCount = length (gs^.deck) + length (gs^.pile) + (foldr ((+).length) 0 (gs^.hands))
        -- cardCount = length (gs^.deck) + length (gs^.pile) + (sum $ map (length.snd) (gs^.players))
        -- cardCount = length (gs^.deck) + length (gs^.pile) + length (gs^.players.folded._2) -- v. nice to read
        -- cardCount = length (gs^.deck) + length (gs^.pile) + sumOf (hands . folded . _2 . to length) gs



  -- uncurry (players .~) $
  --   foldr (\(p,h) (ps,gs') ->
  --           let (x,y) = splitAt (cardCount `div` playerCount+1) h
  --           in ((p,x):ps, gs' & deck <>~ y))
  --         ([],gs)
  --         (gs^.players)
  --   where playerCount = length (gs^.players)
  --         -- cardCount = length (gs^.deck) + length (gs^.pile) + (sum $ map (length.snd) (gs^.players))
  --         -- cardCount = length (gs^.deck) + length (gs^.pile) + length (gs^.players.folded._2) -- v. nice to read
  --         cardCount = length (gs^.deck) + length (gs^.pile) + sumOf (players . folded . _2 . to length) gs -- faster but less readable?

cardFromDeck :: GameState -> (Card,GameState)
cardFromDeck gs = case gs ^. deck of
    (c:cs) -> (c, gs & deck .~ cs)
    [] -> let (p1,p2) = splitAt 1 (gs^.pile) in
        case ( ( broadcast "Shuffling pile into deck, applying taxes..."
                 . shuffleDeck
                 . (deck <>~ p2) . (pile .~ p1)
                 . taxes ) gs ) ^. deck of
            (c:cs) -> (c, gs & deck .~ cs)
            _ -> error "No cards in deck after taxes & shuffling pile into deck (where've they gone??)"

cardToPile :: Card -> GameState -> GameState
cardToPile c = pile %~ (c:)

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
cardFromHand' :: PlayerIndex -> Card -> GameState -> GameState
cardFromHand' p c = hands %~ (Map.adjust (delete c) p)
  -- (((players . each) %~) .) . (. (second . delete)) . if2' . (. fst) . (==)
-- cardFromHand' p c = players.each %~ (if2' ((p==).fst) (second (delete c)))
-- cardFromHand' p c = players.each %~ ((ap .) . liftM2 if') ((p==).fst) (second (delete c)) id

--only one of the posssible interpretations
--mkR :: (GameState -> GameState) -> Rule
--mkR f act a g = f (act a g) -- f.(act a) -- (.) f (act a) -- (f.).act -- ((f.).) -- (.)((.)f) -- (.).(.)
--mkR = (.).(.)
--liftM2 (++) tail ((:[]) . head)
-- precond: requires at least one player
nextTurn :: Game -- perhaps nextTurn should also set lastMoveLegal .~ True
-- nextTurn = const ((players .~) =<< (\(x:xs)->xs++[x]) . (^. players))
nextTurn = const $ players %~ (\(x:xs)->xs++[x])
-- nextTurn _ g@GS {players = (p:ps)} = g{players=ps++[p]}
    -- g{nextPlayer = ((nextPlayer g+1) `mod` length (hands g))}

when :: (a -> Bool) -> Game -> a -> Game
when q act x = if q x then act else const id


with :: (GameState -> a) -> (a -> Rule) -> Rule
with = undefined


penalty :: String -> PlayerIndex -> GameState -> GameState
penalty s p = broadcast ("Penalty :"++p++s) . draw1 p . (lastMoveLegal .~ False)

--wasLegal :: Event -> GameState -> GameState

doAfter :: Game -> Rule
doAfter act1 act2 e = act2 e . act1 e

doBefore :: Game -> Rule
doBefore act1 act2 e = act1 e . act2 e

doOnly :: Game -> Rule
doOnly = const

{-
getPlayerCard :: PlayerIndex -> CardIndex -> GameState -> Maybe Card
getPlayerCard p i gs = do
    h <- getHand p gs
    fromHand i h-}

-- | player's next action must be the given one
-- how do I make require actions for something other than a single
require :: (PlayerIndex, Action, String) -> Rule
require (p, a, m) = undefined

onPlay :: (Card -> Rule) -> Rule
onPlay f act e@(Action p (Play c) m) gs = f c act e gs
onPlay f act e gs = act e gs

onLegalCard :: (Card -> Game) -> Rule
onLegalCard f act e@(Action p (Play c) m) s = let s' = act e s in
    if s' ^. lastMoveLegal then f c e s' else s'
onLegalCard f act a s = act a s
