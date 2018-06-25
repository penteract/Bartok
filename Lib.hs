{-# LANGUAGE TemplateHaskell, GADTs #-}

module Lib where

import Control.Arrow (first,second)
import Control.Lens
import Control.Monad (join,liftM,liftM2)
import Control.Monad.Random
import Control.Monad.Trans.State
import Data.List
import Data.Maybe
import Data.Char
import System.Random.Shuffle (shuffle')

type Parser = StateT String Maybe

next :: (Enum a, Bounded a, Eq a) => a -> a
next a = if a == maxBound then minBound else succ a
prev ::(Enum a, Bounded a, Eq a) =>  a -> a
prev a = if a == minBound then maxBound else pred a

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Show,Eq,Enum,Bounded)
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show,Eq,Enum,Bounded)

suitChar :: Suit -> Char
suitChar s = case s of
  Clubs -> 'C'
  Diamonds -> 'D'
  Hearts -> 'H'
  Spades -> 'S'


rankChar :: Rank -> Char
rankChar r = (['A'] ++ [head $ show i | i <- [2..9]::[Int] ] ++ ['T','J','Q','K'])!!fromEnum r

type Card = (Rank,Suit)
type Hand = [Card]

uniCard :: Card -> Char
uniCard (r,s) = toEnum (0x1F0A0 + fromEnum s * 16 + fromEnum r + 1)

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = undefined

parseRank :: Parser Rank
parseRank = StateT (\s ->
  fmap (\(r,c) -> (r,drop (length c) s)) $ listToMaybe $
    filter (\(_,c) -> isPrefixOf (map toLower c) (map toLower s))
      (map (\x -> (x,show x)) (enumFrom minBound) ++ map (\x -> (x,((:[]).rankChar) x)) (enumFrom minBound)))

parseSuit :: Parser Suit
parseSuit = StateT (\s ->
  fmap (\(r,c) -> (r,drop (length c) s)) $ listToMaybe $
    filter (\(_,c) -> isPrefixOf (map toLower c) (map toLower s))
      (map (\x -> (x,show x)) (enumFrom minBound) ++ map (\x -> (x,((:[]).suitChar) x)) (enumFrom minBound)))

ignore :: String -> Parser ()
ignore s = StateT (\s' -> let s'' = dropWhile isSpace s' in
                            let s''' = stripPrefix s s'' in
                              fmap ((,) () . dropWhile isSpace) s''')

parseCard :: Parser Card
parseCard = do
  r <- parseRank
  ignore "of"
  s <- parseSuit
  return (r,s)

type CardIndex = Int

type Name = String
type PlayerIndex = Name
data Action = Draw Int | Play Card
data Event = Action PlayerIndex Action String | Timeout


-- I'd call a function playmove or runevent. the problem is that it's used too much
--the type could almost be called Game
type Game = Event -> GameState -> GameState
type Rule = Game -> Game --this type is named correctly

data GameState = GS { _players :: [(Name,Hand)], -- current player is head of list
       _deck :: [Card],
       _pile :: [Card],
    --nextPlayer :: Player, --required to be smaller than length hands
       _messages :: [String],
       _lastMoveLegal :: Bool,
       _prevGS :: Maybe (GameState,Action),

       _randg :: StdGen
     }
makeLenses ''GameState

hands :: GameState -> [Hand]
hands gs = map snd $ gs^.players
-- map snd (gs^.players)


suit :: Card -> Suit
suit = snd

rank :: Card -> Rank
rank = fst



--type TState = GameState -> GameState (better to just write GameState->GameState everywhere)



baseAct :: Game
baseAct e gs
    | (Action p (Draw n) m)<-e , p == fst (head $ gs ^. players) = broadcast (p++" draws "++show n++" cards.") . broadcast (p++": "++m) . draw n p $ nextTurn undefined gs
    | (Action p (Draw n) m)<-e = broadcast (p++" draws "++show n++" cards.") . broadcast (p++": "++m) $ draw n p gs
    -- | (Action p (Play c) m)<-e , p == fst (head $ gs ^. players) , suit c == suit (head $ gs ^. pile) || rank c == rank $ gs ^. pile = broadcast (p++" plays "++show c) $ undefined -- need to play the card
    | (Action p (Play c) m)<-e , p == fst (head $ gs ^. players) = broadcast (p++" tries to play bad card "++show c++", draws 1 card as penalty.") $ draw 1 p gs -- also use m
    -- | (Action p (Play c) m)<-e , suit c == suit $ gs ^. pile || rank c == rank = broadcast (p++" tries to play "++show c++" out of turn, receives 1 card penalty.") $ draw 1 p gs
    | (Action p (Play c) m)<-e = broadcast (p++" tries to play bad card "++show c++ "out of turn, receives 2 penalty cards.") $ draw 2 p gs -- also use m
--baseAct (Action (p,Play i,m)) g = broadcast m .

broadcast :: String -> GameState -> GameState
broadcast = join . ((messages .~) .) . (. (^. messages)) . (:)

draw :: Int -> PlayerIndex -> GameState -> GameState
draw n p = foldl (.) id (replicate n (draw1 p))


getHand :: PlayerIndex -> GameState -> Maybe Hand
getHand p gs = lookup p (gs^.players)

{-
fromHand :: CardIndex -> Hand -> Maybe Card
--fromHand i h = if i>=0 and i<length h then Just h!!i else Nothing
fromHand 0 (x:xs) = Just x
fromHand n (x:xs) = fromHand (n-1) xs
-}

--does nothing if player is invalid
-- Angus: not quite sure what it's supposed to do but I think this does it!
-- not as efficient as can be, should "break" once the player is found
withHand :: PlayerIndex -> (Hand -> (Hand,GameState)) -> GameState -> GameState
withHand p f gs = uncurry (players .~) $ foldr (\(n,h) (l,gs') -> if p == n then let (h',gs'') = f h in ((n,h'):l,gs'') else ((n,h):l,gs')) ([],gs) (gs^.players)

draw1 :: PlayerIndex -> GameState -> GameState
draw1 p gs = withHand p (\h ->
     case getCard gs of
        (Just c,gs') -> (c:h,gs')
        (Nothing,gs') -> ([],(pile .~ h ++ gs ^. pile) gs')) gs


-- shuffle :: [Card] -> [Card]
shuffle :: (RandomGen g) => g -> [Card] -> [Card]
shuffle g cs = shuffle' cs (length cs) g -- use shuffleM but everything has to be a monad then? can StateT it?

getCard :: GameState -> (Maybe Card,GameState)
getCard gs = case gs ^. deck of
    (c:cs) -> (Just c,(deck .~ cs) gs)
    [] -> Control.Arrow.second (broadcast "Shuffling pile to deck") (case gs ^. pile of
        (c:cs) -> case shuffle (gs ^. randg) cs of -- should pile always have one card in? , c not shuffled as remains top card of the pile
            (d:ds) -> (Just d,(deck .~ ds) gs)
            [] -> (Nothing,gs))



--only one of the posssible interpretations
--mkR :: (GameState -> GameState) -> Rule
--mkR f act a g = f (act a g) -- f.(act a) -- (.) f (act a) -- (f.).act -- ((f.).) -- (.)((.)f) -- (.).(.)
--mkR = (.).(.)

-- precond: requires at least one player
nextTurn :: Game
nextTurn = const ((players .~) =<< liftM2 (++) tail (return . head) . (^. players))
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

onPlay :: (Card -> Rule) -> Rule
onPlay f act e@(Action p (Play c) m) gs = f c act e gs
onPlay f act e gs = act e gs

onLegalCard :: (Card -> Game) -> Rule
onLegalCard f act e@(Action p (Play c) m) s = let s' = act e s in
    if s' ^. lastMoveLegal then f c e s' else s'
onLegalCard f act a s = act a s
