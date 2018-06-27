module Lib where

import Control.Monad.Trans.State
--import Control.Monad.Trans.Class
import Data.List
import Data.Maybe
import Data.Char

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


-- | Given 2 parsers, tries the first, if it fails, try the second
(<|>) :: Parser a -> Parser a -> Parser a
a <|> b = StateT (\s -> case runStateT a s of
    Just (x,s') -> Just (x,s')
    Nothing -> runStateT b s)--note that state is saved - Parsec does not do this for efficiency


parseRank :: Parser Rank
parseRank = StateT (\s ->
  fmap (\(r,c) -> (r,drop (length c) s)) $ listToMaybe $
    filter (\(_,c) -> isPrefixOf (map toLower c) (map toLower s))
      (map (\x -> (x,show x)) (enumFrom minBound) ++ zip (enumFrom minBound) (map ((:[]).rankChar) $ enumFrom minBound)))

parseSuit :: Parser Suit
parseSuit = StateT (\s ->
  fmap (\(r,c) -> (r,drop (length c) s)) $ listToMaybe $
    filter (\(_,c) -> isPrefixOf (map toLower c) (map toLower s))
      (map (\x -> (x,show x)) (enumFrom minBound) ++ zip (enumFrom minBound) (map ((:[]).suitChar) $ enumFrom minBound)))

ignore :: String -> Parser ()
ignore s = StateT (\s -> undefined )

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
type Rule = Game -> Game --this type is named correctlt

data GameState = GS {
    players :: [(Name,[Card])], -- first player is head of the list
    deck :: [Card],
    pile :: [Card],
    --nextPlayer :: Player, --required to be smaller than length hands
    messages :: [String],
    lastMoveLegal :: Bool,
    prevGS :: Maybe (GameState,Action)
}

hands :: GameState -> [Hand]
hands gs = map snd (players gs)


suit :: Card -> Suit
suit = snd

rank :: Card -> Rank
rank = fst



--type TState = GameState -> GameState (better to just write GameState->GameState everywhere)



baseAct :: Game
baseAct (Action p (Draw n) m) = broadcast m . draw n p
--baseAct (Action (p,Play i,m)) g = broadcast m .

broadcast :: String -> GameState -> GameState
broadcast m gs = gs{messages = m:messages gs}

draw :: Int -> PlayerIndex -> GameState -> GameState
draw n p = foldl (.) id (replicate n (draw1 p))


getHand :: PlayerIndex -> GameState -> Maybe Hand
getHand = undefined

{-
fromHand :: CardIndex -> Hand -> Maybe Card
--fromHand i h = if i>=0 and i<length h then Just h!!i else Nothing
fromHand 0 (x:xs) = Just x
fromHand n (x:xs) = fromHand (n-1) xs
-}

--does nothing if player is invalid
withHand :: PlayerIndex ->(Hand -> (Hand,GameState))-> GameState -> GameState
withHand p f = undefined

draw1 :: PlayerIndex -> GameState -> GameState
draw1 p gs = withHand p (\h ->
     case getCard gs of
        (Just c,gs') -> (c:h,gs)
        (Nothing,gs') -> ([],gs'{pile=h++pile gs'})) gs


shuffle :: [Card] -> [Card]
shuffle = id

getCard :: GameState -> (Maybe Card,GameState)
getCard gs = case deck gs of
    (c:cs) -> (Just c,gs{deck = cs})
    [] -> (\(c,gs') -> (c,broadcast "shuffling" gs')) (case pile gs of
        (c:cs) -> case shuffle cs of-- should pile always have one card in?
            (d:ds) -> (Just d,gs{deck = ds})
            [] -> (Nothing,gs))



--only one of the posssible interpretations
--mkR :: (GameState -> GameState) -> Rule
--mkR f act a g = f (act a g) -- f.(act a) -- (.) f (act a) -- (f.).act -- ((f.).) -- (.)((.)f) -- (.).(.)
--mkR = (.).(.)

nextTurn :: Game
nextTurn _ g@GS {players = (p:ps)} = g{players=ps++[p]}
    -- g{nextPlayer = ((nextPlayer g+1) `mod` length (hands g))}

when :: (a -> Bool) -> Game -> a -> Game
when q act x = if q x then act else const id


with :: (GameState -> a) -> (a -> Rule) -> Rule
with = undefined


penalty :: String -> PlayerIndex -> GameState -> GameState
penalty s p = broadcast ("penalty :"++p++s) . draw1 p .(\gs -> gs{lastMoveLegal = False})

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
    if lastMoveLegal s' then f c e s' else s'
onLegalCard f act a s = act a s
