{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module DataTypes where

import Control.Lens
import Control.Monad --(join,liftM,liftM2)
import Control.Monad.Trans.State
import Data.Char (toLower,isSpace)
import Data.List (isPrefixOf,stripPrefix,delete)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.Map as Map (Map,insert,findWithDefault,empty,fromList)
import Data.Maybe (listToMaybe)
import System.Random
import System.Random.Shuffle (shuffle')

type Parser = StateT String Maybe

next :: (Enum a, Bounded a, Eq a) => a -> a
next a = if a == maxBound then minBound else succ a
prev ::(Enum a, Bounded a, Eq a) =>  a -> a
prev a = if a == minBound then maxBound else pred a

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show,Eq,Enum,Bounded,Ord)
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Knight | Queen | King deriving (Show,Eq,Bounded,Ord)


type Card = (Rank,Suit)
type Hand = [Card]


type CardIndex = Int

type Name = String
type PlayerIndex = Name
data Action = Draw Int | Play Card deriving (Show,Eq)
data Event = Action PlayerIndex Action String | Timeout deriving (Show,Eq)


-- I'd call a function playmove or runevent. the problem is that it's used too much
--the type could almost be called Game
type Game = Event -> GameState -> GameState
type Rule = Game -> Game --this type is named correctly

data GameState = GS {
       _players :: NonEmpty Name, -- current player is head of list
       _hands :: Map.Map Name Hand,
       _deck :: [Card],
       _pile :: NonEmpty Card,
    --nextPlayer :: Player, --required to be smaller than length hands
       _messages :: [String],
       _lastMoveLegal :: Bool,
       _prevGS :: Maybe (GameState,Action),

       _randg :: StdGen,

       _varMap :: Map.Map String Int
     } deriving Show
makeLenses ''GameState



-- | Card processing functions

suit :: Card -> Suit
suit = snd

rank :: Card -> Rank
rank = fst

instance Enum Rank where
  toEnum i = case i of
    1 -> Ace
    2 -> Two
    3 -> Three
    4 -> Four
    5 -> Five
    6 -> Six
    7 -> Seven
    8 -> Eight
    9 -> Nine
    10 -> Ten
    11 -> Jack
    12 -> Knight
    13 -> Queen
    14 -> King
  fromEnum r = case r of
    Ace -> 1
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9
    Ten -> 10
    Jack -> 11
    Knight -> 12
    Queen -> 13
    King -> 14
  enumFrom n = map toEnum [fromEnum n..fromEnum (maxBound::Rank)]

suitChar :: Suit -> Char
suitChar s = case s of
  Clubs -> 'C'
  Diamonds -> 'D'
  Hearts -> 'H'
  Spades -> 'S'


rankChar :: Rank -> Char
rankChar r = (['A'] ++ [head $ show i | i <- [2..9]::[Int] ] ++ ['T','J','C','Q','K'])!!(fromEnum r - 1) -- UNSAFE

instance (Enum a, Enum b, Bounded a, Bounded b, Eq a, Eq b) => Enum (a,b) where
  toEnum i = (\(x,y) -> (toEnum (x + fromEnum (minBound::a)),toEnum (y + fromEnum (minBound::b)))) $ i `divMod` (1+(fromEnum (maxBound::b) - fromEnum (minBound::b))) -- i `divMod` (fromEnum $ maxBound :: b)
  fromEnum (r,s) = (fromEnum r - fromEnum (minBound::a)) * (1+fromEnum (maxBound::b)-fromEnum (minBound::b)) + (fromEnum s - fromEnum (minBound::b))
  enumFrom c = c:(if c==maxBound then [] else enumFrom (succ c))

uniCard :: Card -> Char
uniCard (r,s) = toEnum (0x1F0A0 + (fromEnum (maxBound::Suit) + fromEnum (minBound::Suit) - fromEnum s) * 16 + fromEnum r)


-- | Given 2 parsers, tries the first, if it fails, try the second
(<|>) :: Parser a -> Parser a -> Parser a
a <|> b = StateT (\s -> case runStateT a s of
    Just (x,s') -> Just (x,s')
    Nothing -> runStateT b s)--note that state is saved - Parsec does not do this for efficiency


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
  ignore "of" -- and possibly spaces either side
  s <- parseSuit
  return (r,s)


-- | variable processing

readVar :: String -> GameState -> Int
readVar s gs = Map.findWithDefault 0 s (gs^.varMap)
setVar :: String -> Int -> GameState -> GameState
setVar s i = varMap %~ Map.insert s i
modifyVar :: String -> (Int -> Int) -> GameState -> GameState
modifyVar s f gs = setVar s (f $ readVar s gs) gs

shuffleDeck :: GameState -> GameState
shuffleDeck = (deck /\ randg) %~ ap ((`ap` snd) . ((,) .) . (. fst) . liftM2 shuffle' fst (length . fst)) (split . snd)
-- shuffleDeck = (deck /\ randg) %~ (\(d,r) -> let (r1,r2) = split r in (shuffle' d (length d) r1,r2))

newGame :: NonEmpty String -> GameState
newGame pls =  ((pile /\ deck) %~ (\(_,y:ys) -> (y:|[],ys))) . shuffleDeck $ -- UNSAFE
           GS { _deck = [ minBound.. ]
              , _pile = undefined -- put a card so it's happy for now
              , _messages = []
              , _lastMoveLegal = True
              , _randg = mkStdGen 0
              , _varMap = Map.empty
              , _players = pls  --[("Angus",[]),("Toby",[]),("Anne",[])]
              , _hands = Map.fromList $ map (flip (,) []) (NE.toList pls)
              , _prevGS = Nothing
              }

(/\)
    :: (Functor f)
    => ((a -> (a, a)) -> (c -> (a, c)))
    -- ^ Lens' c a
    -> ((b -> (b, b)) -> (c -> (b, c)))
    -- ^ Lens' c b
    -> (((a, b) -> f (a, b)) -> (c -> f c))
    -- ^ Lens' c (a, b)
(lens1 /\ lens2) f c0 =
    let (a, _) = lens1 (\a_ -> (a_, a_)) c0
        (b, _) = lens2 (\b_ -> (b_, b_)) c0
        fab = f (a, b)
    in fmap (\(a, b) ->
            let (_, c1) = lens1 (\a_ -> (a_, a)) c0
                (_, c2) = lens2 (\b_ -> (b_, b)) c1
            in c2
            ) fab

infixl 7 /\