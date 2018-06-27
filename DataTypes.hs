{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module DataTypes where

import Control.Lens
import Control.Monad --(join,liftM,liftM2)
import Control.Monad.Trans.State
import Data.Char (toLower,isSpace)
import Data.List (isPrefixOf,stripPrefix,delete)
import qualified Data.Map as Map (Map,insert,findWithDefault,empty,fromList)
import Data.Maybe (listToMaybe)
import System.Random

type Parser = StateT String Maybe

next :: (Enum a, Bounded a, Eq a) => a -> a
next a = if a == maxBound then minBound else succ a
prev ::(Enum a, Bounded a, Eq a) =>  a -> a
prev a = if a == minBound then maxBound else pred a

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show,Eq,Enum,Bounded)
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

instance (Enum a, Enum b, Bounded a, Bounded b, Eq a, Eq b) => Enum (a,b) where
  toEnum i = (\(a,b) -> (toEnum b,toEnum a)) $ i `divMod` (1+(fromEnum (maxBound::a) - fromEnum (minBound::a))) -- i `divMod` (fromEnum $ maxBound :: b)
  fromEnum (r,s) = (fromEnum s)*(1+fromEnum (maxBound::a)-fromEnum (minBound::a)) + (fromEnum r)
  enumFrom c = c:(if c==maxBound then [] else enumFrom (succ c))

uniCard :: Card -> Char
uniCard (r,s) = toEnum (0x1F0A0 + (fromEnum (maxBound::Suit) + fromEnum (minBound::Suit) - fromEnum s) * 16 + fromEnum r + 1)


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

type CardIndex = Int

type Name = String
type PlayerIndex = Name
data Action = Draw Int | Play Card deriving Show
data Event = Action PlayerIndex Action String | Timeout


-- I'd call a function playmove or runevent. the problem is that it's used too much
--the type could almost be called Game
type Game = Event -> GameState -> GameState
type Rule = Game -> Game --this type is named correctly

data GameState = GS {
       _players :: [Name], -- current player is head of list
       _hands :: Map.Map Name Hand,
       _deck :: [Card],
       _pile :: [Card],
    --nextPlayer :: Player, --required to be smaller than length hands
       _messages :: [String],
       _lastMoveLegal :: Bool,
       _prevGS :: Maybe (GameState,Action),

       _randg :: StdGen,

       _varMap :: Map.Map String Integer
     } deriving Show
makeLenses ''GameState

readVar :: String -> GameState -> Integer
readVar s gs = Map.findWithDefault 0 s (gs^.varMap)
setVar :: String -> Integer -> GameState -> GameState
setVar s i = varMap %~ Map.insert s i
modifyVar :: String -> (Integer -> Integer) -> GameState -> GameState
modifyVar s f gs = setVar s (f $ readVar s gs) gs

suit :: Card -> Suit
suit = snd

rank :: Card -> Rank
rank = fst

newGame :: [String] -> GameState
newGame pls = GS { _deck = [ minBound.. ]
              , _pile = []
              , _messages = []
              , _lastMoveLegal = True
              , _randg = mkStdGen 0
              , _varMap = Map.empty
              , _players = pls  --[("Angus",[]),("Toby",[]),("Anne",[])]
              , _hands = Map.fromList $ map (flip (,) []) pls
              , _prevGS = Nothing
              }
