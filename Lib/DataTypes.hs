{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DataTypes where

import Control.Lens ((^.),(%~),makeLenses, (%%~),(&),Lens'(..),Lens)
import Control.Monad (ap,liftM2)
import Control.Monad.Trans.State (StateT(StateT),evalStateT,runStateT)
import Data.Char (toLower,isSpace)
import Data.List (isPrefixOf,stripPrefix)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty(NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as Map (empty,findWithDefault,fromList,insert) -- (insert,findWithDefault,empty,fromList)
import Data.Maybe (listToMaybe)
import System.Random (StdGen,mkStdGen,split)
import System.Random.Shuffle (shuffle')

if' :: Bool -> a -> a -> a
if' b a c = if b then a else c

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show,Eq,Enum,Bounded,Ord)
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Knight | Queen | King deriving (Show,Eq,Bounded,Ord)
-- data Rank' = Fool | Individual | Childhood | Youth | Maturity | OldAge | Morning | Afternoon
--            | Evening | Night | EarthAir | WaterFire | Dance | Shopping | OpenAir | VisualArts
--            | Spring | Summer | Autumn | Winter | TheGame | Collective deriving (Show,Eq,Enum,Bounded)
-- data JColour = Red | Black | White deriving (Show,Eq,Enum,Bounded)
--
-- data Suit = Clubs | .. | Trumps | Black | Red | White
-- data Rank = Ace .. King | Joker | Fool ... Collective
-- data Card = SCard Rank Suit | Trump Rank' | Joker JColour deriving (Show,Eq)
type Card = (Rank,Suit)
type Hand = [Card]


--type CardIndex = Int

type Name = String
type PlayerIndex = Name
data Action = Draw Int | Play Card deriving (Show,Eq)

-- | Events that rules should be able to deal with
data Event = Action PlayerIndex Action String | Timeout | PlayerJoin Name deriving (Show,Eq)

type Step = GameState -> GameState

-- I'd call a function playmove or runevent. the problem is that it's used too much
--the type could almost be called Game
-- | This is the type of games (a ruleset, rather than a specific instance of a game).
-- given an event such as a player action, and a game state, it returns the resulting state
type Game = Event -> Step

-- | The type of rules - a rule is added by function composition forming a chain
-- 'such as r3 (r1 (r2 baseAct))' .
-- Most rules should call the first argument under most circumstances.
type Rule = Game -> Game --this type is named correctly
-- Game -> Game == (Event -> GameState -> GameState) -> Event -> GameState -> GameState


-- | The state of a game in play
data GameState = GS {
       _players :: [Name], -- current player is head of list
       _seats :: [Name],
       _hands :: Map Name Hand,
       _deck :: [Card],
       _pile :: NonEmpty Card,
    --nextPlayer :: Player, --required to be smaller than length hands
       _messages :: [String],
       _lastMoveLegal :: Bool,
       _prevGS :: Maybe (GameState,Action),

       _randg :: StdGen,

       _winner :: Maybe Name,

       _varMap :: Map String Int
     } deriving Show
--makeLenses ''GameState
players :: Lens' GameState [Name]
players f gs@GS{_players = p} = (\p' -> gs{_players = p'}) <$> f p
seats :: Lens' GameState [Name]
seats f gs@GS{_seats = s} = (\s' -> gs{_seats = s'}) <$> f s
hands :: Lens' GameState (Map Name Hand)
hands f gs@GS{_hands = h} = (\h' -> gs{_hands = h'}) <$> f h
deck :: Lens' GameState [Card]
deck f gs@GS{_deck = d} = (\d' -> gs{_deck = d'}) <$> f d
pile :: Lens' GameState (NonEmpty Card)
pile f gs@GS{_pile = p} = (\p' -> gs{_pile = p'}) <$> f p
messages :: Lens' GameState [String]
messages f gs@GS{_messages = m} = (\m' -> gs{_messages = m'}) <$> f m
lastMoveLegal :: Lens' GameState Bool
lastMoveLegal f gs@GS{_lastMoveLegal = b} = (\b' -> gs{_lastMoveLegal = b'}) <$> f b
randg :: Lens' GameState StdGen
randg f gs@GS{_randg = r} = (\r' -> gs{_randg = r'}) <$> f r
winner :: Lens' GameState (Maybe Name)
winner f gs@GS{_winner = w} = (\w' -> gs{_winner = w'}) <$> f w
varMap :: Lens' GameState (Map String Int)
varMap f gs@GS{_varMap = v} = (\v' -> gs{_varMap = v'}) <$> f v

-- | A card as viewed - the type of cards sent to the client
data CardView = CardFace Card | CardBack deriving (Show)

-- | The structure describing data seen by players
data GameView = GV {
    _handsV :: [(Name,[CardView])] , -- list is in seating order, beginning with the recipient
    _pileV :: [CardView] ,
    _deckV :: [CardView] ,
    _messagesV :: [String]
} deriving Show
-- makeLenses ''GameView
handsV :: Lens' GameView [(Name,[CardView])]
handsV f gv@GV{_handsV = h} = (\h' -> gv{_handsV = h'}) <$> f h
deckV :: Lens' GameView  [CardView]
deckV f gv@GV{_deckV = d} = (\d' -> gv{_deckV = d'}) <$> f d
pileV :: Lens' GameView  [CardView]
pileV f gv@GV{_pileV = p} = (\p' -> gv{_pileV = p'}) <$> f p
messagesV :: Lens' GameView  [String]
messagesV f gv@GV{_messagesV = m} = (\m' -> gv{_messagesV = m'}) <$> f m

-- | Functions to tell a player what they should see
type Viewer = PlayerIndex -> GameState -> GameView

-- Rules that modify what players see without affecting the game state
type ViewRule = Viewer -> Viewer

-- Complex rules
type Rule' = (Rule,ViewRule)


-- | Card processing functions

-- | Enum instances

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

next :: (Enum a, Bounded a, Eq a) => a -> a
next a = if a == maxBound then minBound else succ a
prev ::(Enum a, Bounded a, Eq a) =>  a -> a
prev a = if a == minBound then maxBound else pred a


instance (Enum a, Enum b, Bounded a, Bounded b, Eq a, Eq b) => Enum (a,b) where
  toEnum i = (\(x,y) -> (toEnum (x + fromEnum (minBound::a)),toEnum (y + fromEnum (minBound::b)))) $ i `divMod` (1+(fromEnum (maxBound::b) - fromEnum (minBound::b))) -- i `divMod` (fromEnum $ maxBound :: b)
  fromEnum (r,s) = (fromEnum r - fromEnum (minBound::a)) * (1+fromEnum (maxBound::b)-fromEnum (minBound::b)) + (fromEnum s - fromEnum (minBound::b))
  enumFrom c = c:(if c==maxBound then [] else enumFrom (succ c))

suitChar :: Suit -> Char
suitChar s = case s of
  Clubs -> 'C'
  Diamonds -> 'D'
  Hearts -> 'H'
  Spades -> 'S'

suit :: Card -> Suit
suit = snd

rank :: Card -> Rank
rank = fst

rankChar :: Rank -> Char
rankChar r = (['A'] ++ [head $ show i | i <- [2..9]::[Int] ] ++ ['T','J','C','Q','K'])!!(fromEnum r - 1) -- UNSAFE

-- | Get the unicode playing card character corresponding to some card
uniCard :: Card -> Char
uniCard (r,s) = toEnum (0x1F0A0 + (fromEnum (maxBound::Suit) + fromEnum (minBound::Suit) - fromEnum s) * 16 + fromEnum r)

--reading cards

type Parser = StateT String Maybe
-- | Given 2 parsers, tries the first, if it fails, try the second
(<|>) :: Parser a -> Parser a -> Parser a
a <|> b = StateT (\s -> case runStateT a s of
    Just (x,s') -> Just (x,s')
    Nothing -> runStateT b s)--note that state is saved - Parsec does not do this for efficiency
runParser :: Parser a -> String -> Maybe a
runParser = evalStateT


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

eventPlayer :: Event -> Maybe PlayerIndex
eventPlayer (Action p _ _) = Just p
eventPlayer (PlayerJoin p) = Just p
eventPlayer Timeout = Nothing

-- | variable processing

readVar :: String -> GameState -> Int
readVar s gs = Map.findWithDefault 0 s (gs^.varMap)
setVar :: String -> Int -> Step
setVar s i = varMap %~ Map.insert s i
modifyVar :: String -> (Int -> Int) -> Step
modifyVar s f gs = setVar s (f $ readVar s gs) gs

shuffleDeck :: Step
shuffleDeck = uncurry ((deck%~).flip (ap shuffle' length)) . (randg %%~ split)
-- shuffleDeck = (deck /\ randg) %~ ap ((`ap` snd) . ((,) .) . (. fst) . liftM2 shuffle' fst (length . fst)) (split . snd)
-- shuffleDeck = (deck /\ randg) %~ (\(d,r) -> let (r1,r2) = split r in (shuffle' d (length d) r1,r2))

newGame :: [String] -> GameState
newGame pls =  ((pile /\ deck) %~ (\(_,y:ys) -> (y:|[],ys))) . shuffleDeck $ -- UNSAFE
           GS { _deck = [ minBound.. ]
              , _pile = undefined
              , _messages = []
              , _lastMoveLegal = True
              , _randg = mkStdGen 0
              , _varMap = Map.empty
              , _players = pls  --[("Angus",[]),("Toby",[]),("Anne",[])]
              , _seats = pls
              , _hands = Map.fromList $ map (flip (,) []) (pls)
              , _prevGS = Nothing
              , _winner = Nothing
              }


-- Thanks stack overflow
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
