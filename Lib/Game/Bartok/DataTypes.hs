{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : DataTypes
-- Description : Data types used for dealing with games
--
-- Datatypes for cards, game state and game view alongside some basic functions to process them.
module Game.Bartok.DataTypes
  ( -- * Basic Rules
    Step,
    Game,
    Rule,

    -- ** Structures
    Name,
    Action (..),
    Event (..),
    GameState (..),
    newGame,

    -- ** Cards
    Hand,
    Card,
    Suit (..),
    Rank (..),
    suit,
    rank,

    -- ** Variables
    -- Game states may store named variables that can be read and written to by rules
    VarName,
    setVar,
    readVar,
    modifyVar,

    -- ** other functions
    shuffleDeck,
    eventPlayer,

    -- * View Rules
    Viewer,
    ViewRule,
    CardView (..),

    -- ** Structure
    GameView (..),

    -- * Combined Rules
    Rule',

    -- * Cards

    -- ** Printing
    uniCard,

    -- ** Parsing
    -- these don't matter too much
    Parser,
    runParser,
    parseSuit,
    parseRank,
    parseCard,

    -- * Other types used
    NonEmpty ((:|)),
    Map,
    StdGen,
    randomR,
    random,

    -- * Angus needs to get rid of these
    if',
    (/\),

    -- ** lenses
    players,
    hands,
    deck,
    pile,
    messages,
    lastMoveLegal,
    randg,
    winner,
    varMap,
    handsV,
    pileV,
    deckV,
    messagesV,
  )
where

import Control.Lens (Lens, Lens' (..), makeLenses, (%%~), (%~), (&), (^.))
import Control.Monad (ap, liftM2)
import Control.Monad.Trans.State (StateT (StateT), evalStateT, runStateT)
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf, stripPrefix)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map (empty, findWithDefault, fromList, insert, map) -- (insert,findWithDefault,empty,fromList)
import Data.Maybe (listToMaybe)
import System.Random (StdGen, mkStdGen, random, randomR, split)
import System.Random.Shuffle (shuffle')

if' :: Bool -> a -> a -> a
if' b a c = if b then a else c

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq, Enum, Bounded, Ord)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Knight | Queen | King deriving (Show, Eq, Bounded, Ord)

-- data Rank' = Fool | Individual | Childhood | Youth | Maturity | OldAge | Morning | Afternoon
--            | Evening | Night | EarthAir | WaterFire | Dance | Shopping | OpenAir | VisualArts
--            | Spring | Summer | Autumn | Winter | TheGame | Collective deriving (Show,Eq,Enum,Bounded)
-- data JColour = Red | Black | White deriving (Show,Eq,Enum,Bounded)
--
-- data Suit = Clubs | .. | Trumps | Black | Red | White
-- data Rank = Ace .. King | Joker | Fool ... Collective
-- data Card = SCard Rank Suit | Trump Rank' | Joker JColour deriving (Show,Eq)

-- | A single playing card
type Card = (Rank, Suit)

-- | An ordered collection of cards forming a player's hand
type Hand = [Card]

-- | Names of players.
type Name = String

-- | At any point, a player can attempt to take one of these actions
data Action = Draw Int | Play Card deriving (Show, Eq)

-- | Events that rules should be able to deal with
data Event
  = -- | A player leaving the game
    PlayerLeave Name
  | -- | A player requesting to join the game. This is not limited to the start
    PlayerJoin Name (Maybe (Name, Name))
  | -- | A player performing one of the above actions and sending a message.
    Action Name Action String
  | -- | If no player has made an action for 10 seconds, this event will be sent
    Timeout
  deriving (Show, Eq)

-- | The type of transformations of game state
type Step = GameState -> GameState

-- I'd call a function playmove or runevent. the problem is that it's used more than once

-- | This is the type of games (meaning a ruleset, rather than a specific instance of a game).
-- Given an event such as a player action, and a game state, it returns the resulting state
type Game = Event -> Step

-- | The type of rules - a rule is added by function composition forming a chain
-- such as 'r3 (r1 (r2 baseAct))' .
-- Most rules should call the first argument under most circumstances and usually without changing the event.
type Rule = Game -> Game
-- ^ Game -> Game == (Event -> GameState -> GameState) -> Event -> GameState -> GameState

-- \ Identifiers for variables
type VarName = String

--TODO: make the documentation true (messages and lastMoveLegal)

-- | The state of a game in play
data GameState = GS
  { -- | The players curretly in the game. The player whose turn it is should be at head of list
    -- and usually this advances forward by 1 each turn.
    _players :: [Name],
    -- | Stores the contents of each player's hand
    _hands :: Map Name Hand,
    -- | The deck from which cards are drawn
    _deck :: [Card],
    -- | The cards that have been played -
    _pile :: NonEmpty Card,
    -- | The messages that .  Contains only those generated by the most recent event.
    _messages :: [String],
    -- | Indicates if the last move was successful.
    -- This should be true
    -- When a new event happens, this is False until baseAct is called.
    -- _prevGS :: Maybe (GameState,Action),
    _lastMoveLegal :: Bool,
    -- | A seeded random number generator so that you can
    _randg :: StdGen,
    -- | @Nothing@ until a player p wins at which point it becomes @Just p@
    _winner :: Maybe Name,
    -- | A store of named variables that rules may use to keep track of state between events.
    _varMap :: Map VarName Int
  }
  deriving (Show)

makeLenses ''GameState

-- players :: Lens' GameState [Name]
-- players f gs@GS{_players = p} = (\p' -> gs{_players = p'}) <$> f p
-- seats :: Lens' GameState [Name]
-- seats f gs@GS{_seats = s} = (\s' -> gs{_seats = s'}) <$> f s
-- hands :: Lens' GameState (Map Name Hand)
-- hands f gs@GS{_hands = h} = (\h' -> gs{_hands = h'}) <$> f h
-- deck :: Lens' GameState [Card]
-- deck f gs@GS{_deck = d} = (\d' -> gs{_deck = d'}) <$> f d
-- pile :: Lens' GameState (NonEmpty Card)
-- pile f gs@GS{_pile = p} = (\p' -> gs{_pile = p'}) <$> f p
-- messages :: Lens' GameState [String]
-- messages f gs@GS{_messages = m} = (\m' -> gs{_messages = m'}) <$> f m
-- lastMoveLegal :: Lens' GameState Bool
-- lastMoveLegal f gs@GS{_lastMoveLegal = b} = (\b' -> gs{_lastMoveLegal = b'}) <$> f b
-- randg :: Lens' GameState StdGen
-- randg f gs@GS{_randg = r} = (\r' -> gs{_randg = r'}) <$> f r
-- winner :: Lens' GameState (Maybe Name)
-- winner f gs@GS{_winner = w} = (\w' -> gs{_winner = w'}) <$> f w
-- varMap :: Lens' GameState (Map String Int)
-- varMap f gs@GS{_varMap = v} = (\v' -> gs{_varMap = v'}) <$> f v

-- | A card as viewed - the type of cards sent to the client
data CardView = CardFace Card | CardBack deriving (Show)

-- | The structure describing data seen by players
data GameView = GV
  { -- | The players and the information a particular player will have about each hand.
    -- By default, this is a 'CardFace's for the viewing player and a number of 'CardBack's for others.
    _handsV :: [(Name, [CardView])],
    -- | What should be seen of the pile. By default, only the top card is visible.
    _pileV :: [CardView],
    -- | What should be seen of the pile. By default, none are visible.
    _deckV :: [CardView],
    -- | The messages a player can see. By default, this is all messages that have been sent
    _messagesV :: [String]
  }
  deriving (Show)

makeLenses ''GameView

-- handsV :: Lens' GameView [(Name,[CardView])]
-- handsV f gv@GV{_handsV = h} = (\h' -> gv{_handsV = h'}) <$> f h
-- deckV :: Lens' GameView  [CardView]
-- deckV f gv@GV{_deckV = d} = (\d' -> gv{_deckV = d'}) <$> f d
-- pileV :: Lens' GameView  [CardView]
-- pileV f gv@GV{_pileV = p} = (\p' -> gv{_pileV = p'}) <$> f p
-- messagesV :: Lens' GameView  [String]
-- messagesV f gv@GV{_messagesV = m} = (\m' -> gv{_messagesV = m'}) <$> f m

-- | Functions to tell a player what they should see
type Viewer = Name -> GameState -> GameView

-- | Rules that modify what players see without affecting the game state
type ViewRule = Viewer -> Viewer

-- | Complex rules
type Rule' = (Rule, ViewRule)

-- Enum instances

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
  enumFrom n = map toEnum [fromEnum n .. fromEnum (maxBound :: Rank)]

next :: (Enum a, Bounded a, Eq a) => a -> a
next a = if a == maxBound then minBound else succ a

prev :: (Enum a, Bounded a, Eq a) => a -> a
prev a = if a == minBound then maxBound else pred a

instance (Enum a, Enum b, Bounded a, Bounded b, Eq a, Eq b) => Enum (a, b) where
  toEnum i = (\(x, y) -> (toEnum (x + fromEnum (minBound :: a)), toEnum (y + fromEnum (minBound :: b)))) $ i `divMod` (1 + (fromEnum (maxBound :: b) - fromEnum (minBound :: b))) -- i `divMod` (fromEnum $ maxBound :: b)
  fromEnum (r, s) = (fromEnum r - fromEnum (minBound :: a)) * (1 + fromEnum (maxBound :: b) - fromEnum (minBound :: b)) + (fromEnum s - fromEnum (minBound :: b))
  enumFrom c = c : (if c == maxBound then [] else enumFrom (succ c))

suitChar :: Suit -> Char
suitChar s = case s of
  Clubs -> 'C'
  Diamonds -> 'D'
  Hearts -> 'H'
  Spades -> 'S'

-- | Get the suit of a card.
suit :: Card -> Suit
suit = snd

-- | Get the rank of a card.
rank :: Card -> Rank
rank = fst

-- | One character corresponding to the rank (A1..9TJCQK)
rankChar :: Rank -> Char
rankChar r = (['A'] ++ [head $ show i | i <- [2 .. 9] :: [Int]] ++ ['T', 'J', 'C', 'Q', 'K']) !! (fromEnum r - 1) -- UNSAFE

-- | Get the unicode playing card character corresponding to some card
uniCard :: Card -> Char
uniCard (r, s) = toEnum (0x1F0A0 + (fromEnum (maxBound :: Suit) + fromEnum (minBound :: Suit) - fromEnum s) * 16 + fromEnum r)

--reading cards

type Parser = StateT String Maybe

-- | Given 2 parsers, tries the first, if it fails, try the second
(<|>) :: Parser a -> Parser a -> Parser a
a <|> b =
  StateT
    ( \s -> case runStateT a s of
        Just (x, s') -> Just (x, s')
        Nothing -> runStateT b s --note that state is saved - Parsec does not do this for efficiency
    )

runParser :: Parser a -> String -> Maybe a
runParser = evalStateT

parseRank :: Parser Rank
parseRank =
  StateT
    ( \s ->
        fmap (\(r, c) -> (r, drop (length c) s)) $ listToMaybe $
          filter
            (\(_, c) -> isPrefixOf (map toLower c) (map toLower s))
            (map (\x -> (x, show x)) (enumFrom minBound) ++ map (\x -> (x, ((: []) . rankChar) x)) (enumFrom minBound))
    )

parseSuit :: Parser Suit
parseSuit =
  StateT
    ( \s ->
        fmap (\(r, c) -> (r, drop (length c) s)) $ listToMaybe $
          filter
            (\(_, c) -> isPrefixOf (map toLower c) (map toLower s))
            (map (\x -> (x, show x)) (enumFrom minBound) ++ map (\x -> (x, ((: []) . suitChar) x)) (enumFrom minBound))
    )

ignore :: String -> Parser ()
ignore s =
  StateT
    ( \s' ->
        let s'' = dropWhile isSpace s'
         in let s''' = stripPrefix s s''
             in fmap ((,) () . dropWhile isSpace) s'''
    )

parseCard :: Parser Card
parseCard = do
  r <- parseRank
  ignore "of" -- and possibly spaces either side
  s <- parseSuit
  return (r, s)

-- | Get the player of an event
eventPlayer :: Event -> Maybe Name
eventPlayer (Action p _ _) = Just p
eventPlayer (PlayerJoin p _) = Just p
eventPlayer Timeout = Nothing

-- | variable processing
readVar :: VarName -> GameState -> Int
readVar s gs = Map.findWithDefault 0 s (gs ^. varMap)

setVar :: VarName -> Int -> Step
setVar s i = varMap %~ Map.insert s i

modifyVar :: VarName -> (Int -> Int) -> Step
modifyVar s f gs = setVar s (f $ readVar s gs) gs

-- | Shuffle the deck (does not touch the pile or hands) using the random seed contained in GameState.
shuffleDeck :: Step
shuffleDeck = uncurry ((deck %~) . flip (ap shuffle' length)) . (randg %%~ split)

-- shuffleDeck = (deck /\ randg) %~ ap ((`ap` snd) . ((,) .) . (. fst) . liftM2 shuffle' fst (length . fst)) (split . snd)
-- shuffleDeck = (deck /\ randg) %~ (\(d,r) -> let (r1,r2) = split r in (shuffle' d (length d) r1,r2))

-- | Construct a new game from a list of player names.
newGame :: [String] -> GameState
newGame pls =
  ((pile /\ deck) %~ (\(_, y : ys) -> (y :| [], ys))) . shuffleDeck $ -- UNSAFE
    GS
      { _deck = [minBound ..],
        _pile = undefined,
        _messages = [],
        _lastMoveLegal = True,
        _randg = mkStdGen 0,
        _varMap = Map.empty,
        _players = pls, --[("Angus",[]),("Toby",[]),("Anne",[])]
        -- , _seats = pls
        _hands = Map.fromList $ map (flip (,) []) (pls),
        --, _prevGS = Nothing
        _winner = Nothing
      }

-- | Make a new game with no players given a seeded rng
randGame :: StdGen -> GameState
randGame rng =
  ((pile /\ deck) %~ (\(_, y : ys) -> (y :| [], ys))) . shuffleDeck $ -- UNSAFE
    GS
      { _deck = [minBound ..],
        _pile = undefined,
        _messages = [],
        _lastMoveLegal = True,
        _randg = rng,
        _varMap = Map.empty,
        _players = [], --[("Angus",[]),("Toby",[]),("Anne",[])]
        -- , _seats = pls
        _hands = Map.empty,
        --, _prevGS = Nothing
        _winner = Nothing
      }

-- Thanks stack overflow
(/\) ::
  (Functor f) =>
  -- | Lens' c a
  ((a -> (a, a)) -> (c -> (a, c))) ->
  -- | Lens' c b
  ((b -> (b, b)) -> (c -> (b, c))) ->
  -- | Lens' c (a, b)
  (((a, b) -> f (a, b)) -> (c -> f c))
(lens1 /\ lens2) f c0 =
  let (a, _) = lens1 (\a_ -> (a_, a_)) c0
      (b, _) = lens2 (\b_ -> (b_, b_)) c0
      fab = f (a, b)
   in fmap
        ( \(a, b) ->
            let (_, c1) = lens1 (\a_ -> (a_, a)) c0
                (_, c2) = lens2 (\b_ -> (b_, b)) c1
             in c2
        )
        fab

infixl 7 /\
