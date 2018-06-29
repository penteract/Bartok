{-# LANGUAGE TemplateHaskell #-}

module Views where

import DataTypes hiding (_hands,_pile,_deck,_messages)
import Lib
import Control.Lens
import qualified Data.Map as Map (toList)
import Data.Map (Map,toList,findWithDefault)
import Data.List.NonEmpty (NonEmpty(..))
import Control.Arrow (first,second,(***))
import Control.Monad (ap)

data CardView = CardFace Card | CardBack

data GameView = GV {
    _handsV :: [(Name,[CardView])] , -- list is in seating order, beginning with the recipient
    _pileV :: [CardView] ,
    _deckV :: [CardView] ,
    _messagesV :: [String]
}
makeLenses ''GameView

defaultView :: PlayerIndex -> GameState -> GameView
defaultView p gs = GV {
    _handsV = uncurry (++) -- turn the tuple back into a list
              . (map (second (map CardFace)) *** (map (second (map (const CardBack))) . takeWhile ((/=p).fst)))
              . splitAt 1 -- treat the viewer's hand separately (they can see their own hand)
              . dropWhile ((/=p).fst) . cycle -- starting with the viewer
              . map (ap (,) (flip (findWithDefault []) (gs^.hands))) -- tuple each player with their hand
              $ (gs^.seats) , -- get the players in seating order
    _pileV = (\(c:|cs) -> (CardFace c:map (const CardBack) cs)) (gs^.pile) ,
    _deckV = map (const CardBack) (gs^.deck) ,
    _messagesV = gs^.messages
}
