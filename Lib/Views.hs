{-# LANGUAGE Safe #-}

module Views where

import Control.Arrow (second)
import Control.Lens ((%~),(^.))
import Control.Monad (ap,join)
import qualified Data.Map as Map (adjust,findWithDefault,map,mapWithKey)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty((:|)))

import DataTypes hiding (_hands,_pile,_deck,_messages)

makeViewTransformer :: (GameView -> GameView) -> ViewRule
makeViewTransformer = (.) . (.)

modifyHandMap :: (Map Name [CardView] -> Map Name [CardView]) -> ViewRule
modifyHandMap = makeViewTransformer . (handsV %~)

mapHands :: ([CardView]->[CardView]) -> ViewRule
mapHands f = modifyHandMap (Map.map f)

mapOwnHand :: PlayerIndex -> ([CardView]->[CardView]) -> ViewRule
mapOwnHand p f = modifyHandMap (Map.adjust f p)

mapOtherHands :: PlayerIndex -> ([CardView]->[CardView]) -> ViewRule
mapOtherHands p f = modifyHandMap (Map.mapWithKey (join . (. f) . if' . (p /=)))

baseViewer :: Viewer
baseViewer p gs = GV {
    _handsV = -- uncurry (++) -- turn the tuple back into a list
              -- . (map (second (liftM3 if' ((>1).length) (map CardFace) (map (const CardBack))))
              --      *** (map (second (map (const CardBack))) . takeWhile ((/=p).fst)))
              -- . splitAt 1 -- treat the viewer's hand separately (they can see their own hand if not last card)
                -- liftM2 (:) head (takeWhile ((/=p).fst)) . dropWhile ((/=p).fst) . cycle -- starting with the viewer
                -- uncurry (flip (++)) . span ((/=p).fst) -- put p to the front
                Map.map (map CardFace) (gs^.hands) ,
    _pileV = (\(c:|cs) -> (CardFace c:map (const CardBack) cs)) (gs^.pile) ,
    _deckV = map (const CardBack) (gs^.deck) ,
    _messagesV = gs^.messages }

-- can not see other players cards
onlyOwnCards :: ViewRule
onlyOwnCards v p = mapOtherHands p (map (const CardBack)) v p

-- make a card not show if it's your last
lastCard :: ViewRule
lastCard v p = mapOwnHand p (ap ((if' =<<) ((>1).length)) (map (const CardBack))) v p
-- lastCard = (,) id (((handsV.each._2 %~
--                ap ((if' =<<) ((>1).length)) (map (const CardBack))).).)

defaultView :: Viewer
defaultView = lastCard . onlyOwnCards $ baseViewer
