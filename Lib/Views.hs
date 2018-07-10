{-# LANGUAGE Safe #-}

module Views where

import Control.Arrow (second)
import Control.Lens ((%~),(^.))
import Control.Monad (ap)
import qualified Data.Map as Map (findWithDefault)
import Data.List.NonEmpty (NonEmpty((:|)))

import DataTypes hiding (_hands,_pile,_deck,_messages)

makeViewTransformer :: (GameView -> GameView) -> ViewRule
makeViewTransformer = (.) . (.)

modifyHandMap :: ([(Name,[CardView])]->[(Name,[CardView])]) -> ViewRule
modifyHandMap = makeViewTransformer . (handsV %~)

mapHands :: ([CardView]->[CardView]) -> ViewRule
mapHands f = modifyHandMap (map (second f))

mapOwnHand :: ([CardView]->[CardView]) -> ViewRule
mapOwnHand f = modifyHandMap (\(me:others) -> second f me:others)

mapOtherHands :: ([CardView]->[CardView]) -> ViewRule
mapOtherHands f = modifyHandMap (\(me:others) -> me:map (second f) others)

baseViewer :: Viewer
baseViewer p gs = GV {
    _handsV = -- uncurry (++) -- turn the tuple back into a list
              -- . (map (second (liftM3 if' ((>1).length) (map CardFace) (map (const CardBack))))
              --      *** (map (second (map (const CardBack))) . takeWhile ((/=p).fst)))
              -- . splitAt 1 -- treat the viewer's hand separately (they can see their own hand if not last card)
                -- liftM2 (:) head (takeWhile ((/=p).fst)) . dropWhile ((/=p).fst) . cycle -- starting with the viewer
                uncurry (flip (++)) . span ((/=p).fst) -- put p to the front
              . map (second (map CardFace)) -- turn Cards into CardViews
              . map (ap (,) (flip (Map.findWithDefault []) (gs^.hands))) -- tuple each player with their hand
              $ (gs^.seats) , -- get the players in seating order
    _pileV = (\(c:|cs) -> (CardFace c:map (const CardBack) cs)) (gs^.pile) ,
    _deckV = map (const CardBack) (gs^.deck) ,
    _messagesV = gs^.messages }

-- can not see other players cards
onlyOwnCards :: ViewRule
onlyOwnCards = mapOtherHands$ map (const CardBack)

-- make a card not show if it's your last
lastCard :: ViewRule
lastCard = mapOwnHand$ ap ((if' =<<) ((>1).length)) (map (const CardBack))
-- lastCard = (,) id (((handsV.each._2 %~
--                ap ((if' =<<) ((>1).length)) (map (const CardBack))).).)

defaultView :: Viewer
defaultView = lastCard . onlyOwnCards $ baseViewer
