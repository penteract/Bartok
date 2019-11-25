{-|
Module      : Views
Description : How the game decides what to send to the client

The functions in this module define how the data sent to clients is constructed.
They should help when writing rules that affect what players can see.
-}
module Game.Bartok.Views(-- * Type Synonyms
Viewer,ViewRule,
-- * Basic Rules
baseViewer,GameView(..),
-- *
lastCard,onlyOwnCards,
defaultView,
-- * Helper functions
makeViewTransformer,
-- ** dealing with hands
mapOwnHand,mapOtherHands, modifyHandMap, mapHands,
hideHand,
) where

import Control.Arrow (second)
import Control.Lens ((%~),(^.))
import Control.Monad
import qualified Data.Map as Map --(adjust,assocs,findWithDefault,map,mapWithKey)
--import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty((:|)))

import Game.Bartok.DataTypes hiding (_hands,_pile,_deck,_messages)

-- | Turn a function transforming a 'GameView' into a viewRule
-- > makeViewTransformer f r p gs == f$ r p gs
makeViewTransformer :: (GameView -> GameView) -> ViewRule
makeViewTransformer = (.) . (.)
--makeViewTransformer f r p gs = f$ r p gs


-- | Apply a modification to the structure describing what can be seen of each player's hand after inner rules have been applied.
modifyHandMap :: ([(Name,[CardView])] -> [(Name,[CardView])]) -> ViewRule
modifyHandMap = makeViewTransformer . (handsV %~)


-- | Apply a function to all views of hands
mapHands :: ([CardView]->[CardView]) -> ViewRule
mapHands f = modifyHandMap (map (second f))
--mapHands f = modifyHandMap (Map.map f)

-- Modify the view of a particular player's hand
-- mapOwnHand :: Name -> ([CardView]->[CardView]) -> ViewRule
-- mapOwnHand p f = modifyHandMap (map (join (ap (if'.(p==).fst) (second f)))) -- (\l -> [if x == p then (x,f y) else (x,y) | (x,y) <- l])
--mapOwnHand p f = modifyHandMap (Map.adjust f p)

-- | Modify a player's view of their own hand
mapOwnHand :: ([CardView]->[CardView]) -> ViewRule
--mapOwnHand f r p = modifyHandMap (map (join (ap (if'.(p==).fst) (second f)))) r p
mapOwnHand f r p = modifyHandMap (\l -> [if p' == p then (p',f h) else (p',h) | (p',h) <- l]) r p

-- Modify the view of all player's hands
-- mapOtherHands :: Name -> ([CardView]->[CardView]) -> ViewRule
-- mapOtherHands p f = modifyHandMap (map (join (ap (if'.(p/=).fst) (second f))))

-- | Modify the view of all other players' hands
mapOtherHands :: ([CardView]->[CardView]) -> ViewRule
--mapOtherHands f r p = modifyHandMap (map (join (ap (if'.(p/=).fst) (second f)))) r p
mapOtherHands f r p = modifyHandMap (\l -> [if p' /= p then (p',f h) else (p',h) | (p',h) <- l]) r p
-- | Like 'baseGame', this defines how cards should be viewed in a very simple game. See 'DataTypes.GameView' for more.
baseViewer :: Viewer
baseViewer _ gs = GV {
    _handsV = -- uncurry (++) -- turn the tuple back into a list
              -- . (map (second (liftM3 if' ((>1).length) (map CardFace) (map (const CardBack))))
              --      *** (map (second (map (const CardBack))) . takeWhile ((/=p).fst)))
              -- . splitAt 1 -- treat the viewer's hand separately (they can see their own hand if not last card)
                -- liftM2 (:) head (takeWhile ((/=p).fst)) . dropWhile ((/=p).fst) . cycle -- starting with the viewer
                -- uncurry (flip (++)) . span ((/=p).fst) -- put p to the front
                map (second $ map CardFace) (Map.assocs $ gs^.hands) ,
    _pileV = (\(c:|cs) -> CardFace c:map (const CardBack) cs) (gs^.pile) ,
    _deckV = map (const CardBack) (gs^.deck) ,
    _messagesV = gs^.messages }

hideHand :: [CardView] -> [CardView]
hideHand = map (const CardBack)

-- | Hides other players' hands.
onlyOwnCards :: ViewRule
onlyOwnCards = mapOtherHands hideHand

-- | Hide the last card in a player's hand.
lastCard :: ViewRule
lastCard = mapOwnHand (ap ((if' =<<) ((>1).length)) hideHand)
-- lastCard = (,) id (((handsV.each._2 %~
--                ap ((if' =<<) ((>1).length)) (map (const CardBack))).).)

-- | 'baseView' with the above rules added to it.
defaultView :: Viewer
defaultView = lastCard . onlyOwnCards $ baseViewer
