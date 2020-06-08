{-# LANGUAGE RankNTypes #-}

module Utils
  ( applyWhen,
    applyUnless,
    whenNothing,
    whenNothing_,
    whenNothingM,
    whenNothingM_,
    (/\),
  )
where

import Control.Lens.Lens (ALens', Lens')
import Control.Lens.Unsound (lensProduct)

-- ala yjtools

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen b f x = if b then f x else x
{-# INLINE applyWhen #-}

applyUnless :: Bool -> (a -> a) -> a -> a
applyUnless b = applyWhen (not b)
{-# INLINE applyUnless #-}

-- ala relude

whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing (Just x) _ = pure x
whenNothing Nothing m = m
{-# INLINE whenNothing #-}

whenNothing_ :: Applicative f => Maybe a -> f () -> f ()
whenNothing_ Nothing m = m
whenNothing_ _ _ = pure ()
{-# INLINE whenNothing_ #-}

whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM mm action = mm >>= \m -> whenNothing m action
{-# INLINE whenNothingM #-}

whenNothingM_ :: Monad m => m (Maybe a) -> m () -> m ()
whenNothingM_ mm action = mm >>= \m -> whenNothing_ m action
{-# INLINE whenNothingM_ #-}

(/\) :: ALens' s a -> ALens' s b -> Lens' s (a, b)
(/\) = lensProduct
{-
-- ala stack overflow
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
        ( \(a', b') ->
            let (_, c1) = lens1 (\a_ -> (a_, a')) c0
                (_, c2) = lens2 (\b_ -> (b_, b')) c1
             in c2
        )
        fab
-}

{-# INLINE (/\) #-}

infixl 7 /\
