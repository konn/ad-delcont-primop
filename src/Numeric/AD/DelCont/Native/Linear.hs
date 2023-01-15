module Numeric.AD.DelCont.Native.Linear (
  negated,
  (^+^),
  (^-^),
  (*^),
  (^*),
  (^/),
  zero,
  sumV,
  dot,
  quadrance,
  norm,
  pured,
) where

import Control.Monad
import Data.Foldable
import qualified Linear as L
import Numeric.AD.DelCont.Native

infixl 6 ^+^

negated :: (L.Additive v, L.Additive u, Num a, Num da) => AD' s (v a) (u da) -> AD' s (v a) (u da)
negated = op1' L.zero (L.^+^) $ \v ->
  (L.negated v, L.negated)

(^+^) ::
  (L.Additive v, L.Additive u, Num a, Num da) =>
  AD' s (v a) (u da) ->
  AD' s (v a) (u da) ->
  AD' s (v a) (u da)
{-# INLINE (^+^) #-}
(^+^) = op2' L.zero (L.^+^) (L.^+^) $ \x y -> (x L.^+^ y, id, id)

infixl 6 ^-^

(^-^) ::
  (L.Additive v, L.Additive u, Num a, Num da) =>
  AD' s (v a) (u da) ->
  AD' s (v a) (u da) ->
  AD' s (v a) (u da)
{-# INLINE (^-^) #-}
(^-^) = op2' L.zero (L.^+^) (L.^+^) $ \x y -> (x L.^-^ y, id, fmap negate)

infixl 7 *^

(*^) ::
  (L.Metric v, Num a) =>
  AD s a ->
  AD s (v a) ->
  AD s (v a)
{-# INLINE (*^) #-}
(*^) = op2' L.zero (+) (L.^+^) $ \c v ->
  (c L.*^ v, (`L.dot` v), (c L.*^))

infixl 7 ^*

(^*) ::
  (L.Metric v, Num a) =>
  AD' s (v a) (v a) ->
  AD' s a a ->
  AD' s (v a) (v a)
{-# INLINE (^*) #-}
(^*) = op2' L.zero (L.^+^) (+) $ \v c ->
  (v L.^* c, (L.^* c), (v `L.dot`))

infixl 7 ^/

(^/) ::
  (L.Metric v, Fractional a) =>
  AD' s (v a) (v a) ->
  AD' s a a ->
  AD' s (v a) (v a)
{-# INLINE (^/) #-}
(^/) = op2' L.zero (L.^+^) (+) $ \v c ->
  (v L.^/ c, (L.^/ c), \dz -> (-dz `L.dot` v / (c * c)))

zero :: (L.Additive v, Num a) => AD' s (v a) (u da)
zero = konst L.zero

sumV ::
  (Foldable t, L.Additive v, L.Additive u, Num a, Num da) =>
  t (AD' s (v a) (u da)) ->
  AD' s (v a) (u da)
sumV = foldl' (^+^) zero

dot ::
  (L.Metric v, Num a) =>
  AD s (v a) ->
  AD s (v a) ->
  AD s a
dot = op2' 0 (L.^+^) (L.^+^) $ \x y ->
  (x `L.dot` y, \dz -> dz L.*^ y, \dz -> dz L.*^ x)

norm ::
  (Floating a, L.Metric v) =>
  AD s (v a) ->
  AD s a
{-# INLINE norm #-}
norm = sqrt . quadrance

quadrance :: (Floating a, L.Metric v) => AD s (v a) -> AD s a
{-# INLINE quadrance #-}
quadrance = join dot

pured :: (L.Additive u, Foldable u, Applicative u, Num da) => AD' s a da -> AD' s (u a) (u da)
pured = op1' L.zero (+) $ \x -> (pure x, sum)
