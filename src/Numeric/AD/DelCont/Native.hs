{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}

module Numeric.AD.DelCont.Native (
  AD',
  eval,
  konst,
  grad,
  op1,
  op2,
) where

import Control.Monad.ST.Strict
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.STRef
import GHC.Generics
import Numeric.AD.DelCont.Native.Internal

data AD' s a da = AD {primal :: !a, dual :: !(PromptTag () -> ST s (STRef s da))}

data a :!: b = !a :!: !b
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

instance Bitraversable (:!:) where
  bitraverse f g (l :!: r) = (:!:) <$> f l <*> g r

instance Bifunctor (:!:) where
  bimap = bimapDefault
  {-# INLINE bimap #-}

instance Bifoldable (:!:) where
  bifoldMap = bifoldMapDefault
  {-# INLINE bifoldMap #-}

withGrad :: Num a => PromptTag () -> (a -> ST s ()) -> ST s (STRef s a)
{-# INLINE withGrad #-}
withGrad tag f = shift tag $ \k -> do
  bx <- newSTRef 0
  k (pure bx)
  f =<< readSTRef bx

op1 :: (Num da, Num db) => (a -> (b, db -> da)) -> AD' s a da -> AD' s b db
{-# INLINE op1 #-}
op1 f (AD x toDx) =
  let (!fx, !deriv) = f x
   in AD fx $ \tag -> do
        dx <- toDx tag
        withGrad tag $ \dc ->
          modifySTRef' dx (+ deriv dc)

op2 ::
  (Num da, Num db, Num dc) =>
  (a -> b -> (c, dc -> da, dc -> db)) ->
  AD' s a da ->
  AD' s b db ->
  AD' s c dc
op2 f (AD x toDx) (AD y toDy) =
  let (!fx, !derivX, !derivY) = f x y
   in AD fx $ \tag -> do
        dx <- toDx tag
        dy <- toDy tag
        withGrad tag $ \dc -> do
          modifySTRef' dx (+ derivX dc)
          modifySTRef' dy (+ derivY dc)

konst :: Num da => a -> AD' s a da
konst = flip AD (const $ newSTRef 0)

instance (Num a, a ~ b) => Num (AD' s a b) where
  fromInteger = konst . fromInteger
  {-# INLINE fromInteger #-}
  signum = op1 $ \c -> (signum c, const 0)
  {-# INLINE signum #-}
  negate = op1 $ \c -> (negate c, negate)
  {-# INLINE negate #-}
  abs = op1 $ \x -> (abs x, (signum x *))
  {-# INLINE abs #-}
  (+) = op2 $ \a b -> (a + b, id, id)
  {-# INLINE (+) #-}
  (-) = op2 $ \a b -> (a - b, id, negate)
  {-# INLINE (-) #-}
  (*) = op2 $ \a b -> (a * b, (* b), (a *))
  {-# INLINE (*) #-}

instance (Fractional a, a ~ b) => Fractional (AD' s a b) where
  fromRational = konst . fromRational
  {-# INLINE fromRational #-}
  recip = op1 $ \x -> (recip x, negate . (/ (x * x)))
  {-# INLINE recip #-}
  (/) = op2 $ \x y -> (x / y, (/ y), (* (x / (y * y))))
  {-# INLINE (/) #-}

instance (Floating a, a ~ b) => Floating (AD' s a b) where
  pi = konst pi
  {-# INLINE pi #-}
  exp = op1 $ \x -> (exp x, (exp x *))
  {-# INLINE exp #-}
  log = op1 $ \x -> (log x, (/ x))
  {-# INLINE log #-}
  sqrt = op1 $ \x -> (sqrt x, (/ (2 * sqrt x)))
  {-# INLINE sqrt #-}
  sin = op1 $ \x -> (sin x, (* cos x))
  {-# INLINE sin #-}
  cos = op1 $ \x -> (cos x, (* (-sin x)))
  {-# INLINE cos #-}
  tan = op1 $ \x -> (tan x, (/ cos x ^ (2 :: Int)))
  {-# INLINE tan #-}
  asin = op1 $ \x -> (asin x, (/ sqrt (1 - x * x)))
  {-# INLINE asin #-}
  acos = op1 $ \x -> (acos x, (/ sqrt (1 - x * x)) . negate)
  {-# INLINE acos #-}
  atan = op1 $ \x -> (atan x, (/ (1 + x * x)))
  {-# INLINE atan #-}
  sinh = op1 $ \x -> (sinh x, (* cosh x))
  {-# INLINE sinh #-}
  cosh = op1 $ \x -> (cosh x, (* sinh x))
  {-# INLINE cosh #-}
  tanh = op1 $ \x -> (tanh x, (/ cosh x ^ (2 :: Int)))
  {-# INLINE tanh #-}
  asinh = op1 $ \x -> (asinh x, (/ sqrt (x * x + 1)))
  {-# INLINE asinh #-}
  acosh = op1 $ \x -> (acosh x, (/ sqrt (x * x - 1)))
  {-# INLINE acosh #-}
  atanh = op1 $ \x -> (atanh x, (/ (1 - x * x)))
  {-# INLINE atanh #-}

eval :: (Num a) => (forall s. AD' s a a -> AD' s b b) -> a -> b
{-# INLINE eval #-}
eval op = primal . op . konst

grad :: (Num da, Num db) => (forall s. AD' s a da -> AD' s b db) -> a -> da
grad op a = runST $ do
  ref <- newSTRef 0
  tag <- newPromptTag
  prompt tag $ do
    let db = dual $ op $ AD a $ const $ pure ref
    dbRef <- db tag
    writeSTRef dbRef 1
  readSTRef ref
