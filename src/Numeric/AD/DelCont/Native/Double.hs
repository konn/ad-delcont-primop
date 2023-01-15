{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Numeric.AD.DelCont.Native.Double (
  ADDouble,
  eval,
  konst,
  diff,
  grad,
  jacobian,
  op1,
  op2,
) where

import Control.Monad
import Control.Monad.ST.Strict
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.PRef
import GHC.Generics
import Numeric.AD.DelCont.Native.Internal

data ADDouble s = AD {primal :: !Double, dual :: !(PromptTag () -> ST s (PRef s Double))}

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

withGrad' :: PromptTag () -> (Double -> ST s ()) -> ST s (PRef s Double)
{-# INLINE withGrad' #-}
withGrad' tag f = shift tag $ \k -> do
  bx <- newPRef 0.0
  k (pure bx)
  f =<< readPRef bx

op1 :: (Double -> (Double, Double -> Double)) -> ADDouble s -> ADDouble s
{-# INLINE op1 #-}
op1 f (AD x toDx) =
  let (!fx, !deriv) = f x
   in AD fx $ \tag -> do
        dx <- toDx tag
        withGrad' tag $ \dc ->
          modifyPRef' dx (+ deriv dc)

op2 ::
  (Double -> Double -> (Double, Double -> Double, Double -> Double)) ->
  ADDouble s ->
  ADDouble s ->
  ADDouble s
{-# INLINE op2 #-}
op2 f (AD x toDx) (AD y toDy) =
  let (!fx, !derivX, !derivY) = f x y
   in AD fx $ \tag -> do
        dx <- toDx tag
        dy <- toDy tag
        withGrad' tag $ \dc -> do
          modifyPRef' dx (+ derivX dc)
          modifyPRef' dy (+ derivY dc)

konst :: Double -> ADDouble s
konst = flip AD (const $ newPRef 0.0)

instance Num (ADDouble s) where
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

instance Fractional (ADDouble s) where
  fromRational = konst . fromRational
  {-# INLINE fromRational #-}
  recip = op1 $ \x -> (recip x, negate . (/ (x * x)))
  {-# INLINE recip #-}
  (/) = op2 $ \x y -> (x / y, (/ y), (* (-x / (y * y))))
  {-# INLINE (/) #-}

instance Floating (ADDouble s) where
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

eval :: (forall s. ADDouble s -> ADDouble s) -> Double -> Double
{-# INLINE eval #-}
eval op = primal . op . konst

getDual :: PromptTag () -> ADDouble s -> ST s (PRef s Double)
{-# INLINE getDual #-}
getDual tag = ($ tag) . dual

diff :: (forall s. ADDouble s -> ADDouble s) -> Double -> Double
{-# INLINE diff #-}
diff op a = runST $ do
  ref <- newPRef 0
  tag <- newPromptTag
  prompt tag $ do
    dbRef <- getDual tag $ op $ AD a $ const $ pure ref
    writePRef dbRef 1
  readPRef ref

grad ::
  (Traversable t) =>
  (forall s. t (ADDouble s) -> ADDouble s) ->
  t Double ->
  t Double
{-# INLINE grad #-}
grad f xs = runST $ do
  inps <- mapM (\a -> (a,) <$> newPRef 0) xs
  tag <- newPromptTag
  prompt tag $ do
    db <- getDual tag $ f $ fmap (\(a, ref) -> AD a $ const $ pure ref) inps
    writePRef db 1
  mapM (readPRef . snd) inps

jacobian ::
  (Traversable t, Traversable g) =>
  (forall s. t (ADDouble s) -> g (ADDouble s)) ->
  t Double ->
  g (t Double)
{-# INLINE jacobian #-}
jacobian f xs = runST $ do
  inps <- mapM (\a -> (a,) <$> newPRef 0) xs
  let duals = fmap dual $ f $ fmap (\(a, ref) -> AD a $ const $ pure ref) inps
  forM duals $ \toDzi -> do
    tag <- newPromptTag
    prompt tag $ flip writePRef 1 =<< toDzi tag
    ans <- mapM (readPRef . snd) inps
    mapM_ (flip writePRef 0 . snd) inps
    pure ans
