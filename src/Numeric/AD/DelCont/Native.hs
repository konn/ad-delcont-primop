{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}

module Numeric.AD.DelCont.Native (
  AD',
  AD,
  eval,
  konst,
  konst',
  diff,
  grad,
  jacobian,
  op1,
  op1',
  op2,
  op2',
) where

import Control.Monad
import Control.Monad.ST.Strict
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Function (on)
import Data.Ord (comparing)
import Data.STRef
import GHC.Generics
import Numeric.AD.DelCont.Native.Internal

data AD' s a da = AD {primal :: !a, dual :: !(PromptTag () -> ST s (STRef s da))}

instance Ord a => Eq (AD' s a da) where
  (==) = (==) `on` primal
  {-# INLINE (==) #-}

instance Ord a => Ord (AD' s a da) where
  compare = comparing primal
  {-# INLINE compare #-}
  (<) = (<) `on` primal
  {-# INLINE (<) #-}
  (<=) = (<=) `on` primal
  {-# INLINE (<=) #-}
  (>) = (>) `on` primal
  {-# INLINE (>) #-}
  (>=) = (>=) `on` primal
  {-# INLINE (>=) #-}

type AD s a = AD' s a a

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

withGrad' :: a -> PromptTag () -> (a -> ST s ()) -> ST s (STRef s a)
{-# INLINE withGrad' #-}
withGrad' zero tag f = shift tag $ \k -> do
  bx <- newSTRef zero
  k (pure bx)
  f =<< readSTRef bx

op1 :: (Num da, Num db) => (a -> (b, db -> da)) -> AD' s a da -> AD' s b db
{-# INLINE op1 #-}
op1 = op1' 0 (+)

op1' :: db -> (da -> da -> da) -> (a -> (b, db -> da)) -> AD' s a da -> AD' s b db
{-# INLINE op1' #-}
op1' zerodb addda f (AD x toDx) =
  let (!fx, !deriv) = f x
   in AD fx $ \tag -> do
        dx <- toDx tag
        withGrad' zerodb tag $ \dc ->
          modifySTRef' dx (`addda` deriv dc)

op2 ::
  (Num da, Num db, Num dc) =>
  (a -> b -> (c, dc -> da, dc -> db)) ->
  AD' s a da ->
  AD' s b db ->
  AD' s c dc
op2 = op2' 0 (+) (+)

op2' ::
  dc ->
  (da -> da -> da) ->
  (db -> db -> db) ->
  (a -> b -> (c, dc -> da, dc -> db)) ->
  AD' s a da ->
  AD' s b db ->
  AD' s c dc
op2' zeroDc addDa addDb f (AD x toDx) (AD y toDy) =
  let (!fx, !derivX, !derivY) = f x y
   in AD fx $ \tag -> do
        dx <- toDx tag
        dy <- toDy tag
        withGrad' zeroDc tag $ \dc -> do
          modifySTRef' dx (`addDa` derivX dc)
          modifySTRef' dy (`addDb` derivY dc)

konst :: Num da => a -> AD' s a da
konst = konst' 0

konst' :: da -> a -> AD' s a da
konst' zeroDa = flip AD (const $ newSTRef zeroDa)

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
  (/) = op2 $ \x y -> (x / y, (/ y), (* (-x / (y * y))))
  {-# INLINE (/) #-}

instance (Floating a, a ~ b) => Floating (AD' s a b) where
  pi = konst pi
  {-# INLINE pi #-}
  exp = op1 $ \x -> (exp x, (exp x *))
  {-# INLINE exp #-}
  log = op1 $ \x -> (log x, (/ x))
  {-# INLINE log #-}
  logBase = op2 $ \x y ->
    ( logBase x y
    , (* (-logBase x y / (log x * x)))
    , (/ (y * log x))
    )
  {-# INLINE logBase #-}
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

eval :: (Num da) => (forall s. AD' s a da -> AD' s b db) -> a -> b
{-# INLINE eval #-}
eval op = primal . op . konst

getDual :: PromptTag () -> AD' s a da -> ST s (STRef s da)
{-# INLINE getDual #-}
getDual tag = ($ tag) . dual

diff :: (Num da, Num db) => (forall s. AD' s a da -> AD' s b db) -> a -> da
{-# INLINE diff #-}
diff op a = runST $ do
  ref <- newSTRef 0
  tag <- newPromptTag
  prompt tag $ do
    dbRef <- getDual tag $ op $ AD a $ const $ pure ref
    writeSTRef dbRef 1
  readSTRef ref

grad ::
  (Num da, Num db, Traversable t) =>
  (forall s. t (AD' s a da) -> AD' s b db) ->
  t a ->
  t da
{-# INLINE grad #-}
grad f xs = runST $ do
  inps <- mapM (\a -> (a,) <$> newSTRef 0) xs
  tag <- newPromptTag
  prompt tag $ do
    db <- getDual tag $ f $ fmap (\(a, ref) -> AD a $ const $ pure ref) inps
    writeSTRef db 1
  mapM (readSTRef . snd) inps

jacobian ::
  (Num da, Num db, Traversable t, Traversable g) =>
  (forall s. t (AD' s a da) -> g (AD' s b db)) ->
  t a ->
  g (t da)
{-# INLINE jacobian #-}
jacobian f xs = runST $ do
  inps <- mapM (\a -> (a,) <$> newSTRef 0) xs
  let duals = fmap dual $ f $ fmap (\(a, ref) -> AD a $ const $ pure ref) inps
  forM duals $ \toDzi -> do
    tag <- newPromptTag
    prompt tag $ flip writeSTRef 1 =<< toDzi tag
    ans <- mapM (readSTRef . snd) inps
    mapM_ (flip writeSTRef 0 . snd) inps
    pure ans
