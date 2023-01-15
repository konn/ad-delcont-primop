{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}

module Numeric.AD.DelCont.Native (
  AD',
  AD,
  eval,
  konst,
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
import Data.Primitive.MutVar
import GHC.Generics
import GHC.Prim
import Numeric.AD.DelCont.Native.Internal

data BRef s a = BRef {-# UNPACK #-} !(# MutVar# s a | (# #) #)

pattern Ref :: MutVar s a -> BRef s a
pattern Ref ref <- BRef (# (MutVar -> ref) | #)
  where
    Ref (MutVar ref#) = BRef (# ref# | #)

pattern Const :: BRef s a
pattern Const = BRef (# | (# #) #)

{-# COMPLETE Const, Ref #-}

newBRef :: a -> ST s (BRef s a)
{-# INLINE newBRef #-}
newBRef = fmap Ref . newMutVar

modifyBRef' :: BRef s a -> (a -> a) -> ST s ()
{-# INLINE modifyBRef' #-}
modifyBRef' = \case
  Ref ref -> modifyMutVar' ref
  Const -> const $ pure ()

writeBRef' :: BRef s a -> a -> ST s ()
writeBRef' = \case
  Ref ref -> \ !a -> writeMutVar ref a
  (Const) -> const $ pure ()

readBRef :: BRef s a -> ST s a
{-# INLINE readBRef #-}
readBRef = readBRefDefault $ error "Empty BRef!"

readBRefDefault :: a -> BRef s a -> ST s a
{-# INLINE readBRefDefault #-}
readBRefDefault = \cases
  _ (Ref ref) -> readMutVar ref
  a Const -> pure a

data AD' s a da = AD {primal :: !a, dual :: !(PromptTag () -> ST s (BRef s da))}

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

withGrad' :: a -> PromptTag () -> (a -> ST s ()) -> ST s (BRef s a)
{-# INLINE withGrad' #-}
withGrad' zero tag f = shift tag $ \k -> do
  bx <- newBRef zero
  k (pure bx)
  f =<< readBRefDefault zero bx

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
          modifyBRef' dx (`addda` deriv dc)

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
          modifyBRef' dx (`addDa` derivX dc)
          modifyBRef' dy (`addDb` derivY dc)

konst :: a -> AD' s a da
konst = flip AD (const $ pure Const)

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

eval :: (forall s. AD' s a da -> AD' s b db) -> a -> b
{-# INLINE eval #-}
eval op = primal . op . konst

getDual :: PromptTag () -> AD' s a da -> ST s (BRef s da)
{-# INLINE getDual #-}
getDual tag = ($ tag) . dual

diff :: (Num da, Num db) => (forall s. AD' s a da -> AD' s b db) -> a -> da
{-# INLINE diff #-}
diff op a = runST $ do
  ref <- newBRef 0
  tag <- newPromptTag
  prompt tag $ do
    dbRef <- getDual tag $ op $ AD a $ const $ pure ref
    writeBRef' dbRef 1
  readBRefDefault 0 ref

grad ::
  (Num da, Num db, Traversable t) =>
  (forall s. t (AD' s a da) -> AD' s b db) ->
  t a ->
  t da
{-# INLINE grad #-}
grad f xs = runST $ do
  inps <- mapM (\a -> (a,) <$> newBRef 0) xs
  tag <- newPromptTag
  prompt tag $ do
    db <- getDual tag $ f $ fmap (\(a, ref) -> AD a $ const $ pure ref) inps
    writeBRef' db 1
  mapM (readBRefDefault 0 . snd) inps

jacobian ::
  (Num da, Num db, Traversable t, Traversable g) =>
  (forall s. t (AD' s a da) -> g (AD' s b db)) ->
  t a ->
  g (t da)
{-# INLINE jacobian #-}
jacobian f xs = runST $ do
  inps <- mapM (\a -> (a,) <$> newBRef 0) xs
  let duals = fmap dual $ f $ fmap (\(a, ref) -> AD a $ const $ pure ref) inps
  forM duals $ \toDzi -> do
    tag <- newPromptTag
    prompt tag $ flip writeBRef' 1 =<< toDzi tag
    ans <- mapM (readBRefDefault 0 . snd) inps
    mapM_ (flip writeBRef' 0 . snd) inps
    pure ans
