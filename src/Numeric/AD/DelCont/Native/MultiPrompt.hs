{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.AD.DelCont.Native.MultiPrompt (
  AD',
  AD,
  eval,
  konst,
  diff,
  grad,
  -- jacobian,
  op1,
  op2,
) where

import Control.Monad
import Control.Monad.ST.Strict
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor
import Data.STRef
import GHC.Generics
import Numeric.AD.DelCont.Native.Internal

data AD' s a da = AD {primal :: !a, dual :: !(PromptTag da -> ST s ())}

type AD s a = AD' s a a

data a :!: b = !a :!: !b
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

fst' :: (a :!: b) -> a
{-# INLINE fst' #-}
fst' = \case a :!: _ -> a

instance Bitraversable (:!:) where
  bitraverse f g (l :!: r) = (:!:) <$> f l <*> g r

instance Bifunctor (:!:) where
  bimap = bimapDefault
  {-# INLINE bimap #-}

instance Bifoldable (:!:) where
  bifoldMap = bifoldMapDefault
  {-# INLINE bifoldMap #-}

op1 :: (a -> (b, db -> da)) -> AD' s a da -> AD' s b db
{-# INLINE op1 #-}
op1 f (AD x toDx) =
  let (!fx, !deriv) = f x
   in AD fx $ \tag -> control0 tag $ \k -> do
        !dy <- k $ pure ()
        void $ reset $ \innerTag -> do
          !() <- toDx innerTag
          pure $! deriv dy
        pure dy

op2 ::
  (a -> b -> (c, dc -> da, dc -> db)) ->
  AD' s a da ->
  AD' s b db ->
  AD' s c dc
op2 f (AD x getDx) (AD y getDy) =
  let (!fx, !derivX, !derivY) = f x y
   in AD fx $ \tag -> do
        control0 tag $ \k -> do
          !dz <- k (pure ())
          void $ reset $ \tagX -> do
            () <- getDx tagX
            pure $! derivX dz
          void $ reset $ \tagY -> do
            () <- getDy tagY
            pure $! derivY dz
          pure dz

konst :: a -> AD' s a da
konst = flip AD (const $ pure ())

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

toDual :: PromptTag da -> AD' s a da -> ST s ()
{-# INLINE toDual #-}
toDual tag = ($ tag) . dual

diff :: forall a da b db. (Num da, Num db) => (forall s. AD' s a da -> AD' s b db) -> a -> da
{-# INLINE diff #-}
diff op a = runST $ do
  ref <- newSTRef 0
  let bdb = op $ AD a $ \tagDa -> control0 tagDa $ \k -> do
        !val <- k $! pure ()
        modifySTRef' ref (+ val)
        pure val
  void $ reset $ \tag -> 1 <$ toDual tag bdb
  readSTRef ref

grad ::
  (Num da, Num db, Traversable t) =>
  (forall s. t (AD' s a da) -> AD' s b db) ->
  t a ->
  t da
{-# INLINE grad #-}
grad f xs = runST $ do
  refs <- mapM (\a -> (:!: a) <$> newSTRef 0) xs
  let bdb =
        f $
          refs <&> \(ref :!: a) -> AD a $ \tagDa -> control0 tagDa $ \k -> do
            !val <- k $! pure ()
            modifySTRef' ref (+ val)
            pure val
  void $ reset $ \tag -> 1 <$ toDual tag bdb
  mapM (readSTRef . fst') refs

{-

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
 -}
