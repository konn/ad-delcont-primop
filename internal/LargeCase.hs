{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Werror=typed-holes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-defer-type-errors #-}

module LargeCase (
  ComposedV128,
  largeFunc1,
  relu,
  sigmoid,
  softmax,
  crossEntropy,
  dummyImage,
  dummyAnswer1,
  dummyNetwork1,
  calcNetwork1,
  calcLossNN1,
  V64,
  Image (..),
  Digit (..),
  Network1,
) where

import Control.DeepSeq
import Data.Distributive
import Data.Functor.Compose
import Data.Functor.Rep
import Data.Reflection
import Data.Vector qualified as V
import GHC.Generics (Generic, Generic1)
import Linear
import Linear.V
import Numeric.AD qualified as AD (auto)
import Numeric.AD.DelCont.Native qualified as PrimOp
import Numeric.AD.DelCont.Native.Double qualified as PrimOpDouble
import Numeric.AD.DelCont.Native.MultiPrompt qualified as MP
import Numeric.AD.DelCont.Native.MultiPrompt.Double qualified as MPDouble
import Numeric.AD.Internal.Reverse qualified as AD
import Numeric.AD.Internal.Reverse.Double qualified as ADDouble
import System.Random.Stateful
import Test.QuickCheck

type ComposedV128 = (Compose V4 V4) `Compose` (Compose V4 V4)

func1Seed :: Floating a => V4 a -> a
{-# INLINE func1Seed #-}
func1Seed = \(V4 x y z w) ->
  logBase (x ^ (2 :: Int) + tanh w) (cos (x * x + 2 * z) + w + 1) ^ (4 :: Int)
    + exp (x + sin (pi * x + w ^ 2) * cosh (exp y ^ 2 * sin z) ^ (2 :: Int) * (w + 1))

largeFunc1 :: Floating a => ComposedV128 a -> a
{-# INLINE largeFunc1 #-}
largeFunc1 =
  func1Seed
    . fmap func1Seed
    . getCompose
    . fmap func1Seed
    . fmap func1Seed
    . fmap getCompose
    . getCompose

type V64 = V 64

relu :: (Num a, Ord a) => a -> a
relu = max 0

sigmoid :: Floating a => a -> a
sigmoid x = recip $ 1 + exp (-x)

softmax :: (Functor t, Foldable t, Floating a) => t a -> t a
softmax xs =
  let exps = fmap exp xs
      !theSum = sum exps
   in (/ theSum) <$> exps

crossEntropy :: (Foldable f, Applicative f, Floating a) => f a -> f a -> a
crossEntropy ys' ys = sum $ (*) <$> ys <*> fmap log ys'

repRandomWith ::
  (Applicative t, Traversable t) =>
  (Double, Double) ->
  StdGen ->
  t Double
repRandomWith ran =
  flip runStateGen_ $ sequenceA . pure . uniformRM ran

dummyImage :: V 128 Double
dummyImage = repRandomWith (0.0, 1.0) (mkStdGen 42)

dummyNetwork1 :: Network1 Double
dummyNetwork1 = repRandomWith (-1.0, 1.0) (mkStdGen 129)

dummyAnswer1 :: V 10 Double
dummyAnswer1 = V $ V.fromList [0, 1, 0, 0, 0, 0, 0, 0, 0, 0]

data Network1 a = Network1
  { layer1 :: !(V 32 (V 128 a))
  , layer2 :: !(V 16 (V 32 a))
  , layer3 :: !(V 10 (V 16 a))
  }
  deriving (Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Representable, NFData, Metric, Additive)
  deriving (Applicative) via Co Network1

calcNetwork1 :: (Ord a, Floating a, LiftDouble a) => Network1 a -> V 128 Double -> V 10 a
{-# INLINE calcNetwork1 #-}
calcNetwork1 Network1 {..} inp =
  let !v32 = relu <$> layer1 !* fmap liftDouble inp
      !v16 = sigmoid <$> layer2 !* v32
   in softmax $ layer3 !* v16

class LiftDouble d where
  liftDouble :: Double -> d

instance LiftDouble Double where
  liftDouble = id
  {-# INLINE liftDouble #-}

instance (a ~ Double, Reifies s AD.Tape) => LiftDouble (AD.Reverse s a) where
  liftDouble = AD.auto
  {-# INLINE liftDouble #-}

instance (Reifies s ADDouble.Tape) => LiftDouble (ADDouble.ReverseDouble s) where
  liftDouble = AD.auto
  {-# INLINE liftDouble #-}

instance (a ~ Double, Num da) => LiftDouble (PrimOp.AD' s a da) where
  liftDouble = PrimOp.konst
  {-# INLINE liftDouble #-}

instance LiftDouble (PrimOpDouble.ADDouble s) where
  liftDouble = PrimOpDouble.konst
  {-# INLINE liftDouble #-}

instance (a ~ Double) => LiftDouble (MP.AD' s a da) where
  liftDouble = MP.konst
  {-# INLINE liftDouble #-}

instance LiftDouble (MPDouble.AD s) where
  liftDouble = MPDouble.konst
  {-# INLINE liftDouble #-}

calcLossNN1 ::
  (Ord a, Floating a, LiftDouble a) =>
  V 128 Double ->
  V 10 Double ->
  Network1 a ->
  a
calcLossNN1 inp ans = \nn ->
  crossEntropy (calcNetwork1 nn inp) (liftDouble <$> ans)

instance Distributive Network1 where
  collect = collectRep
  {-# INLINE collect #-}
  distribute = distributeRep
  {-# INLINE distribute #-}

instance (Random a, Num a) => Arbitrary (Network1 a) where
  arbitrary = Network1 <$> choose (-1, 1) <*> choose (-1, 1) <*> choose (-1, 1)

newtype Image = Image {getImage :: V 128 Double}
  deriving (Show, Eq, Ord, Generic)

newtype Digit = Digit {getDigit :: V 10 Double}
  deriving (Show, Eq, Ord, Generic)

instance Arbitrary Image where
  arbitrary = fmap Image $ sequenceA $ pure (choose (0.0, 1.0))

instance Arbitrary Digit where
  arbitrary = do
    i <- chooseInt (0, 9)
    pure $ Digit $ V $ V.generate 10 (\j -> if i == j then 1.0 else 0.0)
