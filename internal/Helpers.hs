{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Helpers ((==~), isDefinite) where

import Control.DeepSeq
import Data.Foldable
import Data.Functor.Compose
import GHC.TypeNats
import Linear
import Linear.V
import qualified Numeric.Backprop as BP
import Test.QuickCheck

(==~) :: (Show (v Double), Foldable v, Applicative v, Metric v) => v Double -> v Double -> Property
ls ==~ rs =
  counterexample
    ( unlines
        [ "Not near!"
        , " max err: " <> show (maximum $ relErr <$> ls <*> rs)
        , "     lhs: " <> show ls
        , "     rhs: " <> show rs
        ]
    )
    $ conjoin
    $ toList
    $ liftI2
      ( \l r ->
          (isDefinite l .&&. isDefinite r .&&. almostEq 1e5 l r)
            .||. (not (isDefinite l) .&&. not (isDefinite r))
      )
      ls
      rs

relErr :: Double -> Double -> Double
relErr l r
  | nearZero l = abs r
  | nearZero r = abs l
  | otherwise = abs (l - r) / max (abs l) (abs r)

almostEq :: Double -> Double -> Double -> Bool
almostEq thresh l r = nearZero $ relErr l r / thresh

isDefinite :: Double -> Bool
isDefinite c = not (isInfinite c || isNaN c)

instance Arbitrary a => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary
  shrink = mapM shrink

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = mapM shrink

instance Arbitrary a => Arbitrary (V4 a) where
  arbitrary = sequence $ pure arbitrary
  shrink = mapM shrink

instance Arbitrary1 V4 where
  liftArbitrary = sequence . pure
  liftShrink = mapM

instance Arbitrary1 V3 where
  liftArbitrary = sequence . pure
  liftShrink = mapM

instance Arbitrary1 V2 where
  liftArbitrary = sequence . pure
  liftShrink = mapM

instance Arbitrary1 V1 where
  liftArbitrary = sequence . pure
  liftShrink = mapM

instance (Arbitrary a, Dim n) => Arbitrary (V n a) where
  arbitrary = sequenceA $ pure arbitrary
  shrink = mapM shrink

deriving newtype instance Arbitrary a => Arbitrary (V1 a)

deriving via BP.NumBP (V1 a) instance Num a => BP.Backprop (V1 a)

deriving via BP.NumBP (V2 a) instance Num a => BP.Backprop (V2 a)

deriving via BP.NumBP (V3 a) instance Num a => BP.Backprop (V3 a)

deriving via BP.NumBP (V4 a) instance Num a => BP.Backprop (V4 a)

deriving via BP.NumBP (V n a) instance (Dim n, Num a) => BP.Backprop (V n a)

deriving anyclass instance NFData1 V4

deriving anyclass instance NFData1 V3

deriving anyclass instance NFData1 V2

deriving anyclass instance NFData1 V1

deriving newtype instance (Num (u (v a))) => Num (Compose u v a)
