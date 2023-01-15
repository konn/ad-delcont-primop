{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Helpers ((==~), isDefinite) where

import Data.Foldable
import Linear
import qualified Numeric.Backprop as BP
import Test.Tasty.QuickCheck

(==~) :: (Show (v Double), Foldable v, Metric v) => v Double -> v Double -> Property
ls ==~ rs =
  counterexample ("Not near: " <> show (ls, rs)) $
    conjoin $
      toList $
        liftI2
          ( \l r ->
              (isDefinite l .&&. isDefinite r .&&. almostEq 1e1 l r)
                .||. (not (isDefinite l) .&&. not (isDefinite r))
          )
          ls
          rs

almostEq :: Double -> Double -> Double -> Bool
almostEq thresh l r
  | nearZero l = nearZero (r / thresh)
  | otherwise = nearZero $ abs (l - r) / max (abs l) (abs r) / thresh

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

deriving newtype instance Arbitrary a => Arbitrary (V1 a)

deriving via BP.NumBP (V1 a) instance Num a => BP.Backprop (V1 a)

deriving via BP.NumBP (V2 a) instance Num a => BP.Backprop (V2 a)

deriving via BP.NumBP (V3 a) instance Num a => BP.Backprop (V3 a)

deriving via BP.NumBP (V4 a) instance Num a => BP.Backprop (V4 a)
