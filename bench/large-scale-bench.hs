{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main (main) where

import Helpers
import LargeCase
import Macros
import qualified Numeric.AD as AD
import qualified Numeric.AD.DelCont.Native as PrimOp
import qualified Numeric.AD.DelCont.Native.Double as PrimOpDouble
import qualified Numeric.AD.DelCont.Native.MultiPrompt as MP
import qualified Numeric.AD.DelCont.Native.MultiPrompt.Double as MPDouble
import qualified Numeric.AD.Double as ADDouble
import System.Environment
import Test.Tasty.Bench
import Test.Tasty.QuickCheck

main :: IO ()
main = do
  -- FIXME: consider using 'tryIngredients'
  args0 <- getArgs
  let args
        | "-j" `elem` args0 || "--num-threads" `elem` args0 = args0
        | otherwise = "-j" : "1" : args0
  withArgs args $
    defaultMain
      [ env (sequence $ pure (pure 42)) $ \inp ->
          bgroup
            "128-ary (V4 Composed' four times)"
            [$(mkGradBench' [|largeFunc1|]) inp]
      , bgroup
          (show (length $ pure @Network1 ()) <> "-ary")
          [ env (pure (dummyNetwork1, dummyImage, dummyAnswer1)) $ \ ~(nn, img, a1) ->
              bgroup
                "toy-mnist"
                [ testProperty "primop/mp/double almost coincides with ad/double" $
                    \nn1 (Image img1) (Digit ans1) ->
                      let !ad = ADDouble.grad (calcLossNN1 img1 ans1) nn1
                          !preds = MPDouble.grad (calcLossNN1 img1 ans1) nn1
                       in ad ==~ preds
                , bench "ad" $ nf (AD.grad $ calcLossNN1 img a1) nn
                , bench "ad/double" $ nf (ADDouble.grad $ calcLossNN1 img a1) nn
                , bench "primop" $ nf (PrimOp.grad $ calcLossNN1 img a1) nn
                , bench "primop/double" $ nf (PrimOpDouble.grad $ calcLossNN1 img a1) nn
                , bench "primop/mp" $ nf (MP.grad $ calcLossNN1 img a1) nn
                , bench "primop/mp/double" $ nf (MPDouble.grad $ calcLossNN1 img a1) nn
                ]
          ]
      ]
