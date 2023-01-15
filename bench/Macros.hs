{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Macros (mkDiffBench, mkGradBench) where

import Control.Lens.Plated
import Data.Data.Lens
import Helpers
import Language.Haskell.TH
import Linear
import qualified Numeric.AD as AD
import qualified Numeric.AD.DelCont as MTL
import qualified Numeric.AD.DelCont.Native as PrimOp
import qualified Numeric.AD.DelCont.Native.Double as PrimOpDouble
import qualified Numeric.AD.DelCont.Native.MultiPrompt as MP
import qualified Numeric.AD.Double as ADDouble
import qualified Numeric.Backprop as BP
import Test.Tasty.Bench
import Test.Tasty.QuickCheck

mkDiffBench :: ExpQ -> ExpQ
mkDiffBench func = do
  lab <- showBody <$> runQ func
  [|
    bgroup
      lab
      [ testProperty "ad and primops returns almost the same answer" $
          \(x :: Double) ->
            let ad = AD.diff $(func) x
                primop = PrimOp.diff ($func :: PrimOp.AD s Double -> PrimOp.AD s Double) x
             in classify (not $ isDefinite primop) "diverged" $ V1 ad ==~ V1 primop
      , testProperty "ad and primops/mp returns almost the same answer" $
          \(x :: Double) ->
            let ad = AD.diff $(func) x
                primop = MP.diff $(func) x
             in classify (not $ isDefinite primop) "diverged" $ V1 ad ==~ V1 primop
      , testProperty "ad/double and primops/double returns almost the same answer" $
          \(x :: Double) ->
            let ad = AD.diff $(func) x
                primop = PrimOpDouble.diff $(func) x
             in classify (not $ isDefinite primop) "diverged" $ V1 ad ==~ V1 primop
      , bench "transformers" $ nf (snd . MTL.rad1 $func) (42.0 :: Double)
      , bench "ad" $ nf (AD.diff $func) (42.0 :: Double)
      , bench "ad/double" $ nf (ADDouble.diff $func) (42.0 :: Double)
      , bench "backprop" $ nf (BP.gradBP $func) (42.0 :: Double)
      , bench "primops" $ nf (PrimOp.diff ($func :: PrimOp.AD s Double -> PrimOp.AD s Double)) (42.0 :: Double)
      , bench "primops/mp" $ nf (MP.diff ($func :: MP.AD s Double -> MP.AD s Double)) (42.0 :: Double)
      , bench "primops/Double" $ nf (PrimOpDouble.diff $func) (42.0 :: Double)
      ]
    |]

showBody :: Exp -> String
showBody (LamE _ b) = showBody b
showBody e = pprint $ transformOnOf biplate uniplate unQualNames e

unQualNames :: Name -> Name
unQualNames = mkName . nameBase

mkGradBench :: ExpQ -> ExpQ -> ExpQ
mkGradBench func arg0 = do
  lab <- showBody <$> runQ func
  [|
    env (pure ($(arg0) :: _ Double)) $ \arg ->
      bgroup
        lab
        [ testProperty "ad and primops returns almost the same answer" $
            \(x :: _ Double) ->
              let ad = AD.grad $(func) x
                  primop =
                    PrimOp.grad
                      ($func :: _ (PrimOp.AD s Double) -> PrimOp.AD s Double)
                      x
               in classify (any (not . isDefinite) ad) "diverged" $ ad ==~ primop
        , testProperty "ad and primops/mp returns almost the same answer" $
            \(x :: _ Double) ->
              let ad = AD.grad $(func) x
                  primop =
                    MP.grad
                      ($func :: _ (MP.AD s Double) -> MP.AD s Double)
                      x
               in classify (any (not . isDefinite) ad) "diverged" $ ad ==~ primop
        , testProperty "ad/double and primops/double returns almost the same answer" $
            \(x :: _ Double) ->
              let ad = AD.grad $(func) x
                  primop = PrimOpDouble.grad $(func) x
               in classify (any (not . isDefinite) ad) "diverged" $ ad ==~ primop
        , bench "transformers" $ nf (snd . MTL.grad $func) arg
        , bench "ad" $ nf (AD.grad $func) arg
        , bench "ad/double" $ nf (ADDouble.grad $func) arg
        , bench "backprop" $ nf (BP.gradBP ($func . BP.sequenceVar)) arg
        , bench "primops" $ nf (PrimOp.grad ($func :: _ (PrimOp.AD s Double) -> PrimOp.AD s Double)) arg
        , bench "primops/mp" $ nf (MP.grad ($func :: _ (MP.AD s Double) -> MP.AD s Double)) arg
        , bench "primops/double" $ nf (PrimOpDouble.grad $func) arg
        ]
    |]
