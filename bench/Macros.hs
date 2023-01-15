{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Macros (mkDiffBench, mkGradBench) where

import Control.Lens.Plated
import Data.Data.Lens
import Helpers
import Language.Haskell.TH
import Linear
import qualified Numeric.AD.DelCont as MTL
import qualified Numeric.AD.DelCont.Native as PrimOp
import Test.Tasty.Bench
import Test.Tasty.QuickCheck

mkDiffBench :: ExpQ -> ExpQ
mkDiffBench func = do
  lab <- showBody <$> runQ func
  [|
    bgroup
      lab
      [ testProperty "transformers and primops returns almost the same answer" $
          \(x :: Double) ->
            let mtl = snd (MTL.rad1 $(func) x)
                primop = PrimOp.diff ($func :: PrimOp.AD s Double -> PrimOp.AD s Double) x
             in V1 mtl ==~ V1 primop
      , bench "transformers" $ nf (snd . MTL.rad1 $func) (42.0 :: Double)
      , bench "primops" $ nf (PrimOp.diff ($func :: PrimOp.AD s Double -> PrimOp.AD s Double)) (42.0 :: Double)
      ]
    |]

showBody :: Exp -> String
showBody (LamE _ b) = showBody b
showBody e = pprint $ transformOnOf biplate uniplate unQualNames e

unQualNames :: Name -> Name
unQualNames = mkName . nameBase

mkGradBench :: ExpQ -> ExpQ -> ExpQ
mkGradBench func arg = do
  lab <- showBody <$> runQ func
  [|
    bgroup
      lab
      [ testProperty "transformers and primops returns almost the same answer" $
          \(x :: _ Double) ->
            let mtl = snd (MTL.grad $(func) x)
                primop =
                  PrimOp.grad
                    ($func :: _ (PrimOp.AD s Double) -> PrimOp.AD s Double)
                    x
             in mtl ==~ primop
      , bench "transformers" $ nf (snd . MTL.grad $func) ($arg :: _ Double)
      , bench "primops" $ nf (PrimOp.grad ($func :: _ (PrimOp.AD s Double) -> PrimOp.AD s Double)) ($(arg) :: _ Double)
      ]
    |]