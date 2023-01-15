{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Linear (V2 (..), V3 (..), V4 (..))
import Macros
import Test.Tasty.Bench

main :: IO ()
main =
  defaultMain
    [ bgroup "univariate" $
        [ $(mkDiffBench [|id|])
        , $(mkDiffBench [|\x -> (x + 1) * (x + 1)|])
        , $(mkDiffBench [|\x -> x * exp (x * x + 1)|])
        ]
    , bgroup
        "bivariate"
        [ $(mkGradBench [|\(V2 x y) -> x * y|] [|V2 (42 :: Double) 53|])
        , $(mkGradBench [|\(V2 x y) -> sin x * cos y * (x ^ 2 + y)|] [|V2 42 54|])
        , $(mkGradBench [|\(V2 x y) -> y * exp (x * x + y)|] [|V2 42 52|])
        ]
    ]
