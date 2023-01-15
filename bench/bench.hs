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
        , $(mkGradBench [|\(V2 x y) -> (x * cos x + y) ^ 2 * exp (x * sin (x + y * y + 1))|] [|V2 42 52|])
        , $(mkGradBench [|\(V2 x y) -> (tanh (exp y * cosh x) + x ^ 2) ^ 3 - (x * cos x + y) ^ 2 * exp (x * sin (x + y * y + 1))|] [|V2 42 52|])
        ]
    , bgroup
        "trivariate"
        [ $(mkGradBench [|\(V3 x y z) -> x * y * z|] [|V3 4 5 6|])
        , $(mkGradBench [|\(V3 x y z) -> (tanh (exp (y + z ^ 2) * cosh x) + x ^ 2) ^ 3 - (x * (z ^ 2 - 1) * cos x + y) ** (2 * z) * exp (x * sin (x + y * z * x + 1))|] [|V3 4 5 6|])
        ]
    , bgroup
        "4-ary"
        [ $(mkGradBench [|\(V4 x y z w) -> x * y * z * w|] [|V4 4 5 6 7|])
        , $(mkGradBench [|\(V4 x y z w) -> (x + w) ^ 4 * exp (x + cos (y ^ 2 * sin z) * w)|] [|V4 4 5 6 7|])
        , $(mkGradBench [|\(V4 x y z w) -> log (x ^ 2 + w) / log (x + w) ^ 4 * exp (x + cos (y ^ 2 * sin z) * w)|] [|V4 4 5 6 7|])
        ]
    ]
