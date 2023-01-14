# ad-delcont-primop

An attempt to implement Reverse-Mode AD in terms of delcont primops introduced in GHC 9.6.

That is, reimplementing [`ad-delcont`][ad-delcont], which translates Scala implementation of [Backpropagation with Continuation Callbacks][cc-differ], in terms of `newPromptTag#`, `prompt#`, and `control0#`.

## TODOs

- Benchmarks
- Explore more fine-grained use of delcont

## References

- Marco Zocca: [_ad-delcont: Reverse-mode automatic differentiation with delimited continuations_][ad-delcont]
- Fei Wang et al.: [_Backpropagation with Continuation Callbacks: Foundations for Efficient and Expressive Differentiable Programming_][cc-differ]
- Justin Le: [_backprop: Heterogeneous automatic differentation_][backprop]
- Edward Kmett: [_ad: Automatic Differentiation_][ad]
- The GHC Team: [_``Continuations'' section in GHC.Prim document for GHC 9.6_][cont-ghc-prim]
- R. K. Dybvig et al.: [_A Monadic Framework for Delimited Continuations_][monadic-delcont]

[ad-delcont]: https://hackage.haskell.org/package/ad-delcont
[cc-differ]: https://papers.nips.cc/paper/2018/file/34e157766f31db3d2099831d348a7933-Paper.pdf
[backprop]: https://backprop.jle.im
[ad]: https://hackage.haskell.org/package/ad
[cont-ghc-prim]: https://ghc.gitlab.haskell.org/ghc/doc/libraries/ghc-prim-0.10.0/GHC-Prim.html#continuations
[monadic-delcont]: https://legacy.cs.indiana.edu/~dyb/pubs/monadicDC.pdf
