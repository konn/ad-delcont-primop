# ad-delcont-primop

An attempt to implement Reverse-Mode AD in terms of delcont primops introduced in GHC 9.6.

That is, it reimplements [`ad-delcont`][ad-delcont], which translates Scala implementation of [Backpropagation with Continuation Callbacks][cc-differ], in terms of `newPromptTag#`, `prompt#`, and `control0#`.

## Performance

### Summary

- In computing multivariate gradients: in most cases, our implementation is at most slightly faster than Edward Kmett's [`ad`][ad].
  In some cases, ours is 4x-10x faster.
- To differentiate univariate functions, always use [`ad`][ad] as it uses Forward-mode.
- Our implementation in most cases outperforms [`backprop`][backprop] and [`ad-delcont`][ad-delcont] (monad transformer-based impl).

### Legends

- `transformers`: [`ad-delcont`][ad-delcont]
- `ad`: [`ad`][ad], generic functions from `Numeric.AD`
- `ad/double`: [`ad`][ad], `Double`-specialised functions provided in `Numeric.AD.Double`.
- `backprop`: [`backprop`][backprop]
- `primop`: our generic implementation.
- `primop/double`: our implementation specialised for `Double`.

### Univariate Differentiation

#### Identity Function: $f(x) = x$

![ad wins](./bench-results/univariate/00.svg)

#### Binomial: $(x + 1)(x + 1)$

![ad wins](./bench-results/univariate/01.svg)

#### Gauß-like: $x e^{x^2 + 1}$

![ad wins](./bench-results/univariate/02.svg)

### Bivariate

#### Addition: $f(x, y) = x + y$

![we win by 10!](./bench-results/bivariate/00.svg)

#### Trigonometrics: $f(x,y) = \sin x \cos y (x^2 + y)$

![we are 4x faster!](./bench-results/bivariate/01.svg)

#### Exponentials: $f(x, y) = y e^{x^2 + y}$

![still 4x faster!](./bench-results/bivariate/02.svg)

#### Exponentials and Trigonometrics: $f(x, y) = (x \cos x + y)^2 e^{x \sin (x + y^2 + 1)}$

![twice as fast](./bench-results/bivariate/03.svg)

#### Complex formula

$$
f(x, y) = (\tanh (e^y  \cosh x) + x ^ 2) ^ 3 - (x \cos x + y) ^ 2 e^{x \sin (x + y ^2 + 1)}
$$

![1.5x fast](./bench-results/bivariate/04.svg)

### Trivariate

#### Multiplication: $f(x,y,z) = xyz$

![10x fast](./bench-results/trivariate/00.svg)

#### Complex

$$
  (\tanh (e^{y + z ^ 2} \cosh x) + x ^ 2) ^ 3 - (x (z ^ 2 - 1) \cos x + y)^{2z} e^{x  \sin (x + yzx + 1)}
$$

![1.5x fast](./bench-results/trivariate/01.svg)

### 4-ary (quadrivariate)

#### Multiplication: $f(x,y,z,w) = xyzw$

![10x fast](./bench-results/4-ary/00.svg)

#### Trigonometrics: $f(x,y,z,w) =  (x + w) ^ 4 \exp(x + \cos (y ^ 2 \sin z) w)$

![thrice as fast](./bench-results/4-ary/01.svg)

#### Some logarithm

$$
  f(x,y,z,w) =  \log (x ^ 2 + w) / \log (x + w) ^ 4 \exp (x + \cos (y ^ 2 \sin z) w)
$$

![twice as fast](./bench-results/4-ary/02.svg)

#### Some more logarithm

$$
  f(x,y,z,w) =  \log_{x ^ 2 + w}(\cos (x ^ 2 + 2 z) + w + 1) ^ 4 \exp (x + \sin (\pi x) \cos ((e^y) ^ 2 \sin z) w)
$$

![slightly faster](./bench-results/4-ary/03.svg)

#### Really complex

$$
  f(x,y,z,w) = \log_{x ^ 2 + \tanh w} (\cos (x ^ 2 + 2z) + w + 1) ^ 4 + \exp (x + \sin (\pi x + w ^ 2) \cosh ((e^y)^ 2 \sin z) ^ 2 (w + 1))
$$

![slightly faster](./bench-results/4-ary/04.svg)

## TODOs

- :checkmark: Explore more fine-grained use of delcont
  + See `Numeric.AD.DelCont.MultiPrompt` for PoC
  + We can abolish refs except for the ones for the outermost primitive variables
    * perhaps coroutine-like hack can eliminateThis
  + This implementation, however, is not as efficient as STRef-based in terms of time
    * This is because each continuation allocates different values rather than single mutable variable
    * But still in some cases, allocation can be slightly reduced by this approach (need confirmation)
    * In particular, as the # of variable increases, the time overhead seems decaying and allocation becomes slightly fewer
- Avoids (indirect) references at any costs!
- ~~Remove `Ref`s from constants~~
  + This increases both runtime and allocation by twice (see [the benchmark log][const-ref-log])
  + Branching overhead outweighs

[const-ref-log]: https://github.com/konn/ad-delcont-primop/actions/runs/3924787010/jobs/6709300040

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
