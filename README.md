# monad-arith

Arithmetic lifted to Functor, Applicative, and Monad

## Overview

This library defines the `Arithmetic` newtype, which wraps a functor and lifts arithmetic type class instances (such as those that define `+`) to make writing numeric monadic code easier.

It also defines a few lifted type classes (e.g., `EqM` and `RealM`) as some type classes require value extraction, which can't be done for most monads.

Alternatively, you may import the sole `Num` instance for all `Applicative` functors using `Control.Arithmetic.OrphanNum`, which may be more convenient than wrapping and unwrapping them in `Arithmetic`, but this module and `Control.Arithmetic` are incompatible.

## Modules

- `Control.Applicative.Arithmetic`
- `Control.Applicative.Arithmetic.OrphanNum`

## Installation

Add `monad-arith` to your project's dependencies in your `.cabal` file:

```
build-depends:      monad-arith
```

Then run:

```
stack build
```

## License

BSD-3-Clause. See the `LICENSE` file for details.

## Author

axionbuster
