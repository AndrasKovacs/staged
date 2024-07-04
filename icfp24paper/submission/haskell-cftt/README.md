
## haskell-cftt

This package adapts the paper "Closure-Free Functional Programming in a
Two-Level Type Theory" to typed Template Haskell.

Installation:

- install the Haskell stack: https://docs.haskellstack.org/en/stable/
- hit `stack build` or `stack ghci` in the terminal, in the package folder

Overview & differences to the paper:

- We use `Up.hs` to define the type lifting operation as `Up`, and also some
  convenience, like overloading number literals and arithmetic operations for
  `Up`.
- We don't distinguish value and computation types, because GHC doesn't either.
  Hence, code output is not necessarily closure-free, especially monadic code
  with join points.
- In `Pull.hs`, the general definition for `forEach` is not typeable in Haskell.
  Instead, I only defined `forEach` for `Up a`, and provide a `CasePull` class
  which implements splitting on object values for each object type. This could
  be potentially automated in the future, using more low-level untyped
  templates. For now I wanted to stick to typed templates only.

There are examples in `Examples.hs` file. There, I provided a `printUp` function
with which code can be printed.  However, this yields a massive amount of noise
with fully qualified names, and is also prone to mangling indentation.

My preferred approach is the following:

- I build with `stack build --flag haskell-cftt:dump --ghc-options=-O0`. This
  will dump GHC core files in a reasonably readable format.
- The files can be found in `.stack-work/dist/OS/ghc-VERSION/build/CFTT`, where
  the "OS" and "VERSION" may vary on your system. Here, you can search for the
  functions that you want to view.
- You can change the `-O0` to `-O1` to view optimized output. Optimization is
  especially impactful for stream code, where it cleans up the tuple-encoding of
  mutual recursion.
