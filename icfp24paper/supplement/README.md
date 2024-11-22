
## Supplement to the paper "Closure-Free Functional Programming in a Two-Level Type Theory"

This supplement consists of three separate artifacts:

- `agda-cftt`: implements the paper in Agda, using a precise embedding of the
   object language as postulated higher-order abstract syntax. In particular,
   computation and value types are tracked.
- `haskell-cftt`: implements the paper in typed Template Haskell. Does not
   track computation and value types.
- `agda-opsem`: formalizes a syntax and operational semantics for the object
   theory and develops some basic metatheory for it.
- `agda-ifip-tailcall`: not discussed in the paper. Extends `agda-cftt` with monadic tail calls and effectful streams.

In each folder, build instructions and further documentation can be found in the
local README-s and the source files.
