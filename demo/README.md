
## Demo implementation of staged compilation with two-level type theory

This package serves as supplement and demonstration to the preprint "Staged Compilation With Two-Level Type Theory". It contains a command-line
tool for elaborating and staging input programs, and several example files. Short summary of features:
- Two stages, a compile time and a runtime one.
- We have dependent functions, type-in-type and natural numbers at both stages. We can use type-in-type to lambda-encode a wide variety of data types for demo purposes. The built-in natural numbers are used for examples where proper dependent elimination is needed.
- Agda-style implicit functions, with decent higher-order unification.
- Strong inference for staging operations ("annotations"), using coercive subtyping and bidirectional elaboration.

#### Installation

First, clone or download this repository.

Using `stack`:
- Install [stack](https://docs.haskellstack.org/en/stable/README/).
- Run `stack install` in the smalltt directory. If you have LLVM installed, use
   `stack install --flag smalltt:llvm` instead, that gives some performance
   boost.

Using `cabal`:
- Install [cabal](https://www.haskell.org/cabal/)
- Run `cabal v2-update`.
- Run `cabal v2-install` in the smalltt directory. 

Also make sure that the executable is on the PATH. On Linux-es, the `stack`
install directory is `$HOME/.local/bin`, and the `cabal` one is
`$HOME/.cabal/bin`. Installation provides you the `2ltt` executable.

#### Usage

`2ltt` reads a single expression from the standard input, so the typical use case is to pipe a file to `2ltt`, as
in `cat file.2ltt | 2ltt COMMAND`. The following commands are available:
- `elab`: prints elaboration output. This prints all inferred staging operations, but does not print anything inserted by general inference, namely type annotations, implicit arguments and implicit lambdas.
- `elab-verbose`: print elaboration output, showing almost all data inserted by elaboration. It also lists all metavariables and their solutions in the preface of the output.
- `stage`: print staging output, don't show inserted implicits and annotations.
- `stage-verbose`: print staging output, show inserted implicits and annotations.
- `nf`: print the verbose normal form of the input program, together with its type.
- `type`: print the verbose normal type of the input program.

#### Tutorial

See [examples/Basics.2ltt](examples/Basics.2ltt) for a tutorial on language features and usage.

#### Examples

- [examples/FoldrFusion.2ltt](examples/FoldrFusion.2ltt): staged foldr/build fusion for lists.
- [examples/STLC.2ltt](examples/STLC.2ltt): well-typed compile-time interpretation for a simply typed lambda calculus. It converts deeply embedded static syntax to shallowly embedded code in the staging output.
- [examples/Vectors.2ltt](examples/Vectors.2ltt): length-indexed vectors with statically known length, represented as iterated pairs. Also includes a demonstration of encapsulating let-insertion in a monad.

-----
