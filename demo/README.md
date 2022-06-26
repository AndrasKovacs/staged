
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
- Run `stack install` in the directory.

Using `cabal`:
- Install [cabal](https://www.haskell.org/cabal/).
- Run `cabal v2-update`.
- Run `cabal v2-install` in the directory.

Also make sure that the executable is on the PATH. On Linux-es, the `stack`
install directory is `$HOME/.local/bin`, and the `cabal` one is
`$HOME/.cabal/bin`. Installation provides you the `2ltt` executable.

#### Usage

`2ltt` reads a single expression from the standard input, so the typical use case is to pipe a file to `2ltt`, as
in `cat file.2ltt | 2ltt COMMAND`. The following commands are available:
- `elab`: prints elaboration output. This prints all inferred staging operations, but does not print anything inserted by general inference, namely type annotations, implicit arguments and implicit lambdas.
- `elab-verbose`: prints elaboration output, showing almost all data inserted by elaboration. It also lists all metavariables and their solutions in the preface of the output. Metavariables that are inserted in the code are displayed as `?x(..)`, where `x` is a natural number denoting a metavariable, and `(..)` is a shorthand for the application of `x` to all bound variables in the local scope.
- `stage`: prints staging output, doesn't show inserted implicits and annotations.
- `stage-verbose`: prints staging output, shows inserted implicits and annotations.
- `nf`: prints the verbose normal form of the input program, together with its type.
- `type`: prints the verbose normal type of the input program.

#### Tutorial

See [examples/Tutorial.2ltt](examples/Tutorial.2ltt) for a tutorial on language features and usage.

#### Examples

- [examples/FoldrFusion.2ltt](examples/FoldrFusion.2ltt): staged foldr/build fusion for lists.
- [examples/STLC.2ltt](examples/STLC.2ltt): well-typed compile-time interpretation for a simply typed lambda calculus. It converts deeply embedded static syntax to shallowly embedded code in the staging output.
- [examples/Vectors.2ltt](examples/Vectors.2ltt): length-indexed vectors with statically known length, represented as iterated pairs. Also includes a demonstration of encapsulating let-insertion in a monad.

-----

### Comparison to the preprint

- The preprint has a countable hierarchy of universes at each stage, closed under pi and sigma types and natural numbers. In contrast, the demo
here has type-in-type, and does not have sigma types. Type-in-type makes implementation much easier, and allows us to lambda-encode a variety of
type formers (without dependent elimination). We can also encode unit and sigma types, without eta-rules, but this is sufficient for examples.
- The preprint writes splicing as `~t`, while the demo has it as `[t]`. The latter is much easier to parse and print than the paper's notation, and it's not much more verbose. Unfortunately, `[_]` is not viable in the paper because it's already severely overloaded there.

### Notes on implementation

- Elaboration is bidirectional, and uses normalization-by-evaluation for computation.
- We use contextual metavariables. These are outside of basic 2LTT, because they are able to abstract over variables with arbitrarily mixed stages, while proper 2LTT type formers can't cross between stages. In the implementation, I reuse ordinary `App` and `Pi` and `Lam` constructors for contextual metas, but formally they are distinct from 2LTT constructions. Fortunately, the different usages can be always disambiguated in this demo. If all metavariables are solved, then they can be inlined ("zonked"), and then we get syntax which is purely in a 2LTT.
- Implicit functions follow Agda conventions and notation. We use pattern unification with a number of extensions: pruning, inessential nonlinearity, intersections, eta-expansion for metavariables (to get rid of splicing, similarly to when we get rid of sigma projections), and spine inversion modulo quote/splice (with quote/splice viewed as construction/projection for a unary record type). These extra features are documented [here](http://www2.tcs.ifi.lmu.de/~abel/unif-sigma-long.pdf). We don't use postponed constraints though.
- Staging ([Staging.hs](Staging.hs)) follows the optimization notes in Section 4.4. of the paper. Meta-level evaluation is purely syntax-directed and closed, and we additionally erase types during evaluation (replacing them with a dummy value), because they are irrelevant in staging output. Object-level evaluation is simply an implementation of delayed variable renamings, using closures and De Bruijn levels.
- Stages must be unambiguous in source programs; there are no stage metavariables nor stage unification in the implementation. However, this does not seem to make user experience any worse. I had previous prototypes with stage metavariables, and it turned out to be an unnecessary and rarely useful complication. The main point of stage ambiguity is actually the `let`-definitions, and I have found that if `let`-definitions always have explicit stages, then the rest of inference becomes highly effective. In summary, we always want to disambiguate `let`, but if we do so, stage metavariables are unnecessary.
- We use a combination of bidirectional elaboration rules and coercive subtyping to infer staging operations. Bidirectional elaboration can go under quotes, or insert a quote when we are checking a non-quoted term with a lifted type. Coercive subtyping uses `A <= Lift A`, `Lift A <= A` and `U0 <= U1` together with a contravariant-covariant rule for functions. We also try to eliminate unnecessary coercions; for example the naive `(Nat0 -> Nat0) <= (Nat0 -> Nat0)` inserts an unnecessary eta-expansion, but our version keeps track of trivial coercions, and omits them in elaboration output.

### Summary of source files

I do a summary roughly in the order one should read the files, to get a picture of the implementation, following
the order of module dependencies and the elaboration pipeline.

- [Common.hs](Common.hs): miscellaneous definitions, names, De Bruijn indices/levels.
- [Presyntax.hs](Presyntax.hs): defines the type of raw terms, which is the output of parsing and the input of elaboration.
- [Parser.hs](Parser.hs): the parser.
- [Syntax.hs](Syntax.hs): the definition of core syntax.
- [Value.hs](Value.hs): the type of semantic values used in unification and evaluation.
- [Metacontext.hs](Metacontext.hs): this contains a *mutable* top-level state for metavariables, as an `IntMap` in an `IORef`. During unification this
  state can be modified. Each `2ltt` command performs a single run of elaboration, so we don't have to re-initialize the state at any point.
- [Evaluation.hs](Evaluation.hs): contains the evaluator used in *conversion checking*, and also the quotation to normal forms and the "zonking" operation which inlines solved metas. Evaluation computes all redexes (both meta- and object-level redexes).
- [Pretty.hs](Pretty.hs): pretty printing.
- [Cxt.hs](Cxt.hs): the elaboration context + ways to add things to the context.
- [Errors.hs](Errors.hs): the type of errors and the corresponding pretty printing function.
- [Unification.hs](Unification.hs): unification.
- [Elaboration.hs](Elaboration.hs): definitions of bidirectional elaboration and subtyping coercions.
- [Staging.hs](Staging.hs): the staging algorithm.
- [MainInteraction.hs](MainInteraction.hs): main functions, command line option
  processing. This is separate from [Main.hs](Main.hs) to allow the main
  interaction to be separately imported.
- [Tests.hs](Tests.hs): some ad-hoc tests for elaboration that I used to weed out inference/subtyping bugs.
- [Main.hs](Main.hs): the entry point to `2ltt`.
