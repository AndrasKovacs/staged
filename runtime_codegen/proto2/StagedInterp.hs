
module StagedInterp where

{-

Evaluators:
  - Effectful closed
  - Pure open


Runtime objects and environments are shared between all evaluators.

In the native backend we'll ensure that every closed value can be zero-cost cast
to an open value.

  - ADT-s: open values have an extra neutral constructor
  - Closures: likewise
    - Closures store *both* a closed and an open code pointer plus the env.
      But the open code could be stored zero-cost for closed eval, e.g. in
      "tables-next-to-code". Function binder names as well.

- We start closed evaluation in Eff
- From there, if we go under a quote, we switch to open eval and we stay there.
- Open eval:
  - We keep track of stages.
  - At stage 0 we do all beta-reduction
  - At stage suc we do no beta-reduction

- Should we have different value repr in stage 0 and suc?

main : IO

let foo = <\x y z. ... ~(...) >


-}

import Common
import Syntax

data Env = Nil | Def Env Val

data Spine = SId | SApp Spine Val | SSplice Spine

data Closure = Closure Env Tm

data Val
  = VNe Lvl Spine
  | VLam Name Closure
  | VQuote Val
