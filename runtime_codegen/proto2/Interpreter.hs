
module Interpreter where

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

import Data.IORef

data Env = Nil | Def Env Val

envLen :: Env -> Lvl
envLen = go 0 where
  go acc Nil = acc
  go acc (Def e _) = go (acc + 1) e

data Spine = SId | SApp Spine Val | SSplice Spine

data Closure = Closure Env Tm

data Val
  = VNe Lvl Spine
  | VLam Name Icit {-# unpack #-} Closure
  | VPi Name Icit Val {-# unpack #-} Closure
  | VEffect {-# unpack #-} Closure
  | VQuote Val
  | VU
  | VRef (IORef Val)
  | VTt
  | VBox Val

lookupIx :: Env -> Ix -> Val
lookupIx e x = case (e, x) of
  (Def _ v, 0) -> v
  (Def e _, x) -> lookupIx e (x - 1)
  _            -> impossible

inst_pc :: Closure -> Val -> Val
inst_pc (Closure e t) u = ceval (Def e u) t


-- pure closed evaluation
ceval :: Env -> Tm -> Val
ceval e = \case
  Var x       -> lookupIx e x
  TopVar x    -> lookupIx e (lvl2Ix (envLen e) x)
  App t u _   -> case ceval e t of
                  VLam _ _ t -> inst_pc t (ceval e u)
                  _          -> impossible
  Lam x i t   -> VLam x i (Closure e t)
  U           -> VU
  Pi x i a b  -> VPi x i (ceval e a) (Closure e b)
  Let x a t u -> ceval (Def e (ceval e t)) u
  Box t       -> VBox (ceval e t)
  Quote t     -> undefined -- switch to open eval

  PostponedCheck{} -> impossible
  Meta{}           -> impossible
  AppPruning{}     -> impossible

inst_ec :: Closure -> Val -> IO Val
inst_ec (Closure e t) u = cexec (Def e u) t

-- Eff closed evaluation
cexec :: Env -> Tm -> IO Val
cexec e = \case
  Var x       -> case lookupIx e x of
                   VEffect (Closure e' t) -> cexec e' t
                   _                      -> impossible
  TopVar x    -> case lookupIx e (lvl2Ix (envLen e) x) of
                   VEffect (Closure e' t) -> cexec e' t
                   _                      -> impossible
  Return t    -> pure $! ceval e t
  Bind x t u  -> do {t <- cexec e t; cexec (Def e t) u}
  Let x a t u -> cexec (Def e (ceval e t)) u
  App t u _   -> case ceval e t of
                   VLam _ _ t -> inst_ec t (ceval e u)
                   _          -> impossible
  New t       -> VRef <$!> (newIORef $! ceval e t)
  Read t      -> case ceval e t of
                   VRef r -> readIORef r
                   _      -> impossible
  Write t u   -> case ceval e t of
                   VRef r -> do {writeIORef r $! ceval e u; pure VTt}
                   _      -> impossible
  Splice t    -> case ceval e t of
                   VQuote t -> undefined -- generate code from "t" (NOTE: can be open code referring to top vars!)
                   _        -> impossible
  _           -> impossible
