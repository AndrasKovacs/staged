
module Staging (stage) where

import Common
import Syntax
import Evaluation (lvl2Ix)

data Env = Nil | Def0 Env Val0 | Def1 Env Val1

data Closure = Closure Env Tm

data Val1
  = VLam1 {-# unpack #-} Closure
  | VQuote Val0
  | VSomeU1 -- ^ Meta-level types are irrelevant during staging, we evaluate them to this
            --   dummy value.
  | VZero1
  | VSuc1 Val1

data Val0
  = VVar0 Lvl
  | VApp0 Val0 Val0 Icit Verbosity
  | VPi0 Name Icit Val0 {-# unpack #-} Closure
  | VLam0 Name Icit Val0 {-# unpack #-} Closure Verbosity
  | VLet0 Name Val0 Val0 {-# unpack #-} Closure
  | VU0
  | VNat0
  | VZero0
  | VSuc0 Val0
  | VNatElim0 Val0 Val0 Val0 Val0

vVar1 :: Env -> Ix -> Val1
vVar1 (Def1 _ v) 0 = v
vVar1 (Def1 e v) x = vVar1 e (x - 1)
vVar1 (Def0 e v) x = vVar1 e (x - 1)
vVar1 _          _ = impossible

vApp1 :: Val1 -> Val1 -> Val1
vApp1 (VLam1 (Closure e t)) ~u = eval1 (Def1 e u) t
vApp1 _                      _ = impossible

envTail :: Env -> Env
envTail (Def1 e _) = e
envTail (Def0 e _) = e
envTail _          = impossible

metaError :: MetaVar -> a
metaError m = error (
    "Unsolved metavariable during staging: ?" ++ show m
  ++ "\nHint: look at elab-verbose output to find the offending metavariable")

vNatElim1 :: Val1 -> Val1 -> Val1 -> Val1
vNatElim1 s z t = case t of
  VZero1  -> z
  VSuc1 t -> s `vApp1` t `vApp1` vNatElim1 s z t
  _       -> impossible

eval1 :: Env -> Tm -> Val1
eval1 env = \case
  Var x             -> vVar1 env x
  Lam x i a t o     -> VLam1 (Closure env t)
  App t u i o       -> vApp1 (eval1 env t) (eval1 env u)
  Pi{}              -> VSomeU1
  Let _ _ _ t u     -> eval1 (Def1 env (eval1 env t)) u
  Quote t           -> VQuote (eval0 env t)
  Lift{}            -> VSomeU1
  U{}               -> VSomeU1
  Wk t              -> eval1 (envTail env) t
  Nat _             -> VSomeU1
  Zero _            -> VZero1
  Suc _ t           -> VSuc1 (eval1 env t)
  NatElim _ _ s z t -> vNatElim1 (eval1 env s) (eval1 env z) (eval1 env t)
  InsertedMeta m _  -> metaError m
  Meta m            -> metaError m
  AppPruning{}      -> impossible
  Splice{}          -> impossible

vVar0 :: Env -> Ix -> Val0
vVar0 (Def0 _ v) 0 = v
vVar0 (Def0 e v) x = vVar0 e (x - 1)
vVar0 (Def1 e v) x = vVar0 e (x - 1)
vVar0 _          _ = impossible

vSplice :: Val1 -> Val0
vSplice (VQuote v) = v
vSplice _          = impossible

eval0 :: Env -> Tm -> Val0
eval0 env = \case
  Var x             -> vVar0 env x
  Lam x i a t o     -> VLam0 x i (eval0 env a) (Closure env t) o
  App t u i o       -> VApp0 (eval0 env t) (eval0 env u) i o
  Pi x i a b        -> VPi0 x i (eval0 env a) (Closure env b)
  Let _ x a t u     -> VLet0 x (eval0 env a) (eval0 env t) (Closure env u)
  U _               -> VU0
  Splice t          -> vSplice (eval1 env t)
  Wk t              -> eval0 (envTail env) t
  Nat _             -> VNat0
  Zero _            -> VZero0
  Suc _ t           -> VSuc0 (eval0 env t)
  NatElim _ p s z t -> VNatElim0 (eval0 env p) (eval0 env s) (eval0 env z) (eval0 env t)
  InsertedMeta m _  -> metaError m
  Meta m            -> metaError m
  Quote{}           -> impossible
  Lift{}            -> impossible
  AppPruning{}      -> impossible


($$) :: Closure -> Val0 -> Val0
($$) (Closure env t) u = eval0 (Def0 env u) t

quote0 :: Lvl -> Val0 -> Tm
quote0 l = \case
  VVar0 x           -> Var (lvl2Ix l x)
  VApp0 t u i o     -> App (quote0 l t) (quote0 l u) i o
  VPi0 x i a b      -> Pi x i (quote0 l a) (quote0 (l + 1) (b $$ VVar0 l))
  VLam0 x i a t o   -> Lam x i (quote0 l a) (quote0 (l + 1) (t $$ VVar0 l)) o
  VLet0 x a t u     -> Let S0 x (quote0 l a) (quote0 l t) (quote0 (l + 1) (u $$ VVar0 l))
  VU0               -> U S0
  VNat0             -> Nat S0
  VZero0            -> Zero S0
  VSuc0 t           -> Suc S0 (quote0 l t)
  VNatElim0 p s z t -> NatElim S0 (quote0 l p) (quote0 l s) (quote0 l z) (quote0 l t)

stage :: Tm -> Tm
stage t = quote0 0 $ eval0 Nil t
