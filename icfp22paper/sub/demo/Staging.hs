
module Staging (stage) where

import Common
import Syntax
import Evaluation (lvl2Ix)

data Env = Nil | Def0 Env Val0 | Def1 Env Val1

data Val1
  = VLam1 (Val1 -> Val1)
  | VQuote Val0
  | VSomeU1 -- ^ Meta-level types are irrelevant during staging, we evaluate them to this
            --   dummy value.
  | VZero1
  | VSuc1 Val1

data Val0
  = VVar0 Lvl
  | VApp0 Val0 Val0 Icit Verbosity
  | VPi0 Name Icit Val0 (Val0 -> Val0)
  | VLam0 Name Icit Val0 (Val0 -> Val0) Verbosity
  | VLet0 Name Val0 Val0 (Val0 -> Val0) Verbosity
  | VU0
  | VNat0
  | VZero0
  | VSuc0
  | VNatElim0

vVar1 :: Env -> Ix -> Val1
vVar1 (Def1 _ v) 0 = v
vVar1 (Def1 e v) x = vVar1 e (x - 1)
vVar1 (Def0 e v) x = vVar1 e (x - 1)
vVar1 _          _ = impossible

vApp1 :: Val1 -> Val1 -> Val1
vApp1 (VLam1 t) ~u = t u
vApp1 _          _ = impossible

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

eval1Bind :: Env -> Tm -> Val1 -> Val1
eval1Bind env t u = eval1 (Def1 env u) t

eval1 :: Env -> Tm -> Val1
eval1 env = \case
  Var x             -> vVar1 env x
  Lam x i a t o     -> VLam1 (eval1Bind env t)
  App t u i o       -> vApp1 (eval1 env t) (eval1 env u)
  Pi{}              -> VSomeU1
  Let _ _ _ t u _   -> eval1 (Def1 env (eval1 env t)) u
  Quote t           -> VQuote (eval0 env t)
  Lift{}            -> VSomeU1
  U{}               -> VSomeU1
  Wk t              -> eval1 (envTail env) t
  Nat _             -> VSomeU1
  Zero _            -> VZero1
  Suc _             -> VLam1 VSuc1
  NatElim _         -> VLam1 \_ -> VLam1 \s -> VLam1 \z -> VLam1 \t -> vNatElim1 s z t
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

eval0Bind :: Env -> Tm -> Val0 -> Val0
eval0Bind env t u = eval0 (Def0 env u) t

eval0 :: Env -> Tm -> Val0
eval0 env = \case
  Var x            -> vVar0 env x
  Lam x i a t o    -> VLam0 x i (eval0 env a) (eval0Bind env t) o
  App t u i o      -> VApp0 (eval0 env t) (eval0 env u) i o
  Pi x i a b       -> VPi0 x i (eval0 env a) (eval0Bind env b)
  Let _ x a t u v  -> VLet0 x (eval0 env a) (eval0 env t) (eval0Bind env u) v
  U _              -> VU0
  Splice t         -> vSplice (eval1 env t)
  Wk t             -> eval0 (envTail env) t
  Nat _            -> VNat0
  Zero _           -> VZero0
  Suc _            -> VSuc0
  NatElim _        -> VNatElim0
  InsertedMeta m _ -> metaError m
  Meta m           -> metaError m
  Quote{}          -> impossible
  Lift{}           -> impossible
  AppPruning{}     -> impossible

quote0 :: Lvl -> Val0 -> Tm
quote0 l = \case
  VVar0 x         -> Var (lvl2Ix l x)
  VApp0 t u i o   -> App (quote0 l t) (quote0 l u) i o
  VPi0 x i a b    -> Pi x i (quote0 l a) (quote0 (l + 1) (b $ VVar0 l))
  VLam0 x i a t o -> Lam x i (quote0 l a) (quote0 (l + 1) (t $ VVar0 l)) o
  VLet0 x a t u v -> Let S0 x (quote0 l a) (quote0 l t) (quote0 (l + 1) (u $ VVar0 l)) v
  VU0             -> U S0
  VNat0           -> Nat S0
  VZero0          -> Zero S0
  VSuc0           -> Suc S0
  VNatElim0       -> NatElim S0

stage :: Tm -> Tm
stage t = quote0 0 $ eval0 Nil t
