{-# options_ghc -funbox-strict-fields -Wno-unused-top-binds -Wno-unused-imports #-}
{-# language ConstraintKinds, ImplicitParams, RankNTypes #-}

module Staging (stage) where

import Common
import Syntax
import Evaluation (lvl2Ix)

data Env = Nil | Def0 Env Lvl | Def1 Env Val1
  deriving (Eq, Show)

type LvlArg = (?lvl :: Lvl)

data Cl1
  = CEval1 Env Tm
  | CSuc
  | CNatElim0
  | CNatElim1
  | CNatElim2 Val1
  | CNatElim3 Val1 Val1
  deriving (Eq, Show)

data Val1
  = VLam1 Cl1
  | VQuote Val0
  | VSomeU1 -- ^ Meta-level types are irrelevant during staging, we evaluate them to this
            --   dummy value.
  | VZero1
  | VSuc1 Val1
  deriving (Eq, Show)

data EWk = EWk Lvl Val0
  deriving (Eq, Show)

data Val0
  = VVar0 Lvl
  | VApp0 Val0 Val0 Icit Verbosity
  | VPi0 Name Icit Val0 EWk
  | VLam0 Name Icit Val0 EWk Verbosity
  | VLet0 Name Val0 Val0 EWk Verbosity
  | VU0
  | VNat0
  | VZero0
  | VSuc0
  | VNatElim0
  deriving (Eq, Show)

vVar1 :: Env -> Ix -> Val1
vVar1 (Def1 _ v) 0 = v
vVar1 (Def1 e v) x = vVar1 e (x - 1)
vVar1 (Def0 e v) x = vVar1 e (x - 1)
vVar1 _          _ = impossible

capp :: LvlArg => Cl1 -> Val1 -> Val1
capp c ~u = case c of
  CEval1 e t    -> eval1 (Def1 e u) t
  CSuc          -> VSuc1 u
  CNatElim0     -> VLam1 CNatElim1
  CNatElim1     -> VLam1 (CNatElim2 u)
  CNatElim2 s   -> VLam1 (CNatElim3 s u)
  CNatElim3 s z -> vNatElim1 s z u

vApp1 :: LvlArg => Val1 -> Val1 -> Val1
vApp1 (VLam1 t) ~u = capp t u
vApp1 _          _ = impossible

envTail :: Env -> Env
envTail (Def1 e _) = e
envTail (Def0 e _) = e
envTail _          = impossible

metaError :: MetaVar -> a
metaError m = error (
    "Unsolved metavariable during staging: ?" ++ show m
  ++ "\nHint: look at elab-verbose output to find the offending metavariable")

vNatElim1 :: LvlArg => Val1 -> Val1 -> Val1 -> Val1
vNatElim1 s z t = case t of
  VZero1  -> z
  VSuc1 t -> s `vApp1` t `vApp1` vNatElim1 s z t
  _       -> impossible

eval1 :: LvlArg => Env -> Tm -> Val1
eval1 env = \case
  Var x             -> vVar1 env x
  Lam x i a t o     -> VLam1 (CEval1 env t)
  App t u i o       -> vApp1 (eval1 env t) (eval1 env u)
  Pi{}              -> VSomeU1
  Let _ _ _ t u _   -> eval1 (Def1 env (eval1 env t)) u
  Quote t           -> VQuote (eval0 env t)
  Lift{}            -> VSomeU1
  U{}               -> VSomeU1
  Wk t              -> eval1 (envTail env) t
  Nat _             -> VSomeU1
  Zero _            -> VZero1
  Suc _             -> VLam1 CSuc
  NatElim _         -> VLam1 CNatElim0
  InsertedMeta m _  -> metaError m
  Meta m            -> metaError m
  AppPruning{}      -> impossible
  Splice{}          -> impossible

vVar0 :: Env -> Ix -> Val0
vVar0 (Def0 _ v) 0 = VVar0 v
vVar0 (Def0 e v) x = vVar0 e (x - 1)
vVar0 (Def1 e v) x = vVar0 e (x - 1)
vVar0 _          _ = impossible

vSplice :: Val1 -> Val0
vSplice (VQuote v) = v
vSplice _          = impossible

fresh :: (LvlArg => Lvl -> a) -> (LvlArg => a)
fresh act = let x = ?lvl in
            let ?lvl = x + 1 in
            act x

eval0Bind :: LvlArg => Env -> Tm -> EWk
eval0Bind env t =
  -- traceShow ("MKEWK", ?lvl, t)
  EWk ?lvl $ fresh \x -> eval0 (Def0 env x) t

eval0 :: LvlArg => Env -> Tm -> Val0
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

type RenArg = (?ren :: [Lvl])

-- | Trim list to given length.
trim :: Int -> [a] -> [a]
trim l as = drop (length as - l) as

quote0EWk :: LvlArg => RenArg => EWk -> Tm
quote0EWk (EWk l t) = fresh \x ->
  let ?ren = x : trim (coerce l) ?ren
  in quote0 t

applyRen :: RenArg => Lvl -> Lvl
applyRen x =
  -- traceShow ("APPLYREN", ?ren, x) $
  ?ren !! (length ?ren - coerce x - 1)

quote0 :: LvlArg => RenArg => Val0 -> Tm
quote0 = \case
  VVar0 x         -> Var (lvl2Ix ?lvl (applyRen x))
  VApp0 t u i o   -> App (quote0 t) (quote0 u) i o
  VPi0 x i a b    -> Pi x i (quote0 a) (quote0EWk b)
  VLam0 x i a t v -> Lam x i (quote0 a) (quote0EWk t) v
  VLet0 x a t u v -> Let S0 x (quote0 a) (quote0 t) (quote0EWk u) v
  VU0             -> U S0
  VNat0           -> Nat S0
  VZero0          -> Zero S0
  VSuc0           -> Suc S0
  VNatElim0       -> NatElim S0

stage :: Tm -> Tm
stage t =
  let ?lvl = 0
      ?ren = []
  in quote0 $ eval0 Nil t


{-

Caching:

The explicit wk setup is more efficient and precise w.r.t caching.

How to cache?

Let's say we want to cache metafunctions that return comp exprs

Every *cached call* should yield a variable in local scope, which is (or will be)
let-defined as the resulting expr of the call.

This is not so easy!


























-}
