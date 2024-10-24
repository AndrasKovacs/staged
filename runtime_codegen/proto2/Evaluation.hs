
module Evaluation (($$), quote, eval, nf, force, lvl2Ix, vApp, vAppSp, vAppPruning, vSplice) where

import Common
import ElabState
import Syntax
import Value
import Cxt.Type

infixl 8 $$
($$) :: Dbg => Closure -> Val -> Val
($$) (Closure env t) ~u = eval (env :> u) t

vApp :: Val -> Val -> Icit -> Val
vApp t ~u i = case t of
  VLam _ _ t  -> t $$ u
  VFlex  m sp -> VFlex m  (SApp sp u i)
  VRigid x sp -> VRigid x (SApp sp u i)
  t           -> impossible

vAppSp :: Val -> Spine -> Val
vAppSp t = \case
  SId          -> t
  SApp sp u i  -> vApp (vAppSp t sp) u i
  SSplice sp   -> vSplice (vAppSp t sp)

vMeta :: MetaVar -> Val
vMeta m = case lookupMeta m of
  Solved v _  -> v
  Unsolved{}  -> VMeta m

vCheck :: Env -> CheckVar -> Val
vCheck env c = case lookupCheck c of

  -- We know from the saved "cxt" which variables the placeholder "m" abstracts
  -- over.
  Unchecked cxt t a m -> vAppPruning env (vMeta m) (pruning cxt)
  Checked t           -> eval env t

vAppPruning :: Dbg => Env -> Val -> Pruning -> Val
vAppPruning env ~v pr = case (env, pr) of
  ([]       , []           ) -> v
  (env :> t , pr :> Just i ) -> vApp (vAppPruning env v pr) t i
  (env :> t , pr :> Nothing) -> vAppPruning env v pr
  _                          -> impossible

vVar :: Env -> Ix -> Val
vVar env x | unIx x < length env = env !! unIx x
vVar env x = error $ "index out of env: "
                  ++ show ("env len"::String, length env, "ix"::String, x)

vQuote :: Val -> Val
vQuote = \case
  VRigid x (SSplice sp) -> VRigid x sp
  VFlex  x (SSplice sp) -> VFlex x sp
  v                     -> VQuote v

vSplice :: Val -> Val
vSplice = \case
  VQuote v    -> v
  VRigid x sp -> VRigid x (SSplice sp)
  VFlex x sp  -> VFlex x (SSplice sp)
  _           -> impossible

vBind :: Name -> Val -> Closure -> Val
vBind = VBind               -- TODO: definitional monad laws with functional closures

eval :: Dbg => Env -> Tm -> Val
eval env = \case
  Var x            -> vVar env x
  App t u i        -> vApp (eval env t) (eval env u) i
  Lam x i t        -> VLam x i (Closure env t)
  Pi x i a b       -> VPi x i (eval env a) (Closure env b)
  Let _ _ t u      -> eval (env :> eval env t) u
  U                -> VU
  Meta m           -> vMeta m
  AppPruning t pr  -> vAppPruning env (eval env t) pr
  PostponedCheck c -> vCheck env c
  Box t            -> VBox (eval env t)
  Quote t          -> vQuote (eval env t)
  Splice t _       -> vSplice (eval env t)
  Unit             -> VUnit
  Tt               -> VTt
  Eff t            -> VEff (eval env t)
  Return t         -> VReturn (eval env t)
  Bind x t u       -> vBind x (eval env t) (Closure env u)
  Seq t u          -> VSeq (eval env t) (eval env u)
  Ref t            -> VRef (eval env t)
  New t            -> VNew (eval env t)
  Read t           -> VRead (eval env t)
  Write t u        -> VWrite (eval env t) (eval env u)
  Erased _         -> impossible

force :: Val -> Val
force = \case
  VFlex m sp | Solved t _ <- lookupMeta m -> force (vAppSp t sp)
  t -> t

quoteSp :: Lvl -> Tm -> Spine -> Tm
quoteSp l t = \case
  SId          -> t
  SApp sp u i  -> App (quoteSp l t sp) (quote l u) i
  SSplice sp   -> Splice (quoteSp l t sp) Nothing

quote :: Dbg => Lvl -> Val -> Tm
quote l t = case force t of
  VFlex m sp  -> quoteSp l (Meta m) sp
  VRigid x sp -> quoteSp l (Var (lvl2Ix l x)) sp
  VLam x i t  -> Lam x i (quote (l + 1) (t $$ VVar l))
  VPi x i a b -> Pi x i (quote l a) (quote (l + 1) (b $$ VVar l))
  VU          -> U
  VBox t      -> Box (quote l t)
  VQuote t    -> Quote (quote l t)
  VEff t      -> Eff (quote l t)
  VReturn t   -> Return (quote l t)
  VBind x t u -> Bind x (quote l t) (quote (l + 1) (u $$ VVar l))
  VSeq t u    -> Seq (quote l t) (quote l u)
  VUnit       -> Unit
  VTt         -> Tt
  VRef t      -> Ref (quote l t)
  VNew t      -> New (quote l t)
  VRead t     -> Read (quote l t)
  VWrite t u  -> Write (quote l t) (quote l u)

nf :: Env -> Tm -> Tm
nf env t = quote (Lvl (length env)) (eval env t)
