
module Evaluation (
  ($$), quote, eval, nf, force, lvl2Ix, vApp, vAppSp, vAppPruning, vSplice, vAppI, vAppE, vSuc, vProj)
  where

import Common
import ElabState
import Syntax
import Value
import Cxt.Type

class Apply a where
  ($$) :: a -> Val -> Val
  infixl 8 $$

instance Apply Closure where
  ($$) (Closure env t) ~u = eval (env :> u) t

instance Apply (NoShow (Val -> Val)) where
  ($$) f ~u = coerce f u

vApp :: Val -> Val -> Icit -> Val
vApp t ~u i = case t of
  VLam _ _ t  -> t $$ u
  VFlex  m sp -> VFlex m  (SApp sp u i)
  VRigid x sp -> VRigid x (SApp sp u i)
  t           -> impossible

vSuc :: Val -> Val
vSuc = \case
  VNatLit n   -> VNatLit (n + 1)
  VFlex  m sp -> VFlex m  (SSuc sp)
  VRigid x sp -> VRigid x (SSuc sp)
  _           -> impossible

vAppI :: Val -> Val -> Val
vAppI t ~u = vApp t u Impl

vAppE :: Val -> Val -> Val
vAppE t ~u = vApp t u Expl

vNatElimLit :: Val -> Val -> Integer -> Val
vNatElimLit s z n = case n of
  0 -> z
  n -> let n' = n - 1 in
       s `vAppI` VNatLit n' `vAppE` vNatElimLit s z n'

vNatElim :: Val -> Val -> Val -> Val -> Val
vNatElim p s z = \case
  VNatLit n          -> vNatElimLit s z n
  VFlex m (SSuc sp)  -> let v = VFlex  m sp in s `vAppI` v `vAppE` vNatElim p s z v
  VRigid x (SSuc sp) -> let v = VRigid x sp in s `vAppI` v `vAppE` vNatElim p s z v
  VFlex m sp         -> VFlex  m (SNatElim p s z sp)
  VRigid x sp        -> VRigid x (SNatElim p s z sp)
  _                  -> impossible

vProj :: Val -> Name -> Val
vProj v x = case v of
  VRec fs     -> fromJust $ lookup x fs
  VFlex m sp  -> VFlex  m (SProj sp x)
  VRigid i sp -> VRigid i (SProj sp x)
  _           -> impossible

vAppSp :: Val -> Spine -> Val
vAppSp t = \case
  SId              -> t
  SApp sp u i      -> vApp (vAppSp t sp) u i
  SSplice sp       -> vSplice (vAppSp t sp)
  SNatElim p s z n -> vNatElim p s z (vAppSp t n)
  SProj sp x       -> vProj (vAppSp t sp) x
  SSuc sp          -> vSuc (vAppSp t sp)

vMeta :: MetaVar -> Val
vMeta m = case lookupMeta m of
  Solved v _  -> v
  Unsolved{}  -> VMeta m

vCheck :: Env -> CheckVar -> Val
vCheck env c = case lookupCheck c of

  -- We know from the saved "cxt" which variables the placeholder "m" abstracts
  -- over.
  Right (Unchecked cxt t a m) -> vAppPruning env (vMeta m) (pruning cxt)
  Left t                      -> eval env t

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

evalFields :: Env -> [(Name, Tm)] -> [(Name, Val)]
evalFields env = go where
  go []          = []
  go ((x, t):ts) = (:) $$! ((x,) $$! eval env t) $$! go ts

vOpen :: Env -> [Name] -> Val -> Tm -> Val
vOpen env xs t u = eval (foldl' (\env x -> vProj t x:env) env xs) u

eval :: Dbg => Env -> Tm -> Val
eval env = \case
  Var x            -> vVar env x
  App t u i        -> vApp (eval env t) (eval env u) i
  Lam x i t        -> VLam x i (NoShow \u -> eval (u:env) t)
  Pi x i a b       -> VPi x i (eval env a) (NoShow \u -> eval (u:env) b)
  Let _ _ t u      -> eval (env :> eval env t) u
  U                -> VU
  Meta m           -> vMeta m
  AppPruning t pr  -> vAppPruning env (eval env t) pr
  PostponedCheck c -> vCheck env c
  Box              -> VLamE "A" VBox
  Quote t          -> vQuote (eval env t)
  Splice t _       -> vSplice (eval env t)
  Eff              -> VLamE "A" VEff
  Return           -> VLamI "A" \a -> VLamE "a" (VReturn a)
  Bind x t u       -> vBind x (eval env t) (Closure env u)
  Seq t u          -> VSeq (eval env t) (eval env u)
  Ref              -> VLamE "A" VRef
  New              -> VLamI "A" \a -> VLamE "a" (VNew a)
  Read             -> VLamI "A" \a -> VLamE "a" (VRead a)
  Write            -> VLamI "A" \a -> VLamE "t" \t -> VLamE "u" \u -> VWrite a t u
  Erased _         -> impossible
  Nat              -> VNat
  NatLit n         -> VNatLit n
  Suc              -> VLamE "n" \n -> vSuc n
  NatElim          -> VLamE "P" \p -> VLamE "s" \s -> VLamE "z" \z -> VLamE "n" \n ->
                      vNatElim p s z n
  RecTy t          -> VRecTy (RClosure env t)
  Rec t            -> VRec (evalFields env t)
  Proj t x         -> vProj (eval env t) x
  Open xs t u      -> vOpen env xs (eval env t) u

force :: Val -> Val
force = \case
  VFlex m sp | Solved t _ <- lookupMeta m -> force (vAppSp t sp)
  t -> t

quoteSp :: Lvl -> Tm -> Spine -> Tm
quoteSp l t = \case
  SId              -> t
  SApp sp u i      -> App (quoteSp l t sp) (quote l u) i
  SSplice sp       -> Splice (quoteSp l t sp) Nothing
  SNatElim p s z n -> NatElim' (quote l p) (quote l s) (quote l z) (quoteSp l t n)
  SProj sp x       -> Proj (quoteSp l t sp) x
  SSuc sp          -> Suc' (quoteSp l t sp)

quoteRecClosure :: Lvl -> RecClosure -> [(Name, Tm)]
quoteRecClosure l (RClosure env ts) = case ts of
  []        -> []
  (x, a):ts -> let a'  = quote l (eval env a)
                   ts' = quoteRecClosure (l + 1) (RClosure (VVar l:env) ts)
               in (x, a'):ts'

quoteRec :: Lvl -> [(Name, Val)] -> [(Name, Tm)]
quoteRec l = \case
  []        -> []
  (x, t):ts -> (:) $$! ((x,) $$! quote l t) $$! quoteRec l ts

quote :: Dbg => Lvl -> Val -> Tm
quote l t = case force t of
  VFlex m sp   -> quoteSp l (Meta m) sp
  VRigid x sp  -> quoteSp l (Var (lvl2Ix l x)) sp
  VLam x i t   -> Lam x i (quote (l + 1) (coerce t $ VVar l))
  VPi x i a b  -> Pi x i (quote l a) (quote (l + 1) (coerce b $ VVar l))
  VU           -> U
  VBox t       -> Box' (quote l t)
  VQuote t     -> Quote (quote l t)
  VEff t       -> Eff' (quote l t)
  VReturn a t  -> Return' (quote l a) (quote l t)
  VBind x t u  -> Bind x (quote l t) (quote (l + 1) (u $$ VVar l))
  VSeq t u     -> Seq (quote l t) (quote l u)
  VRef t       -> Ref' (quote l t)
  VNew a t     -> New' (quote l a) (quote l t)
  VRead a t    -> Read' (quote l a) (quote l t)
  VWrite a t u -> Write' (quote l a) (quote l t) (quote l u)
  VNat         -> Nat
  VNatLit n    -> NatLit n
  VRecTy ts    -> RecTy (quoteRecClosure l ts)
  VRec ts      -> Rec (quoteRec l ts)

nf :: Env -> Tm -> Tm
nf env t = quote (Lvl (length env)) (eval env t)
