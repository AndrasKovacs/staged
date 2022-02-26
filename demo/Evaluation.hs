
module Evaluation (($$), quote, eval, nf, force, lvl2Ix, vApp, vAppSp, zonk) where

import Common
import Metacontext
import Syntax
import Value

infixl 8 $$
($$) :: Closure -> Val -> Val
($$) (Closure env t) ~u = eval (env :> u) t

vApp :: Val -> Val -> Icit -> Verbosity -> Val
vApp t ~u i o = case t of
  VLam _ _ _ t o -> t $$ u
  VFlex  m sp    -> VFlex m  (SApp sp u i o)
  VRigid x sp    -> VRigid x (SApp sp u i o)
  _              -> impossible

vAppE0 :: Val -> Val -> Val
vAppE0 t u = vApp t u Expl V0

vAppSp :: Val -> Spine -> Val
vAppSp t = \case
  SId                  -> t
  SApp sp u i o        -> vApp (vAppSp t sp) u i o
  SSplice sp           -> vSplice (vAppSp t sp)
  SNatElim st p s z sp -> vNatElim st p s z (vAppSp t sp)

vQuote :: Val -> Val
vQuote = \case
  VFlex  m (SSplice sp) -> VFlex m sp
  VRigid x (SSplice sp) -> VRigid x sp
  t                     -> VQuote t

vSplice :: Val -> Val
vSplice = \case
  VQuote t    -> t
  VFlex m sp  -> VFlex m (SSplice sp)
  VRigid x sp -> VRigid x (SSplice sp)
  _           -> impossible

vMeta :: MetaVar -> Val
vMeta m = case lookupMeta m of
  Solved v _ _ -> v
  Unsolved{}   -> VMeta m

vAppPruning :: Env -> Val -> Pruning -> Val
vAppPruning env ~v pr = case (env, pr) of
  ([]       , []           ) -> v
  (env :> t , pr :> Just i ) -> vApp (vAppPruning env v pr) t i V0
  (env :> t , pr :> Nothing) -> vAppPruning env v pr
  _                          -> impossible

vVar :: Env -> Ix -> Val
vVar (env:>v) 0 = v
vVar (env:>_) x = vVar env (x - 1)
vVar _        _ = impossible

vNatElim :: Stage -> Val -> Val -> Val -> Val -> Val
vNatElim st p s z n = case n of
  VZero _     -> z
  VSuc _ n    -> s `vAppE0` n `vAppE0` vNatElim st p s z n
  VFlex m sp  -> VFlex m (SNatElim st p s z sp)
  VRigid x sp -> VRigid x (SNatElim st p s z sp)
  _           -> impossible

eval :: Env -> Tm -> Val
eval env = \case
  Var x              -> vVar env x
  App t u i o        -> vApp (eval env t) (eval env u) i o
  Lam x i a t o      -> VLam x i (eval env a) (Closure env t) o
  Pi x i a b         -> VPi x i (eval env a) (Closure env b)
  Let _ _ _ t u      -> eval (env :> eval env t) u
  U s                -> VU s
  Meta m             -> vMeta m
  AppPruning t pr    -> vAppPruning env (eval env t) pr
  InsertedMeta m pr  -> vAppPruning env (vMeta m) pr
  Quote t            -> vQuote (eval env t)
  Splice t           -> vSplice (eval env t)
  Lift t             -> VLift (eval env t)
  Wk t               -> eval (tail env) t
  Nat s              -> VNat s
  Zero s             -> VZero s
  Suc s t            -> VSuc s (eval env t)
  NatElim st p s z t -> vNatElim st (eval env p) (eval env s) (eval env z) (eval env t)

force :: Val -> Val
force = \case
  VFlex m sp | Solved t _ _ <- lookupMeta m -> force (vAppSp t sp)
  t -> t

lvl2Ix :: Lvl -> Lvl -> Ix
lvl2Ix (Lvl l) (Lvl x) = Ix (l - x - 1)

quoteSp :: Lvl -> Tm -> Spine -> Tm
quoteSp l t = \case
  SId                  -> t
  SApp sp u i o        -> App (quoteSp l t sp) (quote l u) i o
  SSplice sp           -> Splice (quoteSp l t sp)
  SNatElim st p s z sp -> NatElim st (quote l p) (quote l s) (quote l z) (quoteSp l t sp)

quote :: Lvl -> Val -> Tm
quote l t = case force t of
  VFlex m sp     -> quoteSp l (Meta m) sp
  VRigid x sp    -> quoteSp l (Var (lvl2Ix l x)) sp
  VLam x i a t o -> Lam x i (quote l a) (quote (l + 1) (t $$ VVar l)) o
  VPi x i a b    -> Pi x i (quote l a) (quote (l + 1) (b $$ VVar l))
  VU s           -> U s
  VLift t        -> Lift (quote l t)
  VQuote t       -> Quote (quote l t)
  VNat s         -> Nat s
  VZero s        -> Zero s
  VSuc s t       -> Suc s (quote l t)


nf :: Env -> Tm -> Tm
nf env t = quote (Lvl (length env)) (eval env t)


-- | Unfold all metas and evaluate meta-headed spines, but don't evaluate
--   anything else.
zonk :: Env -> Lvl -> Tm -> Tm
zonk vs l t = go t where

  goSp :: Tm -> Either Val Tm
  goSp = \case
    Meta m      -> case lookupMeta m of
                     Solved v _ _ -> Left v
                     _            -> Right (Meta m)
    App t u i o -> case goSp t of
                     Left t  -> Left $! vApp t (eval vs u) i o
                     Right t -> Right $ App t (go u) i o
    t           -> Right (go t)

  goBind t = zonk (vs :> VVar l) (l+1) t

  go = \case
    Var x              -> Var x
    Meta m             -> case lookupMeta m of
                            Solved v _ _  -> quote l v
                            Unsolved{}    -> Meta m
    U s                -> U s
    Pi x i a b         -> Pi x i (go a) (goBind b)
    App t u i o        -> case goSp t of
                            Left t  -> quote l (vApp t (eval vs u) i o)
                            Right t -> App t (go u) i o
    Lam x i a t o      -> Lam x i (go a) (goBind t) o
    Let s x a t u      -> Let s x (go a) (go t) (goBind u)
    Wk t               -> Wk (zonk (tail vs) (l-1) t)

    AppPruning t pr    -> AppPruning (go t) pr
    InsertedMeta m pr  -> case lookupMeta m of
                            Solved v _ _ -> quote l (vAppPruning vs v pr)
                            Unsolved{}   -> InsertedMeta m pr

    Lift a             -> Lift (go a)
    Quote t            -> Quote (go t)
    Splice t           -> Splice (go t)
    Nat s              -> Nat s
    Zero s             -> Zero s
    Suc s t            -> Suc s (go t)
    NatElim st p s z t -> NatElim st (go p) (go s) (go z) (go t)
