
module Interpreter where

import Common
import Syntax

import Data.IORef

data Env = Nil | Def Env Val

envLen :: Env -> Lvl
envLen = go 0 where
  go acc Nil = acc
  go acc (Def e _) = go (acc + 1) e

type Closed = Val -> Val
type Open   = Lvl -> Val -> Val

data Spine = SId | SApp Spine Val Icit | SSplice Spine
type Stage = Int

data Val
  -- actual runtime objects
  = VNe Lvl Spine               -- only in open eval
  | VLam Name Icit Closed Open
  | VEffect (IO Val)
  | VQuote Val
  | VRefVal (IORef Val)
  | VTt

  -- runtime types and code
  | VEff Val
  | VTopVar Lvl
  | VUnit
  | VBox Val
  | VU
  | VRef Val
  | VPi Name Icit Val Open      -- we only need open code because the only
  | VLet Name Val Val Open  --   purpose of binders here is to eventually pass
  | VReturn Val             --   a bound var and generate syntactic code
  | VBind Name Val Open
  | VConstBind Val Val
  | VNew Val
  | VWrite Val Val
  | VRead Val
  | VApp Val Val Icit


lookupIx :: Env -> Ix -> Val
lookupIx e x = case (e, x) of
  (Def _ v, 0) -> v
  (Def e _, x) -> lookupIx e (x - 1)
  _            -> impossible

cRun :: Val -> IO Val
cRun = \case
  VEffect eff -> eff
  _           -> impossible

cApp :: Val -> Val -> Val
cApp t u = case t of
  VLam _ _ t _ -> t u
  _            -> impossible

cRead :: Val -> IO Val
cRead = \case
  VRefVal r -> readIORef r
  _         -> impossible

cWrite :: Val -> Val -> IO Val
cWrite t u = case t of
  VRefVal r -> VTt <$ writeIORef r u
  _         -> impossible

cSplice :: Val -> Val
cSplice = \case
  VQuote t -> undefined
  _        -> impossible

-- pure closed evaluation
ceval :: Env -> Tm -> Val
ceval e = \case
  Var x         -> lookupIx e x
  TopVar x      -> lookupIx e (lvl2Ix (envLen e) x)
  App t u _     -> cApp (ceval e t) (ceval e u)
  Lam x i t     -> VLam x i (\u -> ceval (Def e u) t) (\l u -> oeval (Def e u) l 0 t)
  U             -> VU
  Pi x i a b    -> VPi x i (ceval e a) \l u -> oeval (Def e u) l 0 b
  Let x a t u   -> ceval (Def e (ceval e t)) u
  Box t         -> VBox (ceval e t)
  Quote t       -> VQuote (oeval e 0 0 t)
  Splice t      -> undefined
  Unit          -> VUnit
  Tt            -> VTt
  Eff t         -> VEff (ceval e t)
  Ref t         -> VRef (ceval e t)
  t@New{}       -> VEffect (cexec e t)
  t@Return{}    -> VEffect (cexec e t)
  t@ConstBind{} -> VEffect (cexec e t)
  t@Bind{}      -> VEffect (cexec e t)
  t@Write{}     -> VEffect (cexec e t)
  t@Read{}      -> VEffect (cexec e t)

  PostponedCheck{} -> impossible
  Meta{}           -> impossible
  AppPruning{}     -> impossible

-- Eff closed evaluation
cexec :: Env -> Tm -> IO Val
cexec e = \case
  Var x       -> cRun (lookupIx e x)
  TopVar x    -> cRun (lookupIx e (lvl2Ix (envLen e) x))
  Return t    -> pure $! ceval e t
  Bind x t u  -> do {t <- cexec e t; cexec (Def e t) u}
  Let x a t u -> cexec (Def e (ceval e t)) u
  App t u _   -> cRun (cApp (ceval e t) (ceval e u))
  New t       -> VRefVal <$!> (newIORef $! ceval e t)
  Read t      -> cRead (ceval e t)
  Write t u   -> cWrite (ceval e t) (ceval e u)
  Splice t    -> cRun (cSplice (ceval e t))
  _           -> impossible

oApp :: Lvl -> Val -> Val -> Icit -> Val
oApp l t u i = case t of
  VLam _ _ _ t -> t l u
  VNe x sp     -> VNe x (SApp sp u i)
  _            -> impossible

oQuote :: Val -> Val
oQuote = \case
  VNe x (SSplice sp) -> VNe x sp
  t                  -> VQuote t

oSplice :: Val -> Val
oSplice = \case
  VQuote t -> t
  VNe x sp -> VNe x (SSplice sp)
  _        -> impossible

oeval :: Env -> Lvl -> Stage -> Tm -> Val
oeval e l 0 = \case
  Var x     -> lookupIx e x
  TopVar x  -> lookupIx e (lvl2Ix (envLen e) x)

  -- open lambdas can never become closed!
  Lam x i t     -> VLam x i (\u -> impossible) (\l u -> oeval (Def e u) l 0 t)
  App t u i     -> oApp l (oeval e l 0 t) (oeval e l 0 u) i
  U             -> VU
  Pi x i a b    -> VPi x i (oeval e l 0 a) \l u -> oeval (Def e u) l 0 b
  Let x a t u   -> oeval (Def e (oeval e l 0 t)) l 0 u
  Box t         -> VBox (oeval e l 0 t)
  Quote t       -> oQuote (oeval e l 1 t)

  Splice t      -> undefined

  Unit          -> VUnit
  Tt            -> VTt
  Eff t         -> VEff (oeval e l 0 t)
  Return t      -> VReturn (oeval e l 0 t)
  Bind x t u    -> VBind x (oeval e l 0 t) \l t -> oeval (Def e t) l 0 u
  ConstBind t u -> VConstBind (oeval e l 0 t) (oeval e l 0 u)
  Ref t         -> VRef (oeval e l 0 t)
  New t         -> VNew (oeval e l 0 t)
  Write t u     -> VWrite (oeval e l 0 t) (oeval e l 0 u)
  Read t        -> VRead (oeval e l 0 t)

  Meta{}           -> impossible
  AppPruning{}     -> impossible
  PostponedCheck{} -> impossible

oeval e l s = \case
  Var x         -> lookupIx e x
  TopVar x      -> VTopVar x
  Lam x i t     -> VLam x i (\u -> impossible) (\l u -> oeval (Def e u) l s t)
  App t u i     -> VApp (oeval e l s t) (oeval e l s u) i
  U             -> VU
  Pi x i a b    -> VPi x i (oeval e l s a) \l u -> oeval (Def e u) l s b
  Let x a t u   -> VLet x (oeval e l s a) (oeval e l s t) \l t -> oeval (Def e t) l s u
  Box t         -> VBox (oeval e l s t)
  Quote t       -> VQuote (oeval e l 1 t)
  Splice t      -> oSplice (oeval e l s t)
  Unit          -> VUnit
  Tt            -> VTt
  Eff t         -> VEff (oeval e l s t)
  Return t      -> VReturn (oeval e l s t)
  Bind x t u    -> VBind x (oeval e l s t) \l t -> oeval (Def e t) l s u
  ConstBind t u -> VConstBind (oeval e l s t) (oeval e l s u)
  Ref t         -> VRef (oeval e l s t)
  New t         -> VNew (oeval e l s t)
  Write t u     -> VWrite (oeval e l s t) (oeval e l s u)
  Read t        -> VRead (oeval e l s t)

  Meta{}           -> impossible
  AppPruning{}     -> impossible
  PostponedCheck{} -> impossible
