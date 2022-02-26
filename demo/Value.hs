
module Value where

import Common

type Env = [Val]
type VTy = Val

data Spine
  = SId
  | SApp Spine ~Val Icit Verbosity
  | SSplice Spine
  | SNatElim Stage Val Val Val Spine

data Val
  = VFlex MetaVar Spine
  | VRigid Lvl Spine
  | VLam Name Icit ~VTy (Val -> Val) Verbosity
  | VPi Name Icit ~VTy (Val -> Val)
  | VU Stage
  | VLift Val
  | VQuote Val
  | VNat Stage
  | VZero Stage
  | VSuc Stage Val

pattern VVar :: Lvl -> Val
pattern VVar x = VRigid x SId

pattern VMeta :: MetaVar -> Val
pattern VMeta m = VFlex m SId

infixr 3 ==>
(==>) :: VTy -> VTy -> VTy
a ==> b = VPi "_" Expl a (\_ -> b)

vlamE0 :: Name -> VTy -> (Val -> Val) -> Val
vlamE0 x a t = VLam x Expl a t V0

vpiE :: Name -> VTy -> (Val -> Val) -> Val
vpiE x = VPi x Expl
