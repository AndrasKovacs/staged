
module Value where

import Common
import Syntax

type Env     = [Val]
data Closure = Closure Env Tm deriving Show
type VTy     = Val

data Spine
  = SId
  | SApp Spine ~Val Icit Verbosity
  | SSplice Spine
  deriving Show

data Val
  = VFlex MetaVar Spine
  | VRigid Lvl Spine
  | VLam Name Icit ~VTy {-# unpack #-} Closure Verbosity
  | VPi Name Icit ~VTy {-# unpack #-} Closure
  | VU Stage
  | VLift Val
  | VQuote Val
  deriving Show

pattern VVar :: Lvl -> Val
pattern VVar x = VRigid x SId

pattern VMeta :: MetaVar -> Val
pattern VMeta m = VFlex m SId
