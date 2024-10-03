
module Value where

import Common
import Syntax

data Spine = SId | SApp Spine Val Icit | SSplice Spine
  deriving Show

type Env     = [Val]
data Closure = Closure Env Tm deriving Show
type VTy     = Val

data Val
  = VFlex MetaVar Spine
  | VRigid Lvl Spine
  | VLam Name Icit {-# unpack #-} Closure
  | VPi Name Icit ~VTy {-# unpack #-} Closure
  | VU

  | VBox Val
  | VQuote Val

  | VEff Val
  | VReturn Val
  | VBind Name Val {-# unpack #-} Closure
  | VSeq Val Val

  | VUnit
  | VTt

  | VRef Val
  | VNew Val
  | VWrite Val Val
  | VRead Val
  deriving Show

-- | Count number of applications in spine
spineApps :: Spine -> Int
spineApps = go 0 where
  go acc SId = acc
  go acc (SApp sp _ _) = go (acc + 1) sp
  go acc (SSplice sp)  = go acc sp

pattern VVar :: Lvl -> Val
pattern VVar x = VRigid x SId

pattern VMeta :: MetaVar -> Val
pattern VMeta m = VFlex m SId
