
module Value where

import Common
import Syntax

data Spine
  = SId
  | SApp Spine Val Icit
  | SSplice Spine
  | SNatElim Val Val Val Spine
  | SProj Spine Name
  | SSuc Spine
  deriving Show

data RevSpine
  = RSId
  | RSApp Val Icit RevSpine
  | RSSplice RevSpine
  | RSNatElim Val Val Val RevSpine
  | RSProj Name RevSpine
  | RSSuc RevSpine
  deriving Show

revSpine :: Spine -> RevSpine
revSpine = go RSId where
  go acc = \case
    SId              -> acc
    SApp t u i       -> go (RSApp u i acc) t
    SProj t x        -> go (RSProj x acc) t
    SSplice t        -> go (RSSplice acc) t
    SNatElim p s z n -> go (RSNatElim p s z acc) n
    SSuc n           -> go (RSSuc acc) n

type Env     = [Val]
data Closure = Closure Env Tm deriving Show
type VTy     = Val

data RecClosure = RClosure Env [(Name, Tm)] deriving Show

data Val
  = VFlex MetaVar Spine
  | VRigid Lvl Spine
  | VLam Name Icit (NoShow (Val -> Val))
  | VPi Name Icit ~VTy (NoShow (Val -> Val))
  | VU

  | VBox Val
  | VQuote Val

  | VEff Val
  | VReturn Val Val
  | VBind Name Val {-# unpack #-} Closure
  | VSeq Val Val

  | VRef Val
  | VNew Val Val
  | VWrite Val Val Val
  | VRead Val Val

  | VNat
  | VNatLit Integer

  | VRecTy {-# unpack #-} RecClosure
  | VRec [(Name, Val)]

  | VReadNat
  | VPrintNat Val
  | VLog String
  deriving Show

-- | Count number of applications in spine
spineApps :: Spine -> Int
spineApps = go 0 where
  go acc SId                 = acc
  go acc (SApp sp _ _)       = go (acc + 1) sp
  go acc (SSplice sp)        = go acc sp
  go acc (SProj sp x)        = go acc sp
  go acc (SNatElim _ _ _ sp) = go acc sp
  go acc (SSuc sp)           = go acc sp

pattern VVar :: Lvl -> Val
pattern VVar x = VRigid x SId

pattern VMeta :: MetaVar -> Val
pattern VMeta m = VFlex m SId

pattern VPiI x a b = VPi x Impl a (NoShow b)
pattern VPiE x a b = VPi x Expl a (NoShow b)
pattern VLamI x t  = VLam x Impl (NoShow t)
pattern VLamE x t  = VLam x Expl (NoShow t)
pattern VZero = VNatLit 0

(==>) :: Val -> Val -> Val
(==>) a b = VPiE "_" a (\_ -> b)
infixr 4 ==>
