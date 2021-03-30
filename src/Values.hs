
module Values where

import Common
import qualified Syntax as S

data Close a = Close Env a
data Env = Nil | Snoc1 Env ~Val1 | Snoc0 Env Lvl

bind :: Env -> Lvl -> U s -> Env
bind env l U0{} = Snoc0 env l
bind env l U1   = Snoc1 env (Var l)
{-# inline bind #-}

type Val1 = Val S1
type Val0 = Val S0
type Ty   = Val1

data Spine
  = SId
  | SApp1 Spine Val1 Icit
  | SField1 Spine Name Int

data UnfoldHead
  = Top1 Lvl
  | Solved MetaVar

data Val :: Stage -> Type where

  Unfold   :: UnfoldHead -> Spine -> ~Val1 -> Val1
  Flex     :: MetaVar    -> Spine -> Val1

  Var      :: Lvl -> Val s
  Top0     :: Lvl -> Val0
  Let      :: Name -> Ty -> Val0 -> {-# unpack #-} (Close S.Tm0) -> Val0

  Lift     :: CV -> Ty -> Ty
  Up       :: Val0 -> Val1
  Down     :: Val1 -> Val0

  TyCon    :: Lvl -> Ty
  DataCon  :: Lvl -> Int -> Val s
  Case     :: Val0 -> {-# unpack #-} (Close (Cases S.Tm0)) -> Val0
  Fix      :: Name -> Name -> {-# unpack #-} (Close S.Tm0) -> Val0

  Pi       :: Name -> Icit -> Ty -> {-# unpack #-} (Close S.Ty) -> Val1
  Lam1     :: Name -> Icit -> Ty -> {-# unpack #-} (Close S.Tm1) -> Val1
  App1     :: Val1 -> Val1 -> Icit -> Val1

  Fun      :: Ty -> Ty -> Ty
  Lam0     :: Name -> Ty -> {-# unpack #-} (Close S.Tm0) -> Val0
  App0     :: Val0 -> Val0 -> Val0

  Rec      :: Fields Ty -> Ty
  RecCon   :: Fields (Val s) -> Val s
  Field    :: Val s -> Name -> Int -> Val s

  U        :: U s -> Ty
