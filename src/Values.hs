
module Values where

import Common
import qualified Syntax as S

data Close a = Close Env a
data Env = Nil | Snoc1 Env ~Val1 | Snoc0 Env Lvl

wk1Env :: Env -> Env
wk1Env = \case Snoc1 env _ -> env; _ -> impossible
{-# inline wk1Env #-}

type Ty = Val1

data Spine
  = SId
  | SApp1 Spine Val1 Icit
  | SField1 Spine Name Int

data UnfoldHead
  = Top1 Lvl
  | Solved MetaVar

data Val0
  = Var0 Lvl
  | Top0 Lvl
  | Let0 Name Ty Val0 {-# unpack #-} (Close S.Tm0)
  | App0 Val0 Val0
  | Case Val0 {-# unpack #-} (Close (Cases S.Tm0))
  | Fix Name Name {-# unpack #-} (Close S.Tm0)
  | Down Val1
  | Field0 Val0 Name Int
  | RecCon0 (Fields Val0)
  | Lam0 Name Ty {-# unpack #-} (Close S.Tm0)
  | Add Val0 Val0
  | Sub Val0 Val0
  | Mul Val0 Val0
  | IntLit Int

data Val1
  = Unfold UnfoldHead Spine ~Val1
  | Flex MetaVar Spine
  | Pi Name Icit Ty {-# unpack #-} (Close S.Ty)
  | Lam1 Name Icit Ty {-# unpack #-} (Close S.Tm1)
  | App1 Val1 Val1 Icit
  | Fun Ty Ty
  | Var1 Lvl
  | Lift CV Ty
  | Up Val0
  | Rec0 (Fields Ty)
  | Rec1 (Close (Fields S.Ty))
  | RecCon1 (Fields Val1)
  | Field1 Val1 Name Int
  | U U
  | TyCon Lvl
  | DataCon Lvl Int
  | Int
