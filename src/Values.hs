
module Values where

import Common
import qualified Syntax as S

data Close a = Close Env a
data Env = Nil | Snoc1 Env ~Val1 | Snoc0 Env Lvl

data Spine
  = SNil
  | SApp Spine Val1 Icit

data UnfoldHead
  = UHMeta MetaVar
  | UHTop Lvl

type Ty = Val1

data RigidHead
  = RHVar Lvl
  | RHDataCon Lvl Int
  | RHTyCon Lvl

data Val1
  = Rigid RigidHead Spine
  | Flex MetaVar Spine
  | Unfold UnfoldHead Spine ~Val1
  | Pi Name Icit Ty {-# unpack #-} (Close S.Tm1)
  | Lam1 Name Icit Ty {-# unpack #-} (Close S.Tm1)
  | Fun Ty Ty
  | Up Val0
  | Lift Ty
  | Rec (Fields Ty)
  | Ty U

data Val0
  = Var0 Lvl
  | Top0 Lvl
  | App0 Val0 Val0
  | Let0 Name Ty Val0 {-# unpack #-} (Close S.Tm0)
  | Lam0 Name Ty {-# unpack #-} (Close S.Tm0)
  | Down Val1
  | DataCon0 Lvl Int
  | RecCon (Fields Val0)
  | Case Val0 {-# unpack #-} (Close (Cases S.Tm0))
  | Field Val0 Name Int

pattern Var1 :: Lvl -> Val1
pattern Var1 x = Rigid (RHVar x) SNil
