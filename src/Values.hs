
module Values where

import Common
import qualified Syntax as S

data Close a = Close Env a
data Env = Nil | Snoc Env ~Val

data Spine
  = SNil
  | SApp Spine Val Icit

data UnfoldHead
  = UHMeta MetaVar
  | UHTop Lvl

type Ty = Val

data RigidHead
  = RHVar Lvl
  | RHDataCon Lvl Int
  | RHTyCon Lvl

data Val
  -- meta
  = Rigid RigidHead Spine
  | Flex MetaVar Spine
  | Unfold UnfoldHead Spine ~Val
  | Pi Name Icit Ty {-# unpack #-} (Close S.Tm)
  | Fun Val Val
  | Up Val
  | Lift Val
  | Rec [(Name, Val)]
  | Ty U

  -- mixed
  | Lam Name Icit Ty {-# unpack #-} (Close S.Tm)

  -- object
  | Top Lvl
  | App Val Val Icit
  | Let Name Ty Val {-# unpack #-} (Close S.Tm)
  | Down Val
  | DataCon Lvl Int
  | RecCon [(Name, Val)]
  | Case Val {-# unpack #-} (Close [(Lvl, [Name], S.Tm)])
  | Field Val Name Int


-- data Val
--   = Var0 Lvl
--   | Top0 Lvl
--   | Let0 Name Ty Val (Close S.Tm0)
--   | Lam0 Name Ty (Close S.Tm0)
--   | App Val Val
--   | DataCon0 Lvl Int
--   | RecCon [(Name, Val)]
--   | Field Val Name Int
--   | Case Val (Close [(Lvl, [Name], S.Tm0)])
--   | Down Val
