{-# options_ghc -funbox-strict-fields #-}

module Values where

import Common
import qualified Syntax as S



data Close a = Close Env a

data Env = Nil | Snoc Env ~Val1

data Spine
  = SNil
  | SApp Spine Val1 Icit

data UnfoldHead
  = UHMeta MetaVar
  | UHTop Lvl

type Ty = Val1

data RigidHead
  = RHVar Lvl
  | RHDataCon1 Lvl Int
  | RHTyCon Lvl

data Val1
  = Rigid RigidHead Spine
  | Flex MetaVar Spine
  | Pi Name Icit Ty (Close S.Tm1)
  | Lam1 Name Icit Ty (Close S.Tm1)
  | Lift Val1
  | Up Val0
  | Rec [(Name, Ty)]

data Val0
  = Var0 Lvl
  | Top0 Lvl
  | Let0 Name Ty Val0 (Close S.Tm0)
  | Lam0 Name Ty (Close S.Tm0)
  | App Val0 Val0
  | DataCon0 Lvl Int
  | RecCon [(Name, Val0)]
  | Field Val0 Name Int
  | Case Val0 (Close [(Lvl, [Name], S.Tm0)])
  | Down Val1
