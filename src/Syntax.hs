
module Syntax where

import Common

--------------------------------------------------------------------------------

type Ty = Tm1

data Tm0
  = Var0 Ix
  | Top0 Lvl
  | Let0 Name Ty Tm0 Tm0
  | DataCon0 Lvl Int
  | Lam0 Name Ty Tm0
  | App0 Tm0 Tm0
  | RecCon (Fields Tm0)
  | Field Tm0 Name Int
  | Case Tm0 (Cases Tm0)
  | Down Tm1
  deriving Show

data Tm1
  = Var1 Ix
  | Top1 Lvl
  | Let1 Name Ty Tm1 Tm1
  | DataCon1 Lvl Int
  | Lam1 Name Icit Ty Tm1
  | App1 Tm1 Tm1 Icit
  | Pi Name Icit Ty Ty
  | Fun Ty Ty
  | Rec (Fields Ty)
  | Meta MetaVar
  | Inserted MetaVar Locals
  | TyCon Lvl
  | Lift Ty
  | Ty U
  | Up Tm0
  deriving Show

data Locals
  = Empty
  | Define Locals Name Tm1 Ty
  | Bind Locals Name Ty
  deriving Show
