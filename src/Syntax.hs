
module Syntax where

import Common

type Ty = Tm1

data Tm1
  = Var1 Ix
  | Top1 Lvl
  | Let1 Name Ty Tm1 Tm1
  | Pi Name Icit Ty Tm1
  | Lam1 Name Icit Ty Tm1
  | App1 Tm1 Tm1 Icit
  | Lift Ty
  | Up Tm0
  | Rec [(Name, Ty)]
  | TyCon
  | DataCon1 Lvl Int
  deriving Show

data Tm0
  = Var0 Ix
  | Top0 Lvl
  | Let0 Name Ty Tm0 Tm0
  | Lam0 Name Ty Tm0
  | App0 Tm0 Tm0
  | DataCon0 Lvl Int
  | RecCon [(Name, Tm0)]
  | Field Tm0 Name Int
  | Case Tm0 [(Lvl, [Name], Tm0)]
  | Down Tm1
  deriving Show


-- type Ty = Tm

-- data Tm
--   = Var Ix
--   | Top Lvl
--   | Let Name Ty U Tm Tm
--   | Pi Name Icit Ty Ty
--   | Lam Name Icit Ty Tm
--   | Fun Ty Ty
--   | App Tm Tm Icit
--   | Ty U
--   | Lift Tm
--   | Up Tm
--   | Down Tm
--   | Rec    [(Name, Tm)]
--   | RecCon [(Name, Tm)]
--   | Field Tm Name Int
--   | Fix Name Name Tm
--   | Case Tm [(Lvl, [Name], Tm)]
--   | DataCon Lvl Int
--   | TyCon Lvl
--   deriving Show
