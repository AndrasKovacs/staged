
module Syntax where

import Common

type Ty = Tm1

data Tm0
  = Var0 Ix
  | Top0 Lvl
  | Let0 Name Ty Tm0 Tm0
  | Lam0 Name Ty Tm0
  | App0 Tm0 Tm0
  | Case Tm0 (Cases Tm0)
  | DataCon0 Lvl Int
  | Fix Name Name Tm0
  | Down Tm1
  | Field0 Tm0 Name Int
  | RecCon0 (Fields Tm0)
  | Wk0 Tm0
  deriving Show

data Tm1
  = Var1 Ix
  | Top1 Lvl
  | Let1 Name Ty Tm1 Tm1
  | Pi Name Icit Ty Ty
  | Lam1 Name Icit Ty Tm1
  | App1 Tm1 Tm1 Icit
  | Fun Ty Ty
  | U U
  | Lift CV Ty
  | Up Tm0
  | Rec0 (Fields Ty)
  | Rec1 (Fields Ty)
  | RecCon1 (Fields Tm1)
  | Field1 Tm1 Name Int
  | TyCon Lvl
  | DataCon1 Lvl Int
  | Inserted MetaVar Locals
  | Wk1 Tm1
  | Meta MetaVar
  deriving Show

data Locals
  = Empty
  | Define Locals Name Ty Tm1
  | Bind0 Locals Name Ty CV
  | Bind1 Locals Name Ty
  deriving Show

up :: Tm0 -> Tm1
up (Down t) = t
up t        = Up t
{-# inline up #-}

down :: Tm1 -> Tm0
down (Up t) = t
down t      = Down t
{-# inline down #-}
