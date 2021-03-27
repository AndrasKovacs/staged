
module Syntax where

import Common

type Ty = Tm

data Tm
  -- structural
  = Var Ix
  | Top Lvl
  | Let Name Ty Tm Tm

  -- Pi
  | Pi Name Icit Ty Ty
  | Lam Name Icit Ty Tm
  | App Tm Tm Icit

  -- U
  | U U

  -- Records
  | Rec (Fields Ty)
  | RecCon (Fields Tm)
  | Field Tm Name Int

  -- ADTs
  | TyCon Lvl
  | DataCon Lvl Int
  | Case Tm (Cases Tm)
  | Fix Name Name       -- TODO

  -- lifts
  | Lift U Ty
  | Up U Tm
  | Down Tm     -- (non-weak, goes from 1 to 0)

  -- meta
  | Inserted MetaVar Locals
  | Meta MetaVar
  deriving Show

data Locals
  = Empty
  | Define Locals Name Ty U Tm
  | Bind Locals Name Ty U
  deriving Show
