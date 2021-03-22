
module Syntax where

import Common

--------------------------------------------------------------------------------

type Ty = Tm

data Tm
  -- object or meta
  = Var Ix
  | Top Lvl
  | Let Name Ty Tm Tm
  | DataCon Lvl Int
  | Lam Name Icit Ty Tm
  | App Tm Tm Icit

  -- meta
  | Pi Name Icit Ty Tm
  | Fun Ty Ty
  | Ty U
  | Lift Ty
  | Up Tm
  | Rec [(Name, Ty)]
  | TyCon Lvl
  | Inserted MetaVar Locals    -- masked inserted meta
  | Meta MetaVar               -- No metavars in object syntax!!

  -- object
  | RecCon [(Name, Tm)]
  | Field Tm Name Int
  | Case Tm [(Lvl, [Name], Tm)]
  | Down Tm
  deriving Show

data Locals
  = Empty
  | Define Locals Name Tm Ty
  | Bind Locals Name Ty
  deriving Show
