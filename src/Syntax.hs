
module Syntax where

import Common

type Ty = Tm

data Tm
  = LocalVar Ix
  | TopDef Lvl
  | Pi Name Icit Ty Ty     -- meta-function
  | Fun Ty Ty              -- mono-function
  | Lam Name Icit Ty Tm
  | App Tm Tm Icit
  | Ty U
  | Lift Tm
  | Up Tm
  | Down Tm
  | Rec    [(Name, Tm)]
  | RecCon [(Name, Tm)]
  | Field Tm Name Int
  | Fix Name Name Tm
  | Case Tm [(Name, [Name], Tm)]
  | DataCon Name Int
  deriving Show
