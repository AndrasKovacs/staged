
module Syntax where

import Common

type Ty  = Tm S1
type Tm0 = Tm S0
type Tm1 = Tm S1

data Tm :: Stage -> Type where
  Var      :: Ix -> Tm s
  Top      :: Lvl -> Tm s
  Let      :: Name -> Ty -> Tm s -> Tm s -> Tm s

  Pi       :: Name -> Icit -> Ty -> Ty -> Ty
  Lam1     :: Name -> Icit -> Ty -> Tm1 -> Tm1
  App1     :: Tm1 -> Tm1 -> Icit -> Tm1

  Fun      :: Ty -> Ty -> Ty
  Lam0     :: Name -> Ty -> Tm0 -> Tm0
  App0     :: Tm0 -> Tm0 -> Tm0

  U        :: U s -> Ty

  Rec      :: Fields Ty -> Ty
  RecCon   :: Fields (Tm s) -> Tm s
  Field    :: Tm s -> Name -> Int -> Tm s

  TyCon    :: Lvl -> Ty
  DataCon  :: Lvl -> Int -> Tm s
  Case     :: Tm0 -> Cases Tm0 -> Tm0
  Fix      :: Name -> Name -> Tm0 -> Tm0

  Lift     :: Ty -> Ty
  Up       :: Tm0 -> Tm1
  Down     :: Tm1 -> Tm0

  Inserted :: MetaVar -> Locals -> Tm1
  Meta     :: MetaVar -> Tm1

deriving instance Show (Tm s)

data Locals where
  Empty  :: Locals
  Define :: Locals -> Name -> Ty -> Tm1 -> Locals
  Bind   :: Locals -> Name -> Ty -> U s -> Locals

deriving instance Show Locals
