
module Cxt.Type where

import qualified Data.Map as M

import Common
import Syntax
import Value

data Cxt = Cxt {                        -- Used for:
    env        :: Env                   -- evaluation
  , lvl        :: Lvl                   -- going under binders
  , locals     :: Locals                -- getting types of fresh metas
  , pruning    :: Pruning               -- getting terms of fresh metas (mask of bound variables)
  , topNames   :: M.Map Name (Lvl, VTy) -- only contains info relevant to raw name lookup
  , localNames :: M.Map Name (Lvl, VTy) -- only contains info relevant to raw name lookup
  , pos        :: SourcePos
  }

names :: Cxt -> [Name]
names = go . locals where
  go LHere              = []
  go (LDefine ls x _ _) = go ls :> x
  go (LBind ls x _)     = go ls :> x

instance Show Cxt where
  show = show . names
