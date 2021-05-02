
module UnifyCxt where

import Lens.Micro.TH

import Common
import Values
import Cxt.Fields

data Cxt = Cxt {cxtLvl :: Lvl, cxtNames :: [Name]}
  deriving Show
makeFields ''Cxt

bind :: Name -> Cxt -> Cxt
bind x (Cxt l xs) = Cxt (l + 1) (x:xs)
{-# inline bind #-}

topVar :: Cxt -> Val1
topVar cxt = Var1 (cxt^.lvl)
{-# inline topVar #-}
