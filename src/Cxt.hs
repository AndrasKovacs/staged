
module Cxt where

import qualified Data.HashMap.Strict as M

import Common
import qualified Syntax as S
import qualified Values as V
import Evaluation

data NameInfo
  = forall s. NITop Lvl V.Ty (U s)
  | forall s. NILocal Lvl V.Ty (U s)

type NameTable = M.HashMap RawName NameInfo

data Cxt = Cxt {
  _env       :: V.Env,
  _lvl       :: Lvl,
  _locals    :: S.Locals,
  _nameTable :: NameTable,
  _src       :: RawName
  }

emptyCxt :: RawName -> Cxt
emptyCxt = Cxt V.Nil 0 S.Empty mempty
{-# inline emptyCxt #-}

addName :: Name -> NameInfo -> NameTable -> NameTable
addName NEmpty _ tbl = tbl
addName (NName x) inf tbl = M.insert x inf tbl
{-# inline addName #-}

bind :: Name -> S.Ty -> U s -> Cxt -> Cxt
bind x a au cxt = bind' x a (eval1 (_env cxt) a) au cxt
{-# inline bind #-}

bind' :: Name -> S.Ty -> V.Ty -> U s -> Cxt -> Cxt
bind' x a va au (Cxt env l loc ntbl src) =
  Cxt (V.bind env l au) (l + 1)
      (S.Bind loc x a au)
      (addName x (NILocal l va au) ntbl)
      src
{-# inline bind' #-}

newBinder1 :: Name -> S.Ty -> Cxt -> Cxt
newBinder1 x a (Cxt env l loc ntbl src) =
  Cxt (V.bind env l U1) (l + 1) (S.Bind loc x a U1) ntbl src
{-# inline newBinder1 #-}

define :: Name -> S.Ty -> S.Tm1 -> Cxt -> Cxt
define x a t cxt =
  define' x a (eval1 (_env cxt) a) t (eval1 (_env cxt) t) cxt
{-# inline define #-}

define' :: Name -> S.Ty -> V.Ty -> S.Tm1 -> V.Val1 -> Cxt -> Cxt
define' x a va t ~vt (Cxt env l loc ntbl src) =
  Cxt (V.Snoc1 env (V.Var l))
      (l + 1)
      (S.Define loc x a t)
      (addName x (NILocal l va U1) ntbl)
      src
{-# inline define' #-}
