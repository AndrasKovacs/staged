
module Cxt.Extension where

import Common
import Cxt.Types
import qualified Values as V
import qualified Syntax as S
import qualified Data.HashMap.Strict as M
import Evaluation

emptyCxt :: RawName -> Cxt
emptyCxt = Cxt V.Nil 0 S.Empty mempty
{-# inline emptyCxt #-}

addName :: Name -> NameInfo -> NameTable -> NameTable
addName NEmpty _ tbl = tbl
addName (NName x) inf tbl = M.insert x inf tbl
{-# inline addName #-}

bind0 :: Name -> S.Ty -> CV -> Cxt -> Cxt
bind0 x a acv cxt = bind0' x a (eval1 (_env cxt) a) acv cxt
{-# inline bind0 #-}

bind0' :: Name -> S.Ty -> V.Ty -> CV -> Cxt -> Cxt
bind0' x a va acv (Cxt env l loc ntbl src) =
  Cxt (V.Snoc0 env l)
      (l + 1)
      (S.Bind0 loc x a acv)
      (addName x (NILocal0 l va acv) ntbl)
      src
{-# inline bind0' #-}

newBinder :: Name -> S.Ty -> Cxt -> Cxt
newBinder x a (Cxt env l loc ntbl src) =
  Cxt (V.Snoc1 env (V.Var1 l)) (l + 1) (S.Bind1 loc x a) ntbl src
{-# inline newBinder #-}

define :: Name -> S.Ty -> S.Tm1 -> Cxt -> Cxt
define x a t cxt =
  define' x a (eval1 (_env cxt) a) t (eval1 (_env cxt) t) cxt
{-# inline define #-}

define' :: Name -> S.Ty -> V.Ty -> S.Tm1 -> V.Val1 -> Cxt -> Cxt
define' x a va t ~vt (Cxt env l loc ntbl src) =
  Cxt (V.Snoc1 env (V.Var1 l))
      (l + 1)
      (S.Define loc x a t)
      (addName x (NILocal1 l va) ntbl)
      src
{-# inline define' #-}
