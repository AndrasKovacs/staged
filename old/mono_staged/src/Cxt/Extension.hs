
module Cxt.Extension where

import Common
import Cxt.Types
import qualified Values as V
import qualified Syntax as S
import qualified Data.HashMap.Strict as M
import Evaluation

emptyCxt :: RawName -> Cxt
emptyCxt = Cxt V.Nil 0 S.Empty [] mempty []
{-# inline emptyCxt #-}

addName :: Name -> NameInfo -> NameTable -> NameTable
addName NEmpty    _   tbl = tbl
addName (NName x) inf tbl = M.insert x inf tbl
addName NX        _   tbl = tbl
{-# inline addName #-}

bind0 :: Name -> S.Ty -> S.CV -> Cxt -> Cxt
bind0 x a acv cxt = bind0' x a (eval1 (cxt^.env) a) acv (eval1 (cxt^.env) acv) cxt
{-# inline bind0 #-}

bind0' :: Name -> S.Ty -> V.Ty -> S.CV -> V.CV -> Cxt -> Cxt
bind0' x a va acv vacv (Cxt env l loc pr ntbl ns src) =
  Cxt (V.Snoc0 env l)
      (l + 1)
      (S.Bind0 loc a acv)
      (pr :> S.PEBind0)
      (addName x (NameInfo0 l va vacv) ntbl)
      (x:ns)
      src
{-# inline bind0' #-}

bind1 :: Name -> S.Ty -> Cxt -> Cxt
bind1 x a cxt = bind1' x a (eval1 (cxt^.env) a) cxt
{-# inline bind1 #-}

bind1' :: Name -> S.Ty -> V.Ty -> Cxt -> Cxt
bind1' x a va (Cxt env l loc pr ntbl ns src) =
  Cxt (V.Snoc1 env (V.Var1 l))
      (l + 1)
      (S.Bind1 loc a)
      (pr :> S.PEBind1 Expl)
      (addName x (NameInfo1 l va) ntbl)
      (x:ns)
      src
{-# inline bind1' #-}

newBinder :: Name -> S.Ty -> Cxt -> Cxt
newBinder x a (Cxt env l loc pr ntbl ns src) =
  Cxt (V.Snoc1 env (V.Var1 l))
      (l + 1)
      (S.Bind1 loc a)
      (pr :> S.PEBind1 Expl)
      ntbl
      (x:ns)
      src
{-# inline newBinder #-}

define :: Name -> S.Ty -> S.Tm1 -> Cxt -> Cxt
define x a t cxt = define' x a (eval1 (cxt^.env) a) t (eval1 (cxt^.env) t) cxt
{-# inline define #-}

define' :: Name -> S.Ty -> V.Ty -> S.Tm1 -> V.Val1 -> Cxt -> Cxt
define' x a va t ~vt (Cxt env l loc pr ntbl ns src) =
  Cxt (V.Snoc1 env (V.Var1 l))
      (l + 1)
      (S.Define loc a t)
      (pr :> S.PESkip)
      (addName x (NameInfo1 l va) ntbl)
      (x:ns)
      src
{-# inline define' #-}
