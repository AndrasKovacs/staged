
module Cxt where

import qualified Data.HashMap.Strict as M

import Common
import qualified Syntax as S
import qualified Values as V

data NameInfo
  = NITop Lvl V.Ty U
  | NILocal Lvl V.Ty U

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

-- bind' :: RawName -> S.Ty -> V.Ty -> U -> Cxt -> Cxt
-- bind' x a va au (Cxt env l loc ntbl src) =
--   Cxt (V.Snoc env ()
--       (l + 1)
--       _
--       (M.insert x (NILocal l va au) ntbl)
--       src
-- {-# inline bind' #-}
