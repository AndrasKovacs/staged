
module Cxt.Types where

import Common
import qualified Syntax as S
import qualified Values as V
import Data.HashMap.Strict

data NameInfo
  = NITop0 Lvl V.Ty CV
  | NITop1 Lvl V.Ty
  | NILocal0 Lvl V.Ty CV
  | NILocal1 Lvl V.Ty

type NameTable = HashMap RawName NameInfo

data Cxt = Cxt {
  _env       :: V.Env,
  _lvl       :: Lvl,
  _locals    :: S.Locals,
  _nameTable :: NameTable,
  _src       :: RawName
  }
