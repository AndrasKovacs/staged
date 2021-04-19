
module Cxt.Types where

import Common
import qualified Syntax as S
import qualified Values as V
import Data.HashMap.Strict

data NameInfo  = NameInfo Lvl V.Ty U
type NameTable = HashMap RawName NameInfo

data Cxt = Cxt {
  _env       :: V.Env,
  _lvl       :: Lvl,
  _locals    :: S.Locals,
  _nameTable :: NameTable,
  _src       :: RawName
  }
