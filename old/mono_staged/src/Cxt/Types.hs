
module Cxt.Types where

import Lens.Micro.TH

import Common
import qualified Syntax as S
import qualified Values as V
import Data.HashMap.Strict

data NameInfo
 = NameInfo0 Lvl V.Ty V.CV
 | NameInfo1 Lvl V.Ty

type NameTable = HashMap RawName NameInfo

data Cxt = Cxt {
  cxtEnv       :: V.Env,
  cxtLvl       :: Lvl,
  cxtLocals    :: S.Locals,
  cxtPruning   :: S.Pruning,
  cxtNameTable :: NameTable,
  cxtNames     :: [Name],
  cxtSrc       :: RawName
  }

makeFields ''Cxt
