{-# options_ghc -Wno-unused-imports #-}

module Elaboration where

import Control.Monad

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as HM
import qualified FlatParse.Stateful  as FP

import qualified Syntax              as S
import qualified Values              as V
import qualified Presyntax           as P
import qualified Evaluation          as Eval

import Common
import Cxt
import ElabState
import EvalInCxt
import Exceptions
import Unification

--------------------------------------------------------------------------------

spanToRawName :: Cxt -> Span -> RawName
spanToRawName cxt span =
  coerce $ FP.unsafeSlice (coerce $ _src cxt) span
{-# inline spanToRawName #-}

spanToName :: Cxt -> Span -> Name
spanToName cxt = NName . spanToRawName cxt
{-# inline spanToName #-}

bindToName :: Cxt -> P.Bind -> Name
bindToName cxt (P.Bind x) = NName (spanToRawName cxt x)
bindToName cxt P.DontBind = NEmpty
{-# inline bindToName #-}

--------------------------------------------------------------------------------

elabError :: Cxt -> P.Tm -> ElabError -> IO a
elabError cxt tgt err = throwIO $ ElabError (_locals cxt) tgt err
{-# inline elabError #-}

data Infer = Infer0 S.Tm0 V.Ty U | Infer1 S.Tm1 V.Ty

check0 :: Cxt -> P.Tm -> V.Ty -> U -> IO S.Tm0
check0 cxt topmostT topA au = case (topmostT, forceFU1 topA) of
  (P.Lam _ x i ma t, V.Fun a b) -> _


check1 :: Cxt -> P.Tm -> V.Ty -> U -> IO S.Tm1
check1 = undefined

infer0 :: Cxt -> P.Tm -> IO Infer
infer0 = undefined

infer1 :: Cxt -> P.Tm -> IO Infer
infer1 = undefined
