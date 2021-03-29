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

data Infer = forall s. Infer (S.Tm s) V.Ty (U s)

insert' :: Cxt -> IO Infer -> IO Infer
insert' = undefined

insert :: Cxt -> IO Infer -> IO Infer
insert = undefined

insertUntilName :: Cxt -> P.Tm -> RawName -> IO Infer -> IO Infer
insertUntilName = undefined

checkRecCon0 :: Cxt -> [(Span, P.Tm)] -> Fields V.Ty -> IO S.Tm0
checkRecCon0 = undefined

checkRecCon1 :: Cxt -> [(Span, P.Tm)] -> Fields V.Ty -> IO S.Tm1
checkRecCon1 = undefined

checkRecCon :: Cxt -> [(Span, P.Tm)] -> Fields V.Ty -> U s -> IO (S.Tm s)
checkRecCon cxt t as = \case
  U0{} -> checkRecCon0 cxt t as
  U1   -> checkRecCon1 cxt t as

check :: forall s. Cxt -> P.Tm -> V.Ty -> U s -> IO (S.Tm s)
check cxt topT topA topU = case (topT, forceFU topA, topU) of

  (P.Lam _ x i ma t, V.Pi x' i' a' b', U1)
    | case i of NoName i                    -> i == i'
                Named (spanToName cxt -> x) -> x == x' -> do
    let qa' = quote cxt a'
    case x of
      P.Bind (spanToRawName cxt -> x) ->
        S.Lam1 (NName x) i' qa' <$>
          check (bind' (NName x) qa' a' U1 cxt) t (b' $$ V.Var (_lvl cxt)) U1
      P.DontBind ->
        S.Lam1 NEmpty i' qa' <$>
          check (bind' NEmpty qa' a' U1 cxt) t (b' $$ V.Var (_lvl cxt)) U1

  (t, V.Pi x' Impl a' b', U1) -> do
    let qa' = quote cxt a'
    S.Lam1 x' Impl qa' <$>
      check (newBinder1 x' qa' cxt) t (b' $$ V.Var (_lvl cxt)) topU

  (P.RecCon _ ts, V.Rec as, topU) -> do
    checkRecCon cxt ts as topU



infer :: Cxt -> P.Tm -> IO Infer
infer = undefined


{-

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
-}
