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

elabError :: Cxt -> P.Tm -> ElabError -> IO a
elabError cxt tgt err = throwIO $ ElabError (_locals cxt) tgt err
{-# inline elabError #-}

data Infer = forall s. Infer (S.Tm s) V.Ty (U s)


-- Converting spans to names
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


-- Implicit insertions
--------------------------------------------------------------------------------

insert' :: Cxt -> IO Infer -> IO Infer
insert' = undefined

insert :: Cxt -> IO Infer -> IO Infer
insert = undefined

insertUntilName :: Cxt -> P.Tm -> RawName -> IO Infer -> IO Infer
insertUntilName = undefined


-- Record checking
--------------------------------------------------------------------------------

checkRecCon0 :: Cxt -> [(Span, P.Tm)] -> Fields V.Ty -> IO (Fields S.Tm0)
checkRecCon0 cxt ts as = let
  go = checkRecCon0 cxt; {-# inline go #-}
  in case (ts, as) of
    ([], FNil) -> pure FNil
    ((spanToName cxt -> x, t):ts, FCons x' a fs) -> undefined
    _ -> undefined

checkRecCon1 :: Cxt -> [(Span, P.Tm)] -> Fields V.Ty -> IO (Fields S.Tm1)
checkRecCon1 = undefined

-- coercion
--------------------------------------------------------------------------------

coe :: Cxt -> S.Tm s1 -> V.Ty -> U s1 -> V.Ty -> U s2 -> IO (S.Tm s2)
coe = undefined


--------------------------------------------------------------------------------

check0 :: Cxt -> P.Tm -> V.Ty -> CV -> IO S.Tm0
check0 cxt topT topA cv = case (topT, forceFU topA, forceCV cv) of

  (P.Lam _ (bindToName cxt -> x) i ma t, V.Fun a' b', cv) -> do
    case i of NoName Expl -> pure ()
              _           -> elabError cxt topT NoImplicitLam0
    let qa' = quote cxt a'
    S.Lam0 x qa' <$>
      check0 (bind' x qa' a' (U0 V) cxt) t b' C

  (P.RecCon _ ts, V.Rec as, _) ->
    S.RecCon <$> checkRecCon0 cxt ts as

  (P.Tuple _ ts, V.Rec as, _) ->
    undefined

  (P.EmptyRec _, V.Rec FNil, _) ->
    pure $ S.RecCon FNil

  (P.Let0 _ (spanToName cxt -> x) ma t u, topA, cv) -> do
    acv  <- freshCV
    let au = U0 acv
    a <- maybeCheckU cxt ma au
    let va = eval1 cxt a
    t <- check0 cxt t va acv
    u <- check0 (bind' x a va au cxt) u topA cv
    pure $ S.Let x a t u

  (P.Hole _, topA, cv) -> do
    S.Down <$> freshMeta cxt (S.Lift cv (quote cxt topA))

  (P.Down _ t, topA, cv) -> do
    S.down <$> check1 cxt t (V.Lift cv topA)

  (t, topA, cv) -> do
    Infer t a au <- insert cxt $ infer cxt t
    coe cxt t a au topA (U0 cv)


check1 :: Cxt -> P.Tm -> V.Ty -> IO S.Tm1
check1 cxt topT topA = case (topT, forceFU topA) of
  (P.Lam _ (bindToName cxt -> x) i ma t, V.Pi x' i' a' b')
    | case i of NoName i                    -> i == i'
                Named (spanToName cxt -> x) -> x == x' -> do
    let qa' = quote cxt a'
    S.Lam1 x' i' qa' <$>
      check1 (bind' x (quote cxt a') a' U1 cxt) t (b' $$ V.Var (_lvl cxt))

  (t, V.Pi x' Impl a' b') -> do
    let qa' = quote cxt a'
    S.Lam1 x' Impl qa' <$>
      check1 (newBinder1 x' qa' cxt) t (b' $$ V.Var (_lvl cxt))

  (P.Lift _ t, V.U U1) -> do
    cv <- freshCV
    S.Lift cv <$> check1 cxt t (V.U (U0 cv))

  (P.RecCon _ ts, V.Rec as) -> do
    S.RecCon <$> checkRecCon1 cxt ts as

  (P.Let1 _ (spanToName cxt -> x) ma t u, topA) -> do
    a <- maybeCheckU cxt ma U1
    let va = eval1 cxt a
    t <- check1 cxt t va
    let ~vt = eval1 cxt t
    u <- check1 (define' x a va t vt cxt) u topA
    pure $ S.Let x a t u

  (P.Up _ t, V.Lift cv topA) -> do
    S.up <$> check0 cxt t topA cv

  (t, V.Lift cv topA) -> do
    S.up <$> check0 cxt t topA cv

  (P.Hole _, topA) -> do
    freshMeta cxt (quote cxt topA)

  (t, topA) -> do
    Infer t a au <- insert cxt $ infer cxt t
    coe cxt t a au topA U1


maybeCheckU :: Cxt -> Maybe P.Tm -> U s -> IO S.Ty
maybeCheckU cxt ma un =
  maybe (freshMeta cxt (S.U un)) (\a -> check1 cxt a (V.U un)) ma
{-# inline maybeCheckU #-}

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
