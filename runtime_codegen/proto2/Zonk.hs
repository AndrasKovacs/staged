
module Zonk (Tm(..), zonk, zonk0, castTm, unzonk) where

import Cxt
import Common hiding (Lvl)
import Errors
import Metacontext

import qualified Common as C
import qualified Evaluation as E
import qualified Syntax as S
import qualified Value as V

import Control.Exception
import Data.Void
import Unsafe.Coerce

--------------------------------------------------------------------------------

data Tm a
  = Var Ix
  | Let Name (Tm a) (Tm a)
  | Lam Name (Tm a)
  | App (Tm a) (Tm a)
  | Erased
  | Quote (Tm a)
  | Splice (Tm a)
  | Return (Tm a)
  | Bind Name (Tm a) (Tm a)
  | Seq (Tm a) (Tm a)
  | New (Tm a)
  | Write (Tm a) (Tm a)
  | Read (Tm a)
  | CSP a
  deriving Show

castTm :: Tm Void -> Tm a
castTm = unsafeCoerce

zonk :: C.Lvl -> V.Env -> S.Tm -> IO (Tm Void)
zonk l e = go where

  goSp :: S.Tm -> IO (Tm Void)
  goSp t = goSp' t >>= either (go . E.quote l) pure where

    goSp' :: S.Tm -> IO (Either V.Val (Tm Void))
    goSp' = \case
      S.Meta x           -> case lookupMeta x of
                              Solved v _     -> pure $ Left v
                              Unsolved _ _ p -> throwIO $ Error (emptyCxt p) $ UnsolvedMetaInZonk x
      S.PostponedCheck x -> case lookupCheck x of
                              Checked t   -> Right <$!> go t
                              Unchecked{} -> impossible
      S.App t u i        -> goSp' t >>= \case
                              Left v  -> pure $! Left $! E.vApp v (E.eval e u) i
                              Right t -> Right . App t <$!> go u
      S.Splice t         -> goSp' t >>= \case
                              Left v  -> pure $! Left $! E.vSplice v
                              Right t -> pure $ Right $ Splice t
      t                  -> Right <$!> go t

  goBind :: S.Tm -> IO (Tm Void)
  goBind t = zonk (l + 1) (V.VVar l : e) t

  unAppPruning :: S.Tm -> S.Pruning -> S.Tm
  unAppPruning t = go 0 where
    go x []              = t
    go x (pr :> Nothing) = go (x + 1) pr
    go x (pr :> Just i ) = S.App (go (x + 1) pr) (S.Var x) i

  go :: S.Tm -> IO (Tm Void)
  go = \case
    S.Var x              -> pure $ Var x
    S.Lam x i t          -> Lam x <$!> goBind t
    t@S.App{}            -> goSp t
    S.Let x a t u        -> Let x <$!> go t <*!> goBind u
    S.AppPruning t pr    -> go (unAppPruning t pr)
    S.U                  -> pure Erased
    S.Pi{}               -> pure Erased
    t@S.Meta{}           -> goSp t
    t@S.PostponedCheck{} -> goSp t
    S.Box{}              -> pure Erased
    S.Quote t            -> Quote <$!> go t
    t@S.Splice{}         -> goSp t
    S.Unit               -> pure Erased
    S.Tt                 -> pure Erased
    S.Eff{}              -> pure Erased
    S.Return t           -> Return <$!> go t
    S.Bind x t u         -> Bind x <$!> go t <*!> goBind u
    S.Seq t u            -> Seq <$!> go t <*!> go u
    S.Ref{}              -> pure Erased
    S.New t              -> New <$!> go t
    S.Write t u          -> Write <$!> go t <*!> go u
    S.Read t             -> Read <$!> go t
    S.Erased             -> pure Erased

zonk0 :: S.Tm -> IO (Tm Void)
zonk0 = zonk 0 []

unzonk :: Tm Void -> S.Tm
unzonk = \case
  Var x      -> S.Var x
  Let x t u  -> S.Let x S.Erased (unzonk t) (unzonk u)
  Lam x t    -> S.Lam x Expl (unzonk t)
  App t u    -> S.App (unzonk t) (unzonk u) Expl
  Erased     -> S.Erased
  Quote t    -> S.Quote (unzonk t)
  Splice t   -> S.Splice (unzonk t)
  Return t   -> S.Return (unzonk t)
  Bind x t u -> S.Bind x (unzonk t) (unzonk u)
  Seq t u    -> S.Seq (unzonk t) (unzonk u)
  New t      -> S.New (unzonk t)
  Write t u  -> S.Write (unzonk t) (unzonk u)
  Read t     -> S.Read (unzonk t)
