
module Zonk (Tm(..), zonk, zonk0, castTm, unzonk) where

import Cxt
import Common hiding (Lvl)
import Errors
import ElabState

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
  | Erased String
  | Quote (Tm a)
  | Splice (Tm a) (Maybe String)
  | Return (Tm a)
  | Bind Name (Tm a) (Tm a)
  | Seq (Tm a) (Tm a)
  | New (Tm a)
  | Write (Tm a) (Tm a)
  | Read (Tm a)
  | NatLit Integer
  | Suc (Tm a)
  | NatElim (Tm a) (Tm a) (Tm a)
  | Rec [(Name, Tm a)]
  | Proj (Tm a) Name
  | CSP (Maybe Name) a
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
                              Solved v _          ->
                                pure $ Left v
                              Unsolved _ cxt a _ p ->
                                throwIO $ Error cxt $ UnsolvedMetaInZonk x (E.quote (lvl cxt) a)
      S.PostponedCheck x -> case lookupCheck x of
                              Left t -> Right <$!> go t
                              _      -> impossible
      S.App t u i        -> goSp' t >>= \case
                              Left v  -> pure $! Left $! E.vApp v (E.eval e u) i
                              Right t -> Right . App t <$!> go u
      S.Splice t pos     -> goSp' t >>= \case
                              Left v  -> pure $! Left $! E.vSplice v
                              Right t -> pure $ Right $ Splice t pos
      S.Proj t x         -> goSp' t >>= \case
                              Left v  -> pure $! Left $! E.vProj v x
                              Right t -> pure $ Right $ Proj t x
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
    S.Let x a t u        -> Let x <$!> go t <*!> goBind u
    S.AppPruning t pr    -> go (unAppPruning t pr)
    S.U                  -> pure $ Erased "⊘"
    S.Pi{}               -> pure $ Erased "⊘"
    t@S.Meta{}           -> goSp t
    t@S.PostponedCheck{} -> goSp t
    S.Quote t            -> Quote <$!> go t
    t@S.Splice{}         -> goSp t

    S.Return' _ t        -> Return <$!> go t
    S.Bind x t u         -> Bind x <$!> go t <*!> goBind u
    S.Seq t u            -> Seq <$!> go t <*!> go u
    S.Ref{}              -> pure $ Erased "⊘"
    S.New' _ t           -> New <$!> go t
    S.Write' _ t u       -> Write <$!> go t <*!> go u
    S.Read' _ t          -> Read <$!> go t
    S.Erased _           -> pure $ Erased "⊘"
    S.Nat                -> pure $ Erased "⊘"
    S.NatLit n           -> pure $ NatLit n
    S.Suc' n             -> Suc <$!> go n
    S.NatElim' p s z n   -> NatElim <$!> go s <*!> go z <*!> go n
    S.RecTy fs           -> pure $ Erased "⊘"
    S.Rec ts             -> Rec <$!> traverse (traverse go) ts
    S.Proj t x           -> Proj <$!> go t <*!> pure x
    S.Box' _             -> pure $ Erased "⊘"
    S.Eff' _             -> pure $ Erased "⊘"

    t@S.App{}            -> goSp t

    S.Return  -> pure $! Lam "A" $ Lam "a" $ Return (Var 0)
    S.New     -> pure $! Lam "A" $ Lam "a" $ New (Var 0)
    S.Write   -> pure $! Lam "A" $ Lam "t" $ Lam "u" $ Write (Var 1) (Var 0)
    S.Read    -> pure $! Lam "A" $ Lam "t" $ Read (Var 0)
    S.Suc     -> pure $! Lam "n" $ Suc (Var 0)
    S.NatElim -> pure $! Lam "P" $ Lam "s" $ Lam "z" $ Lam "n" $ NatElim (Var 2) (Var 1) (Var 0)
    S.Eff     -> pure $! Lam "A" $ Erased "⊘"
    S.Box     -> pure $! Lam "A" $ Erased "⊘"

zonk0 :: S.Tm -> IO (Tm Void)
zonk0 = zonk 0 []

unzonk :: Tm a -> S.Tm
unzonk = \case
  Var x         -> S.Var x
  Let x t u     -> S.Let x (S.Erased "⊘") (unzonk t) (unzonk u)
  Lam x t       -> S.Lam x Expl (unzonk t)
  App t u       -> S.App (unzonk t) (unzonk u) Expl
  Erased s      -> S.Erased s
  Quote t       -> S.Quote (unzonk t)
  Splice t pos  -> S.Splice (unzonk t) pos
  Return t      -> S.Return' (S.Erased "⊘") (unzonk t)
  Bind x t u    -> S.Bind x (unzonk t) (unzonk u)
  Seq t u       -> S.Seq (unzonk t) (unzonk u)
  New t         -> S.New' (S.Erased "⊘") (unzonk t)
  Write t u     -> S.Write' (S.Erased "⊘") (unzonk t) (unzonk u)
  Read t        -> S.Read' (S.Erased "⊘") (unzonk t)
  CSP x t       -> S.Erased $ maybe "*CSP*" (\x -> "*"++x++"*") x
  NatLit n      -> S.NatLit n
  Suc n         -> S.Suc' (unzonk n)
  NatElim s z n -> S.NatElim' (S.Erased "⊘") (unzonk s) (unzonk z) (unzonk n)
  Rec ts        -> S.Rec (fmap (fmap unzonk) ts)
  Proj t x      -> S.Proj (unzonk t) x
