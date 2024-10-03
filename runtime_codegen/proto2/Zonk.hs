
module Zonk (Tm(..), zonk, castTm) where

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

zonk :: C.Lvl -> V.Env -> S.Tm -> Tm Void
zonk l e = go where

  goSp :: S.Tm -> Tm Void
  goSp t = either (go . E.quote l) id (goSp' t) where

    goSp' :: S.Tm -> Either V.Val (Tm Void)
    goSp' = \case
      S.Meta x    -> case lookupMeta x of
                       Solved v _ -> Left v
                       Unsolved{} -> throw $ UnsolvedMetaInZonk x
      S.App t u i -> case goSp' t of
                       Left v  -> Left $! E.vApp v (E.eval e u) i
                       Right t -> Right $! App t (go u)
      S.Splice t  -> case goSp' t of
                       Left v  -> Left $! E.vSplice v
                       Right t -> Right $! Splice t
      t           -> Right (go t)

  goBind :: S.Tm -> Tm Void
  goBind t = zonk (l + 1) (V.VVar l : e) t

  unAppPruning :: S.Tm -> S.Pruning -> S.Tm
  unAppPruning t = go 0 where
    go x []              = t
    go x (pr :> Nothing) = go (x + 1) pr
    go x (pr :> Just i ) = S.App (go (x + 1) pr) (S.Var x) i

  go :: S.Tm -> Tm Void
  go = \case
    S.Var x            -> Var x
    S.Lam x i t        -> Lam x (goBind t)
    t@S.App{}          -> goSp t
    S.Let x a t u      -> Let x (go t) (goBind u)
    S.AppPruning t pr  -> go (unAppPruning t pr)
    S.U                -> Erased
    S.Pi{}             -> Erased
    t@S.Meta{}         -> goSp t
    S.PostponedCheck{} -> impossible
    S.Box{}            -> Erased
    S.Quote t          -> Quote (go t)
    t@S.Splice{}       -> goSp t
    S.Unit             -> Erased
    S.Tt               -> Erased
    S.Eff{}            -> Erased
    S.Return t         -> Return (go t)
    S.Bind x t u       -> Bind x (go t) (goBind u)
    S.Seq t u          -> Seq (go t) (go u)
    S.Ref{}            -> Erased
    S.New t            -> New (go t)
    S.Write t u        -> Write (go t) (go u)
    S.Read t           -> Read (go t)
