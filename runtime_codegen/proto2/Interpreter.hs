{-# options_ghc -Wno-unused-imports #-}

module Interpreter where

import Common hiding (Lvl)
import Metacontext
import Errors
import Control.Exception
import qualified Common as C
import qualified Syntax as S
import qualified Evaluation as E
import qualified Value as V

import Data.IORef

--------------------------------------------------------------------------------

data Tm
  = Var Ix
  | Let Name Tm Tm
  | Lam Name Tm
  | App Tm Tm
  | Erased
  | Quote Tm
  | Splice Tm
  | Return Tm
  | Bind Name Tm Tm
  | Seq Tm Tm
  | New Tm
  | Write Tm Tm
  | Read Tm
  | CSP Closed

-- Zonking and erasure
--------------------------------------------------------------------------------

zonk :: C.Lvl -> V.Env -> S.Tm -> Tm
zonk l e = go where

  goSp :: S.Tm -> Either V.Val Tm
  goSp = \case
    S.Meta x    -> case lookupMeta x of
                     Solved v _ -> Left v
                     Unsolved{} -> throw $ UnsolvedMetaInZonk x
    S.App t u i -> case goSp t of
                     Left v  -> Left $! E.vApp v (E.eval e u) i
                     Right t -> Right $! App t (go u)
    S.Splice t  -> case goSp t of
                     Left v  -> Left $! E.vSplice v
                     Right t -> Right $! Splice t
    t           -> Right (go t)

  goBind :: S.Tm -> Tm
  goBind t = zonk (l + 1) (V.VVar l : e) t

  unAppPruning :: S.Tm -> S.Pruning -> S.Tm
  unAppPruning t = go 0 where
    go x []              = t
    go x (pr :> Nothing) = go (x + 1) pr
    go x (pr :> Just i ) = S.App (go (x + 1) pr) (S.Var x) i

  go :: S.Tm -> Tm
  go = \case
    S.Var x            -> Var x
    S.Lam x i t        -> Lam x (goBind t)
    t@S.App{}          -> either (go . E.quote l) id $ goSp t
    S.Let x a t u      -> Let x (go t) (goBind u)
    S.AppPruning t pr  -> go (unAppPruning t pr)
    S.U                -> Erased
    S.Pi{}             -> Erased
    S.Meta x           -> case lookupMeta x of
                            Solved v _ -> go (E.quote l v)
                            Unsolved{} -> throw $ UnsolvedMetaInZonk x
    S.PostponedCheck{} -> impossible
    S.Box{}            -> Erased
    S.Quote t          -> Quote (go t)
    t@S.Splice{}       -> either (go . E.quote l) id $ goSp t
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


-- Reference Interpreter
--------------------------------------------------------------------------------

type Lvl   = (?lvl   :: C.Lvl)
type Env a = (?env   :: [a])
type Stage = (?stage :: Int)
type CEnv  = Env Closed
type OEnv  = Env Open

data Closed
  = CLam Name (Closed -> Closed) (Open -> Open)
  | CAction (IO Closed)
  | CRef (IORef Closed)
  | CQuote Open
  | CErased

data Open
  = OVar C.Lvl
  | OLet Name Open (Lvl => Open)
  | OLam Name (Open -> Open)
  | OApp Open Open
  | OErased
  | OQuote Open
  | OSplice Open
  | OReturn Open
  | OBind Name Open (Lvl => Open)
  | OSeq Open Open
  | ONew Open
  | OWrite Open Open
  | ORead Open
  | OClosed Closed

def :: v -> (Env v => a) -> (Env v => a)
def t act = let ?env = t: ?env in act

stage :: Int -> (Stage => a) -> a
stage s act = let ?stage = s in act

lvl :: C.Lvl -> (Lvl => a) -> a
lvl l act = let ?lvl = l in act

env :: [v] -> (Env v => a) -> a
env e act = let ?env = e in act

fresh :: (Lvl => Open -> a) -> Lvl => a
fresh act = let v = OVar ?lvl in let ?lvl = ?lvl + 1 in act v

closeEnv :: (OEnv => a) -> CEnv => a
closeEnv act = let ?env = map OClosed ?env in act

idEnv :: C.Lvl -> [Open]
idEnv l = map OVar [l-1,l-2..0]

--------------------------------------------------------------------------------

eRun :: Closed -> IO Closed
eRun = \case
  CAction t -> t
  _         -> impossible

cApp :: Closed -> Closed -> Closed
cApp t u = case t of
  CLam x f g -> f u
  t          -> impossible

cSplice :: Closed -> Closed
cSplice = \case
  CQuote t -> env [] $ ceval $ lvl 0 $ gen t
  _        -> impossible

exec :: CEnv => Tm -> IO Closed
exec = \case
  Var x      -> eRun (?env !! coerce x)
  Let x t u  -> def (ceval t) (exec u)
  Lam x t    -> impossible
  App t u    -> eRun $ cApp (ceval t) (ceval u)
  Erased     -> impossible
  Quote t    -> impossible
  Splice t   -> eRun $ cSplice (ceval t)
  Return t   -> pure $! ceval t
  Bind x t u -> do {t <- exec t; def t $ exec u}
  Seq t u    -> exec t >> exec u
  New t      -> CRef <$!> (newIORef $! ceval t)
  Write t u  -> case ceval t of
                  CRef r -> CErased <$ (writeIORef r $! ceval u)
                  _      -> impossible
  Read t     -> case ceval t of
                  CRef r -> readIORef r
                  _      -> impossible
  CSP t      -> eRun t

ceval :: CEnv => Tm -> Closed
ceval = \case
  Var x      -> ?env !! coerce x
  Let _ t u  -> def (ceval t) (ceval u)
  Lam x t    -> CLam x (\v -> def v $ ceval t)
                       (\v -> closeEnv $ def v $ lvl 0 $ stage 0 $ oeval t)
  App t u    -> cApp (ceval t) (ceval u)
  Erased     -> CErased
  Quote t    -> CQuote (closeEnv $ lvl 0 $ stage 1 $ oeval t)
  Splice t   -> cSplice (ceval t)
  t@Return{} -> CAction (exec t)
  t@Bind{}   -> CAction (exec t)
  t@Seq{}    -> CAction (exec t)
  t@New{}    -> CAction (exec t)
  t@Write{}  -> CAction (exec t)
  t@Read{}   -> CAction (exec t)
  CSP t      -> t

oeval :: OEnv => Lvl => Stage => Tm -> Open
oeval = \case
  Var x      -> ?env !! coerce x
  Lam x t    -> OLam x \v -> def v $ oeval t
  Erased     -> OErased
  Quote t    -> OQuote $ stage (?stage + 1) $ oeval t
  Return t   -> OReturn (oeval t)
  Bind x t u -> OBind x (oeval t) (oeval u)
  Seq t u    -> OSeq (oeval t) (oeval u)
  New t      -> ONew (oeval t)
  Write t u  -> OWrite (oeval t) (oeval u)
  Read t     -> ORead (oeval t)
  CSP t      -> OClosed t
  t -> case ?stage of
    0 -> case t of
      Let x t u  -> def (oeval t) (oeval u)
      App t u    -> case (oeval t, oeval u) of
                      (OClosed (CLam _ f _), OClosed u) -> OClosed (f u)
                      (OClosed (CLam _ _ f), u        ) -> f u
                      (OLam _ f            , u        ) -> f u
                      (t                   , u        ) -> OApp t u
      Splice t   -> case oeval t of
                      OQuote t -> env (idEnv ?lvl) $ oeval $ gen t
                      t        -> OSplice t
    _ -> case t of
      Let x t u  -> OLet x (oeval t) (oeval u)
      App t u    -> OApp (oeval t) (oeval u)
      Splice t   -> case stage (?stage - 1) $ oeval t of
                      OQuote t -> t
                      t        -> OSplice t

gen :: Lvl => Open -> Tm
gen = \case
  OVar x      -> Var (coerce (?lvl - x - 1))
  OLet x t u  -> Let x (gen t) $ fresh \_ -> gen u
  OLam x t    -> Lam x $ fresh \v -> gen (t v)
  OApp t u    -> App (gen t) (gen u)
  OErased     -> Erased
  OQuote t    -> Quote (gen t)
  OSplice t   -> Splice (gen t)
  OReturn t   -> Return (gen t)
  OBind x t u -> Bind x (gen t) $ fresh \_ -> gen u
  OSeq t u    -> Seq (gen t) (gen u)
  ONew t      -> New (gen t)
  OWrite t u  -> Write (gen t) (gen u)
  ORead t     -> Read (gen t)
  OClosed t   -> CSP t
