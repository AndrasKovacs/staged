{-# options_ghc -Wno-unused-imports #-}

module Interpreter where

import Common hiding (Lvl)
import qualified Common as C
import qualified Syntax as S

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
  | New Tm
  | Write Tm Tm
  | Read Tm
  | CSP Closed

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
  | ONew Open
  | OWrite Open Open
  | ORead Open
  | OClosed Closed

-- Zonking and erasure
--------------------------------------------------------------------------------




-- Reference interpretation
--------------------------------------------------------------------------------

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

eRead :: Closed -> IO Closed
eRead = \case
  CRef r -> readIORef r
  _      -> impossible

eWrite :: Closed -> Closed -> IO Closed
eWrite t u = case t of
  CRef r -> CErased <$ writeIORef r u
  _      -> impossible

cApp :: Closed -> Closed -> Closed
cApp t u = case t of
  CLam x f g -> f u
  t          -> impossible

oApp :: Open -> Open -> Open
oApp t u = case (t, u) of
  (OClosed (CLam _ f _), OClosed u) -> OClosed (f u)
  (OClosed (CLam _ _ f), u        ) -> f u
  (OLam _ f            , u        ) -> f u
  (t                   , u        ) -> OApp t u

--------------------------------------------------------------------------------

exec :: CEnv => Tm -> IO Closed
exec = \case
  Var x      -> eRun (?env !! coerce x)
  Let x t u  -> def (ceval t) (exec u)
  Lam x t    -> impossible
  App t u    -> eRun (cApp (ceval t) (ceval u))
  Erased     -> impossible
  Quote t    -> impossible
  Splice t   -> eRun undefined
  Return t   -> pure $! ceval t
  Bind x t u -> do {t <- exec t; def t $ exec u}
  New t      -> CRef <$!> (newIORef $! ceval t)
  Write t u  -> eWrite (ceval t) (ceval u)
  Read t     -> eRead (ceval t)
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
  Splice t   -> case ceval t of CQuote t -> env [] $ ceval $ lvl 0 $ gen t
                                _        -> impossible
  t@Return{} -> CAction (exec t)
  t@Bind{}   -> CAction (exec t)
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
  New t      -> ONew (oeval t)
  Write t u  -> OWrite (oeval t) (oeval u)
  Read t     -> ORead (oeval t)
  CSP t      -> OClosed t
  t -> case ?stage of
    0 -> case t of
      Let x t u  -> def (oeval t) (oeval u)
      App t u    -> oApp (oeval t) (oeval u)
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
  ONew t      -> New (gen t)
  OWrite t u  -> Write (gen t) (gen u)
  ORead t     -> Read (gen t)
  OClosed t   -> CSP t
