
module Interpreter (execTop, readBackClosed) where

import Common hiding (Lvl)
import Zonk   hiding (Tm)

import qualified Zonk   as Z
import qualified Common as C
import Pretty
import ElabState
import Errors

import Data.IORef
import System.IO.Unsafe

--------------------------------------------------------------------------------

type Tm    = Z.Tm Closed
type Lvl   = (?lvl   :: C.Lvl)
type Env a = (?env   :: [a])
type Stage = (?stage :: Int)
type CEnv  = Env Closed
type OEnv  = Env Open

data Closed
  = CLam Name (NoShow (Closed -> Closed)) (NoShow (Open -> Open))
  | CAction (NoShow (IO Closed))
  | CRef (NoShow (IORef Closed))
  | CQuote Open
  | CErased String
  deriving Show

data Open
  = OVar C.Lvl
  | OLet Name Open (NoShow (C.Lvl -> Open))
  | OLam Name (NoShow (Open -> Open))
  | OApp Open Open
  | OErased String
  | OQuote Open
  | OSplice Open
  | OReturn Open
  | OBind Name Open (NoShow (C.Lvl -> Open))
  | OSeq Open Open
  | ONew Open
  | OWrite Open Open
  | ORead Open
  | OClosed Closed
  deriving Show

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
  CAction t -> coerce t
  _         -> impossible

cApp :: Closed -> Closed -> Closed
cApp t u = case t of
  CLam x f g -> coerce f u
  t          -> impossible

cSplice :: Closed -> Maybe SourcePos -> Closed
cSplice t pos = case t of
  CQuote t -> env [] $ lvl 0 $ ceval $ traceGen t pos
  _        -> impossible

exec :: CEnv => Tm -> IO Closed
exec = \case
  Var x      -> eRun (?env !! coerce x)
  Let x t u  -> def (ceval t) (exec u)
  Lam x t    -> impossible
  App t u    -> eRun $ cApp (ceval t) (ceval u)
  Erased{}   -> impossible
  Quote t    -> impossible
  Splice t pos -> eRun $ cSplice (ceval t) pos
  Return t   -> pure $! ceval t
  Bind x t u -> do {t <- exec t; def t $ exec u}
  Seq t u    -> exec t >> exec u
  New t      -> CRef <$!> (coerce (newIORef $! ceval (coerce t)))
  Write t u  -> case ceval t of
                  CRef r -> CErased "tt" <$ (writeIORef (coerce r) $! ceval u)
                  _      -> impossible
  Read t     -> case ceval t of
                  CRef r -> readIORef (coerce r)
                  _      -> impossible
  CSP t      -> eRun t

ceval :: CEnv => Tm -> Closed
ceval = \case
  Var x        -> ?env !! coerce x
  Let _ t u    -> def (ceval t) (ceval u)
  Lam x t      -> CLam x (coerce (\v -> def v $ ceval t))
                         (coerce (\v -> closeEnv $ def v $ lvl 0 $ stage 0 $ oeval t))
  App t u      -> cApp (ceval t) (ceval u)
  Erased s     -> CErased s
  Quote t      -> CQuote (closeEnv $ lvl 0 $ stage 1 $ oeval t)
  Splice t pos -> cSplice (ceval t) pos
  t@Return{}   -> CAction (coerce (exec t))
  t@Bind{}     -> CAction (coerce (exec t))
  t@Seq{}      -> CAction (coerce (exec t))
  t@New{}      -> CAction (coerce (exec t))
  t@Write{}    -> CAction (coerce (exec t))
  t@Read{}     -> CAction (coerce (exec t))
  CSP t        -> t

oeval :: OEnv => Lvl => Stage => Tm -> Open
oeval = \case
  Var x      -> ?env !! coerce x
  Lam x t    -> OLam x (coerce \v -> def v $ oeval t)
  Erased s   -> OErased s
  Quote t    -> OQuote $ stage (?stage + 1) $ oeval t
  Return t   -> OReturn (oeval t)
  Bind x t u -> OBind x (oeval t) (NoShow \l -> lvl l $ oeval u)
  Seq t u    -> OSeq (oeval t) (oeval u)
  New t      -> ONew (oeval t)
  Write t u  -> OWrite (oeval t) (oeval u)
  Read t     -> ORead (oeval t)
  CSP t      -> OClosed t
  t -> case ?stage of
    0 -> case t of
      Let x t u    -> def (oeval t) (oeval u)
      App t u      -> case (oeval t, oeval u) of
                        (OClosed (CLam _ f _), OClosed u) -> OClosed (coerce f u)
                        (OClosed (CLam _ _ f), u        ) -> coerce f u
                        (OLam _ f            , u        ) -> coerce f u
                        (t                   , u        ) -> OApp t u
      Splice t pos -> case oeval t of
                        OQuote t -> env (idEnv ?lvl) $ oeval $ traceGen t pos
                        t        -> OSplice t
    _ -> case t of
      Let x t u    -> OLet x (oeval t) (NoShow \l -> lvl l $ oeval u)
      App t u      -> OApp (oeval t) (oeval u)
      Splice t pos -> case stage (?stage - 1) $ oeval t of
                        OQuote t -> t
                        t        -> OSplice t

traceGen :: Lvl => Open -> Maybe SourcePos -> Tm
traceGen t pos =
  let t' = gen t
      freevars = map (\l -> "x" ++ show l) [0.. ?lvl - 1]
      displayCode  = prettyTm 0 0 freevars (Z.unzonk t') "" in
  case pos of
    Nothing ->
      trace ("CODE GENERATED: \n\n" ++ displayCode ++ "\n") t'
    Just pos ->
      let file = unsafeDupablePerformIO (readIORef sourceCode)
      in trace ("CODE GENERATED AT:\n" ++ displayLocation pos file ++ "\nCODE:\n  " ++ displayCode ++ "\n") t'

gen :: Lvl => Open -> Tm
gen = \case
  OVar x      -> Var (coerce (?lvl - x - 1))
  OLet x t u  -> Let x (gen t) $ fresh \_ -> gen (coerce u ?lvl)
  OLam x t    -> Lam x $ fresh \v -> gen (coerce t v)
  OApp t u    -> App (gen t) (gen u)
  OErased s   -> Erased s
  OQuote t    -> Quote (gen t)
  OSplice t   -> Splice (gen t) Nothing
  OReturn t   -> Return (gen t)
  OBind x t u -> Bind x (gen t) $ fresh \_ -> gen (coerce u ?lvl)
  OSeq t u    -> Seq (gen t) (gen u)
  ONew t      -> New (gen t)
  OWrite t u  -> Write (gen t) (gen u)
  ORead t     -> Read (gen t)
  OClosed t   -> CSP t

-- Only for pretty printing purposes
readBackClosed :: Closed -> Tm
readBackClosed t = let ?lvl = 0 in case t of
  CLam x _ t -> Lam x $ fresh \v -> gen (coerce t v)
  CAction _  -> Erased "Action"
  CRef _     -> Erased "Ref"
  CQuote t   -> Quote (gen t)
  CErased s  -> Erased s

execTop :: Tm -> IO Closed
execTop t = env [] $ exec t
