
module Interpreter (execTop, readBackClosed) where

import Common hiding (Lvl)
import Zonk   hiding (Tm)

import qualified Zonk   as Z
import qualified Common as C
import Pretty

import Data.IORef

--------------------------------------------------------------------------------

type Tm    = Z.Tm Closed
type Lvl   = (?lvl   :: C.Lvl)
type Env a = (?env   :: [(Name, a)])
type Stage = (?stage :: Int)
type CEnv  = Env Closed
type OEnv  = Env Open

data Closed
  = CLam Name (NoShow (Closed -> Closed)) (NoShow (C.Lvl -> Open -> Open))
  | CAction (NoShow (IO Closed))
  | CRef (NoShow (IORef Closed))
  | CQuote Open
  | CErased String
  | CRec [(Name, Closed)]
  | CNat Integer
  deriving Show

data Open
  = OVar C.Lvl
  | OLet Name Open (NoShow (C.Lvl -> Open))
  | OLam Name (NoShow (C.Lvl -> Open -> Open))
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
  | OClosed (Maybe Name) Closed
  | ONatLit Integer
  | OSuc Open
  | ONatElim Open Open Open
  | OProj Open Name
  | ORec [(Name, Open)]
  | OOpen [Name] Open (NoShow (C.Lvl -> Open))
  deriving Show

def :: Name -> v -> (Env v => a) -> (Env v => a)
def x t act = let ?env = (x, t): ?env in act

stage :: Int -> (Stage => a) -> a
stage s act = let ?stage = s in act

lvl :: C.Lvl -> (Lvl => a) -> a
lvl l act = let ?lvl = l in act

env :: [(Name, v)] -> (Env v => a) -> a
env e act = let ?env = e in act

fresh :: (Lvl => Open -> a) -> Lvl => a
fresh act = let v = OVar ?lvl in let ?lvl = ?lvl + 1 in act v

freshes :: [Name] -> (Lvl => a) -> Lvl => a
freshes xs act = let ?lvl = ?lvl + coerce (length xs) in seq ?lvl act

closeEnv :: (OEnv => a) -> CEnv => a
closeEnv act = let ?env = map (\(x, v) -> (x, OClosed (Just x) v)) ?env in act

idEnv :: C.Lvl -> [(Name, Open)]
idEnv l = map (\x -> ("lvl"++show x, OVar x)) [l-1,l-2..0]

--------------------------------------------------------------------------------

eRun :: Closed -> IO Closed
eRun = \case
  CAction t -> coerce t
  _         -> impossible

cApp :: Closed -> Closed -> Closed
cApp t u = case t of
  CLam x f g -> coerce f u
  t          -> error $ show t

cProj :: Closed -> Name -> Closed
cProj t x = case t of
  CRec ts -> fromJust $ lookup x ts
  _       -> impossible

cSplice :: Closed -> Maybe String -> Closed
cSplice t loc = case t of
  CQuote t -> env [] $ lvl 0 $ ceval $ traceGen t loc
  _        -> impossible

cSuc :: Closed -> Closed
cSuc = \case
  CNat n -> CNat (n + 1)
  _      -> impossible

cNatElim :: Closed -> Closed -> Closed -> Closed
cNatElim s z n =
  let go 0 = z
      go n = s `cApp` CNat (n - 1) `cApp` go (n - 1)
  in case n of
    CNat n -> go n
    _      -> impossible

cOpen :: [Name] -> Closed -> (CEnv => a) -> CEnv => a
cOpen xs t u = let ?env = foldl' (\env x -> ((x,) $! cProj t x) : env) ?env xs
               in seq ?env u

oNatElim :: Lvl => Open -> Open -> Open -> Open
oNatElim s z n =
  let go 0 = z
      go n = s `oApp` ONatLit (n - 1) `oApp` go (n - 1)
  in case n of
    ONatLit n -> go n
    OSuc n    -> s `oApp` n `oApp` oNatElim s z n
    n         -> ONatElim s z n

oQuote :: Open -> Open
oQuote = \case
  OSplice t -> t
  t         -> OQuote t

exec :: CEnv => Tm -> IO Closed
exec = \case
  Var x         -> eRun (snd (?env !! coerce x))
  Let x t u     -> def x (ceval t) (exec u)
  Lam x t       -> impossible
  App t u       -> eRun $ cApp (ceval t) (ceval u)
  Erased{}      -> impossible
  Quote t       -> impossible
  Splice t pos  -> eRun $ cSplice (ceval t) pos
  Return t      -> pure $! ceval t
  Bind x t u    -> do {t <- exec t; def x t $ exec u}
  Seq t u       -> exec t >> exec u
  New t         -> CRef <$!> (coerce (newIORef $! ceval (coerce t)))
  Write t u     -> case ceval t of
                     CRef r -> CErased "tt" <$ (writeIORef (coerce r) $! ceval u)
                     _      -> impossible
  Read t        -> case ceval t of
                     CRef r -> readIORef (coerce r)
                     _      -> impossible
  NatLit n      -> impossible
  Suc t         -> impossible
  NatElim s z n -> eRun $ cNatElim (ceval s) (ceval z) (ceval n)
  Rec ts        -> impossible
  Proj t x      -> eRun (cProj (ceval t) x)
  CSP x t       -> eRun t
  Open xs t u   -> cOpen xs (ceval t) (exec u)

ceval :: CEnv => Tm -> Closed
ceval = \case
  Var x         -> snd (?env !! coerce x)
  Let x t u     -> def x (ceval t) (ceval u)
  Lam x t       -> CLam x (coerce (\v -> def x v $ ceval t))
                          (coerce (\l v -> closeEnv $ def x v $ lvl l $ stage 0 $ oeval t))
  App t u       -> cApp (ceval t) (ceval u)
  Erased s      -> CErased s
  Quote t       -> CQuote (closeEnv $ lvl 0 $ stage 1 $ oeval t)
  Splice t pos  -> cSplice (ceval t) pos
  t@Return{}    -> CAction (coerce (exec t))
  t@Bind{}      -> CAction (coerce (exec t))
  t@Seq{}       -> CAction (coerce (exec t))
  t@New{}       -> CAction (coerce (exec t))
  t@Write{}     -> CAction (coerce (exec t))
  t@Read{}      -> CAction (coerce (exec t))
  NatLit n      -> CNat n
  Suc t         -> cSuc (ceval t)
  NatElim s z n -> cNatElim (ceval s) (ceval z) (ceval n)
  Rec ts        -> CRec (fmap (fmap ceval) ts)
  Proj t x      -> cProj (ceval t) x
  CSP _ t       -> t
  Open xs t u   -> cOpen xs (ceval t) (ceval u)


oApp :: Lvl => Open -> Open -> Open
oApp t u = case (t, u) of
  (OClosed x (CLam _ f _), OClosed x' u) -> OClosed Nothing (coerce f u)
  (OClosed x (CLam _ _ f), u           ) -> coerce f ?lvl u
  (OLam _ f              , u           ) -> coerce f ?lvl u
  (t                     , u           ) -> OApp t u

oSplice :: Open -> Open
oSplice = \case
  OClosed _ (CQuote t) -> t
  OQuote t             -> t
  t                    -> OSplice t

oProj :: Open -> Name -> Open
oProj t x = case t of
  ORec ts -> fromJust $ lookup x ts
  t       -> OProj t x

oOpen :: [Name] -> Open -> (OEnv => a) -> OEnv => a
oOpen xs t u =
  let ?env = foldl' (\env x -> (x, oProj t x):env) ?env xs
  in seq ?env u

oeval :: OEnv => Lvl => Stage => Tm -> Open
oeval = \case
  Var x      -> snd (?env !! coerce x)
  Lam x t    -> OLam x (coerce \l v -> def x v $ lvl l $ oeval t)
  Erased s   -> OErased s
  Quote t    -> oQuote $ stage (?stage + 1) $ oeval t
  Return t   -> OReturn (oeval t)
  Bind x t u -> OBind x (oeval t) (NoShow \l -> lvl l $ oeval u)
  Seq t u    -> OSeq (oeval t) (oeval u)
  New t      -> ONew (oeval t)
  Write t u  -> OWrite (oeval t) (oeval u)
  Read t     -> ORead (oeval t)
  CSP x t    -> OClosed x t
  Suc t      -> OSuc (oeval t)
  Rec ts     -> ORec (fmap (fmap oeval) ts)
  NatLit n   -> ONatLit n
  t -> case ?stage of
    0 -> case t of
      Let x t u     -> def x (oeval t) (oeval u)
      Open xs t u   -> oOpen xs (oeval t) (oeval u)
      App t u       -> oApp (oeval t) (oeval u)
      Splice t loc  -> case oeval t of
                         OClosed _ (CQuote t) -> env (idEnv ?lvl) $ oeval $ traceGen t loc
                         OQuote t             -> env (idEnv ?lvl) $ oeval $ traceGen t loc
                         t                    -> OSplice t
      NatElim s z n -> oNatElim (oeval s) (oeval z) (oeval n)
      Proj t x      -> oProj (oeval t) x
    _ -> case t of
      Let x t u     -> OLet x (oeval t) (NoShow \l -> lvl l $ oeval u)
      Open xs t u   -> OOpen xs (oeval t) (NoShow \l -> lvl l $ oeval u)
      App t u       -> OApp (oeval t) (oeval u)
      Splice t pos  -> oSplice $ stage (?stage - 1) $ oeval t
      NatElim s z n -> ONatElim (oeval s) (oeval z) (oeval n)
      Proj t x      -> OProj (oeval t) x

traceGen :: Lvl => Open -> Maybe String -> Tm
traceGen t loc =
  let t' = gen t
      freevars = map (\l -> "x" ++ show l) [0.. ?lvl - 1]
      displayCode  = prettyTm 0 0 freevars (Z.unzonk t') ""
  in case loc of
    Nothing ->
      trace ("CODE GENERATED: \n\n" ++ displayCode ++ "\n") t'
    Just loc ->
      trace ("CODE GENERATED AT:\n" ++ loc ++ "\nCODE:\n  " ++ displayCode ++ "\n") t'

gen :: Lvl => Open -> Tm
gen = \case
  OVar x         -> Var (coerce (?lvl - x - 1))
  OLet x t u     -> Let x (gen t) $ fresh \_ -> gen (coerce u ?lvl)
  OLam x t       -> Lam x $ fresh \v -> gen (coerce t ?lvl v)
  OApp t u       -> App (gen t) (gen u)
  OErased s      -> Erased s
  OQuote t       -> Quote (gen t)
  OSplice t      -> Splice (gen t) Nothing
  OReturn t      -> Return (gen t)
  OBind x t u    -> Bind x (gen t) $ fresh \_ -> gen (coerce u ?lvl)
  OSeq t u       -> Seq (gen t) (gen u)
  ONew t         -> New (gen t)
  OWrite t u     -> Write (gen t) (gen u)
  ORead t        -> Read (gen t)
  ONatLit n      -> NatLit n
  OSuc t         -> Suc (gen t)
  ONatElim s z n -> NatElim (gen s) (gen z) (gen n)
  ORec ts        -> Rec (fmap (fmap gen) ts)
  OProj t x      -> Proj (gen t) x
  OClosed x t    -> CSP x t
  OOpen xs t u   -> Open xs (gen t) $ freshes xs $ gen (coerce u ?lvl)

-- Only for pretty printing purposes
readBackClosed :: Closed -> Tm
readBackClosed t = let ?lvl = 0 in case t of
  CLam x _ t -> Lam x $ fresh \v -> gen (coerce t ?lvl v)
  CAction _  -> Erased "Action"
  CRef _     -> Erased "Ref"
  CQuote t   -> Quote (gen t)
  CErased s  -> Erased s
  CRec ts    -> Rec (fmap (fmap readBackClosed) ts)
  CNat n     -> NatLit n

execTop :: Tm -> IO Closed
execTop t = env [] $ exec t
