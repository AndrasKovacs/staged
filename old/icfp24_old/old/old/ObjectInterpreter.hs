{-# language Strict, LambdaCase #-}
{-# options_ghc -Wincomplete-patterns #-}

module ObjectInterpreter where
import Data.Maybe
import GHC.Stack

data F = K ValTy | Id | ProdF F F | SumF F F
  deriving Show

data ValTy = One | Mu F | Prod ValTy ValTy | Sum ValTy ValTy
  deriving Show

data Ty = Fun ValTy Ty | Val ValTy
  deriving Show

type Name = String

data Tm
  = Var Name
  | Let Name Ty Tm Tm
  | Fix Name Tm            -- fix only for functions!
  | Lam Name Tm
  | App Tm Tm
  | Pair Tm Tm
  | Fst Tm
  | Snd Tm
  | Tt
  | Inl Tm
  | Inr Tm
  | Case Tm Name Tm Name Tm
  | In Tm
  | Out Tm
  deriving Show

--------------------------------------------------------------------------------

data Val = VTt | VPair Val Val | VInl Val | VInr Val | VIn Val
  deriving Show

type ValEnv   = [(Name, Val)]
type FunSig   = [(Name, Tm)]
type ArgStack = [Val]

impossible :: HasCallStack => a
impossible = error "impossible"

lookupVar :: HasCallStack => Name -> [(Name, a)] -> a
lookupVar x kvs = case lookup x kvs of
  Nothing -> impossible
  Just a  -> a

eval :: ValEnv -> FunSig -> Tm -> ArgStack -> Val
eval vs fs t args = case t of
  Var x -> case args of
    [] -> lookupVar x vs
    _  -> eval vs fs (lookupVar x fs) args
  Let x a t u -> case a of
    Val _ -> let v = eval vs fs t [] in eval ((x,v):vs) fs u args
    _     -> eval vs ((x,t):fs) u args
  Fix x t -> eval vs ((x, Fix x t):fs) t args
  Lam x t -> case args of
    v:args -> eval ((x,v):vs) fs t args
    _      -> impossible
  App t u -> let v = eval vs fs u [] in eval vs fs t (v:args)
  Pair t u -> VPair (eval vs fs t []) (eval vs fs u [])
  Fst t -> case eval vs fs t [] of
    VPair t _ -> t
    _         -> impossible
  Snd t -> case eval vs fs t [] of
    VPair _ u -> u
    _         -> impossible
  Tt -> VTt
  Inl t -> VInl (eval vs fs t [])
  Inr t -> VInr (eval vs fs t [])
  Case t x u y v -> case eval vs fs t [] of
    VInl t -> eval ((x,t):vs) fs u args
    VInr t -> eval ((y,t):vs) fs v args
    _      -> impossible
  In t -> VIn (eval vs fs t [])
  Out t -> case eval vs fs t [] of
    VIn t -> t
    _     -> impossible
