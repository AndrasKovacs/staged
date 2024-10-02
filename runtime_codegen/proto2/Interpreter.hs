{-# options_ghc -Wno-unused-imports #-}

module Interpreter where

import Common hiding (Lvl)
import qualified Common as C
import qualified Zonk as Z

import Data.IORef

{-
IDEA:

1. Type erasure + zonking (in Zonk)

2. Reference interpretation
   - All objects are mashed together in the same data type, whether code or semantic value
   - That type will serve as the javascript model
   - all binders are HOAS; if we squint we can see staged HOAS interpretation
     as compilation to javascript
   - separate top and local env

Q
- should we have a single neutral node, or just do dumb Var/App/Splice?
- let's do the dumb thing

-}

data Spine = SId | SApp Spine Val | SSplice Spine

type Lvl   = (?lvl :: C.Lvl)
type Env   = (?env :: [Val])
type Top   = (?top :: [Val])
type Stage = (?stage :: Int)

def :: Val -> (Env => a) -> (Env => a)
def t act = let ?env = t: ?env in act

stage :: Int -> (Stage => a) -> a
stage s act = let ?stage = s in act

lvl :: C.Lvl -> (Lvl => a) -> a
lvl l act = let ?lvl = l in act

env :: [Val] -> (Env => a) -> a
env e act = let ?env = e in act

top :: [Val] -> (Top => a) -> a
top e act = let ?top = e in act

data Val
  -- canonicals
  = Lam Name (Val -> Val) (Lvl => Val -> Val) -- closed eval, open eval
  | Quote Val
  | Effect (IO Val) ~Val                      -- closed exec, open eval
  | Ref (IORef Val)

  -- code and neutrals
  | Var C.Lvl
  | TopVar C.Lvl
  | Let Name Val (Lvl => Val)
  | App Val Val
  | Erased
  | Splice Val
  | Return Val
  | Bind Name Val (Lvl => Val)
  | New Val
  | Write Val Val
  | Read Val

lookupLocal :: Env => Ix -> Val
lookupLocal x = ?env !! coerce x

lookupTop :: Top => C.Lvl -> Val
lookupTop x = ?top !! (length ?top - coerce x - 1)

cApp :: Val -> Val -> Val
cApp t u = case t of
  Lam x f g -> f u
  t         -> impossible

cRun :: Val -> IO Val
cRun = \case
  Effect f g -> f
  _          -> impossible

cSplice :: Top => Val -> Val
cSplice = \case
  Quote t -> env [] $ lvl 0 $ ceval $ gen t
  _       -> impossible

cRead :: Val -> IO Val
cRead = \case
  Ref r -> readIORef r
  _     -> impossible

cWrite :: Val -> Val -> IO Val
cWrite t u = case t of
  Ref r -> Erased <$ writeIORef r u
  _     -> impossible

-- pure closed evaluation
ceval :: Top => Env => Z.Tm -> Val
ceval = \case
  Z.Var x      -> lookupLocal x
  Z.TopVar x   -> lookupTop x
  Z.App t u    -> cApp (ceval t) (ceval u)
  Z.Let _ t u  -> def (ceval t) (ceval u)
  Z.Lam x t    -> Lam x (\v -> def v $ ceval t) (\v -> def v $ stage 0 $ oeval t)
  Z.Erased     -> Erased
  Z.Quote t    -> Quote (stage 1 $ lvl 0 $ oeval t)
  Z.Splice t   -> cSplice (ceval t)
  t@Z.Return{} -> Effect (exec t) (stage 0 $ lvl 0 $ oeval t)
  t@Z.Bind{}   -> Effect (exec t) (stage 0 $ lvl 0 $ oeval t)
  t@Z.New{}    -> Effect (exec t) (stage 0 $ lvl 0 $ oeval t)
  t@Z.Write{}  -> Effect (exec t) (stage 0 $ lvl 0 $ oeval t)
  t@Z.Read{}   -> Effect (exec t) (stage 0 $ lvl 0 $ oeval t)

-- pure open evaluation
oeval :: Top => Env => Lvl => Stage => Z.Tm -> Val
oeval = undefined

-- closed effectful evaluation
exec :: Top => Env => Z.Tm -> IO Val
exec = \case
  Z.Var x      -> cRun $ lookupLocal x
  Z.TopVar x   -> cRun $ lookupTop x
  Z.Let _ t u  -> def (ceval t) (exec u)
  Z.Return t   -> return $! ceval t
  Z.Bind _ t u -> do {t <- exec t; def t $ exec u}
  Z.App t u    -> cRun (cApp (ceval t) (ceval u))
  Z.New t      -> do {r <- newIORef $! ceval t; pure $ Ref r}
  Z.Read t     -> cRead (ceval t)
  Z.Write t u  -> cWrite (ceval t) (ceval u)
  Z.Splice t   -> cRun (cSplice (ceval t))
  _            -> impossible

fresh :: (Lvl => Val -> a) -> (Lvl => a)
fresh act = let v = Var ?lvl in let ?lvl = ?lvl + 1 in act v

gen :: Lvl => Val -> Z.Tm
gen = \case
  Lam x f g  -> Z.Lam x $ fresh \v -> gen (g v)
  Quote t    -> Z.Quote (gen t)
  Effect f g -> gen g
  Ref t      -> Z.Ref (gen t)

-- -- Eff closed evaluation
-- cexec :: Env -> Tm -> IO Val
-- cexec e = \case
--   Var x       -> cRun (lookupIx e x)
--   TopVar x    -> cRun (lookupIx e (lvl2Ix (envLen e) x))
--   Return t    -> pure $! ceval e t
--   Bind x t u  -> do {t <- cexec e t; cexec (Def e t) u}
--   Let x a t u -> cexec (Def e (ceval e t)) u
--   App t u _   -> cRun (cApp (ceval e t) (ceval e u))
--   New t       -> VRefVal <$!> (newIORef $! ceval e t)
--   Read t      -> cRead (ceval e t)
--   Write t u   -> cWrite (ceval e t) (ceval e u)
--   Splice t    -> cRun (cSplice (ceval e t))
--   _           -> impossible

-- oApp :: Lvl -> Val -> Val -> Icit -> Val
-- oApp l t u i = case t of
--   VLam _ _ _ t -> t l u
--   VNe x sp     -> VNe x (SApp sp u i)
--   _            -> impossible

-- oQuote :: Val -> Val
-- oQuote = \case
--   VNe x (SSplice sp) -> VNe x sp
--   t                  -> VQuote t

-- oSplice :: Val -> Val
-- oSplice = \case
--   VQuote t -> t
--   VNe x sp -> VNe x (SSplice sp)
--   _        -> impossible

-- oeval :: Env -> Lvl -> Stage -> Tm -> Val
-- oeval e l 0 = \case
--   Var x     -> lookupIx e x
--   TopVar x  -> lookupIx e (lvl2Ix (envLen e) x)

--   -- open lambdas can never become closed!
--   Lam x i t     -> VLam x i (\u -> impossible) (\l u -> oeval (Def e u) l 0 t)
--   App t u i     -> oApp l (oeval e l 0 t) (oeval e l 0 u) i
--   U             -> VU
--   Pi x i a b    -> VPi x i (oeval e l 0 a) \l u -> oeval (Def e u) l 0 b
--   Let x a t u   -> oeval (Def e (oeval e l 0 t)) l 0 u
--   Box t         -> VBox (oeval e l 0 t)
--   Quote t       -> oQuote (oeval e l 1 t)

--   Splice t      -> undefined

--   Unit          -> VUnit
--   Tt            -> VTt
--   Eff t         -> VEff (oeval e l 0 t)
--   Return t      -> VReturn (oeval e l 0 t)
--   Bind x t u    -> VBind x (oeval e l 0 t) \l t -> oeval (Def e t) l 0 u
--   ConstBind t u -> VConstBind (oeval e l 0 t) (oeval e l 0 u)
--   Ref t         -> VRef (oeval e l 0 t)
--   New t         -> VNew (oeval e l 0 t)
--   Write t u     -> VWrite (oeval e l 0 t) (oeval e l 0 u)
--   Read t        -> VRead (oeval e l 0 t)

--   Meta{}           -> impossible
--   AppPruning{}     -> impossible
--   PostponedCheck{} -> impossible

-- oeval e l s = \case
--   Var x         -> lookupIx e x
--   TopVar x      -> VTopVar x
--   Lam x i t     -> VLam x i (\u -> impossible) (\l u -> oeval (Def e u) l s t)
--   App t u i     -> VApp (oeval e l s t) (oeval e l s u) i
--   U             -> VU
--   Pi x i a b    -> VPi x i (oeval e l s a) \l u -> oeval (Def e u) l s b
--   Let x a t u   -> VLet x (oeval e l s a) (oeval e l s t) \l t -> oeval (Def e t) l s u
--   Box t         -> VBox (oeval e l s t)
--   Quote t       -> VQuote (oeval e l 1 t)
--   Splice t      -> oSplice (oeval e l s t)
--   Unit          -> VUnit
--   Tt            -> VTt
--   Eff t         -> VEff (oeval e l s t)
--   Return t      -> VReturn (oeval e l s t)
--   Bind x t u    -> VBind x (oeval e l s t) \l t -> oeval (Def e t) l s u
--   ConstBind t u -> VConstBind (oeval e l s t) (oeval e l s u)
--   Ref t         -> VRef (oeval e l s t)
--   New t         -> VNew (oeval e l s t)
--   Write t u     -> VWrite (oeval e l s t) (oeval e l s u)
--   Read t        -> VRead (oeval e l s t)

--   Meta{}           -> impossible
--   AppPruning{}     -> impossible
--   PostponedCheck{} -> impossible

{-

  r <- new 10;
  let foo = <x <- read r; return (x + 200)>;
  kek <- ~foo;
  ...


  let f : Nat -> Nat = \x. x;
  let foo = <f>;

  foo (r : Ref Int) : Code (Eff Int) =
    <read r>

  foo (n : Nat) : Code Nat = <n>

------------------------------------------------------------
  let x  = suc (suc (suc zero));
  let c = <zero + zero>;
  <x + ~c>

  result is <suc (suc (suc zero)) + (zero + zero)>



-}
