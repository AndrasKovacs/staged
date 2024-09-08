{-# language Strict, LambdaCase, BlockArguments, ImplicitParams, ConstraintKinds #-}
{-# options_ghc -Wincomplete-patterns #-}

module SimpleANF2 where

import Control.Monad.State.Strict
import Data.Foldable

{-
If the continuation k has no Case,
push the Case and eval the scrutinee

   eval (case t u v) k =
     eval t (Case * u v ; k)

If the continuation has a Case, split the it off and reify it.

   eval (case t u v) (k1 ; Case * u' v' ; k2) =
     let k x = ACase x (eval u' k2) (eval v' k2)
     eval t (Case * u v ; k1 ; k *)

We have at most one Case in the continuation, somewhere in the middle.

Continuations can be of the following form:

   - apps
   - apps, Case, apps
   - apps, Case, apps ; k *




-}

--------------------------------------------------------------------------------

type Ix = Int
type Lvl = Int
type Arity = Int

data Tm
  = Var Ix
  | LetV Tm Tm
  | LetC Arity Tm Tm
  | In1 Tm
  | In2 Tm
  | Case Tm Tm Tm
  | Lam Tm
  | App Tm Tm
  deriving Show

--------------------------------------------------------------------------------

data Comp = CLam Comp | CBody ANF
  deriving Show

data Let = LComp Comp | LApp Lvl [Lvl] | LIn1 Lvl | LIn2 Lvl
  deriving Show

data ANF = ARet Lvl | ACase Lvl ANF ANF | ALet Let ANF
  deriving Show

--------------------------------------------------------------------------------

data Cont
  = Apps      [Lvl]
  | AppsCase  [Lvl] ANF ANF [Lvl]
  | AppsCaseK [Lvl] ANF ANF [Lvl] Lvl
  deriving Show

type M = State (Lvl, [Let])

addLet :: Let -> M Lvl
addLet lt = do
  (l, lets) <- get
  put (l+1, lt:lets)
  pure l

locally :: M a -> M a
locally act = do
  (fresh, lets) <- get
  put (fresh, [])
  a <- act
  put (fresh, lets)
  pure a

evalVar :: [Lvl] -> Ix -> Cont -> M Lvl
evalVar env i = \case
  Apps [] -> pure (env !! i)
  Apps as -> addLet (LApp (env !! i) as)
  AppsCase as l r as' -> do
    x <- addLet (LApp (env !! i) as)
    undefined
  AppsCaseK as l r as' callk -> undefined

consApp :: Lvl -> Cont -> Cont
consApp l = \case
  Apps as                    -> Apps (l:as)
  AppsCase as t u as'        -> AppsCase (l:as) t u as'
  AppsCaseK as t u as' callk -> AppsCaseK (l:as) t u as' callk

unconsApp :: Cont -> (Lvl, Cont)
unconsApp = \case

evalComp :: [Lvl] -> Arity -> Tm -> Cont -> M Comp
evalComp = undefined

-- -- problem: eval can return a var or a case!!!
-- eval :: [Lvl] -> Tm -> Cont -> M Lvl
-- eval env t k = case t of
--   Var i -> evalVar env i k
--   LetV t u -> do
--     t <- eval env t (Apps [])
--     eval (t:env) u k
--   LetC arity t u -> do
--     t <- locally $ evalComp env arity t (Apps [])
--     t <- addLet (LComp t)
--     eval (t:env) u k
--   Lam t -> do
--     (l,k) <- pure $ unconsApp k
--     eval (l:env) t k
--   App t u -> do
--     u <- eval env u (Apps [])
--     eval env t (consApp u k)
--   In1 t -> do
--     t <- eval env t (Apps [])
--     addLet (LIn1 t)
--   In2 t -> do
--     t <- eval env t (Apps [])
--     addLet (LIn2 t)
--   Case t u v -> do
--     _


-- uhh: Case on the right kinda inverts control...

-- f (case x _ _)
-- let y = case x _ _
-- f y

-- f (case x l r)

-- case (case x l r) l' r'
-- case x l r     case l' r'
--

-- let k y = f y;
-- case x l;k r;k



-- case (f args) l r

-- let x = f args;
-- case x l r

-- f (case x l r)
-- case x
--  l -> .. f y
--  r -> .. f y

(case x ...) (case y ....)

case y
  l -> case ...
  r -> case ...

-- assume saturation
-- then a function cannot be a case
--  in    t u
--        t can only be an args spine!!
-- thus, in t u,
--      evaluating u, with (apply t) as continuation
--      is always safe for code size!

{-
if we have

A -> B       A

(case ..) (case ..)

that should be saturated

to (\x -> case ... l x, r x) (case ...)
   let x = case ...; case .. lx, rx

   let x = case _; case ... lx, rx


   f (case _) (case _)
   -->

   case _
     case _
       f _
       f _
     case _
       f _
       f _

-}


-- evalAndRet :: [Lvl] -> Tm -> Spine -> M ANF
-- evalAndRet env t stack = do
--   t <- eval env t stack
--   lets <- gets snd
--   pure $ foldl' (flip ALet) (ARet t) lets

-- evalComp :: [Lvl] -> Arity -> Tm -> Spine -> M Comp
-- evalComp env arity t stack = case arity of
--   0     -> CBody <$> evalAndRet env t stack
--   arity -> do (l, lets) <- get
--               put (l+1, lets)
--               CLam <$> evalComp (l:env) (arity-1) t (SApp stack l)

-- eval :: [Lvl] -> Tm -> Spine -> M Lvl
-- eval env t stack = case t of
--   Var i ->
--     case stack of Id -> pure (env !! i)
--                   _  -> addLet (LApp (env !! i) stack)
--   LetV t u -> do
--     t <- eval env t Id
--     eval (t:env) u stack
--   LetC arity t u -> do
--     t <- locally $ evalComp env arity t Id
--     t <- addLet (LComp t)
--     eval (t:env) u stack
--   Lam t ->
--     case stack of
--       Id           -> error "impossible"
--       SApp stack l -> eval (l:env) t stack
--   App t u -> do
--     u <- eval env u Id
--     eval env t (SApp stack u)
--   In1 t -> do
--     t <- eval env t Id
--     addLet (LIn1 t)
--   In2 t -> do
--     t <- eval env t Id
--     addLet (LIn2 t)
--   Case t u v -> do
--     t <- eval env t Id
--     _

-- -- case (case ...) _ _

-- -- The top level is a computation
-- evalTop :: Arity -> Tm -> Comp
-- evalTop arity t = evalState (evalComp [] arity t Id) (0, [])

-- --------------------------------------------------------------------------------

-- {-
-- example : Int -> Int =
--   \x.
--     let id : Int -> Int = (let id2 : Int -> Int = \y. y; \z. id2 z);
--     id x
-- ~>
-- example x =
--   let id2 y = y;
--   let id z = id2 z;
--   let a = id x;
--   a
-- -}

-- ($$) = App
-- infixl 8 $$

-- t :: Tm
-- t = Lam $
--   LetC 1 (LetC 1 (Lam $ Var 0) $ Lam $ Var 1 $$ Var 0) $
--   Var 0 $$ Var 1

-- -- out:

-- -- CLam (CBody (ALet (Comp (CLam (CBody (ALet (Comp (CLam (CBody (ARet 2)))) (ALet (Val 2 (SApp Id 1))
-- -- (ARet 3)))))) (ALet (Val 1 (SApp Id 0)) (ARet 2))))

-- {-
-- \x.
--   let id = \y.
--         let id2 = \z. z;
--         let foo = id2 y;
--         foo;
--   let res = id x;
--   res;
-- -}

-- --------------------------------------------------------------------------------
