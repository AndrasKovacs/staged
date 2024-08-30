{-# language Strict, LambdaCase, BlockArguments, ImplicitParams, ConstraintKinds #-}
{-# options_ghc -Wincomplete-patterns #-}

module SimpleANF where

import Control.Monad.State.Strict
import Data.Foldable

--------------------------------------------------------------------------------

type Ix = Int
type Lvl = Int
type Arity = Int

data Tm
  = Var Ix
  | LetV Tm Tm
  | LetC Arity Tm Tm
  | Lam Tm
  | App Tm Tm
  deriving Show

--------------------------------------------------------------------------------

data Spine = Id | SApp Spine Lvl
  deriving Show

data Comp = CLam Comp | CBody ANF
  deriving Show

data Let = Comp Comp | Val Lvl Spine
  deriving Show

data ANF = ARet Lvl | ALet Let ANF
  deriving Show

--------------------------------------------------------------------------------

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

evalAndRet :: [Lvl] -> Tm -> Spine -> M ANF
evalAndRet env t stack = do
  t <- eval env t stack
  lets <- gets snd
  pure $ foldl' (flip ALet) (ARet t) lets

evalComp :: [Lvl] -> Arity -> Tm -> Spine -> M Comp
evalComp env arity t stack = case arity of
  0     -> CBody <$> evalAndRet env t stack
  arity -> do (l, lets) <- get
              put (l+1, lets)
              CLam <$> evalComp (l:env) (arity-1) t (SApp stack l)

eval :: [Lvl] -> Tm -> Spine -> M Lvl
eval env t stack = case t of
  Var i ->
    case stack of Id -> pure (env !! i)
                  _  -> addLet (Val (env !! i) stack)
  LetV t u -> do
    t <- eval env t Id
    eval (t:env) u stack
  LetC arity t u -> do
    t <- locally $ evalComp env arity t Id
    t <- addLet (Comp t)
    eval (t:env) u stack
  Lam t ->
    case stack of
      Id           -> error "impossible"
      SApp stack l -> eval (l:env) t stack
  App t u -> do
    u <- eval env u Id
    eval env t (SApp stack u)

-- The top level is a computation
evalTop :: Arity -> Tm -> Comp
evalTop arity t = evalState (evalComp [] arity t Id) (0, [])

--------------------------------------------------------------------------------

{-
example : Int -> Int =
  \x.
    let id : Int -> Int = (let id2 : Int -> Int = \y. y; \z. id2 z);
    id x
~>
example x =
  let id2 y = y;
  let id z = id2 z;
  let a = id x;
  a
-}

($$) = App
infixl 8 $$

t :: Tm
t = Lam $
  LetC 1 (LetC 1 (Lam $ Var 0) $ Lam $ Var 1 $$ Var 0) $
  Var 0 $$ Var 1

-- out:

-- CLam (CBody (ALet (Comp (CLam (CBody (ALet (Comp (CLam (CBody (ARet 2)))) (ALet (Val 2 (SApp Id 1))
-- (ARet 3)))))) (ALet (Val 1 (SApp Id 0)) (ARet 2))))

{-
\x.
  let id = \y.
        let id2 = \z. z;
        let foo = id2 y;
        foo;
  let res = id x;
  res;
-}



--------------------------------------------------------------------------------
