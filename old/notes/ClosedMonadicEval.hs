{-# language OverloadedStrings #-}

import GHC.Stack
import Data.Maybe
import Data.String

type Name = String

data Tm
  = Var Name
  | App Tm Tm
  | Lam Name Tm
  | Let Name Tm Tm
  | Bind Name Tm Tm
  | Return Tm
  | Hello                  -- Hello : M ()
  | Tt                     -- Tt : Unit
  deriving (Eq, Show)

data Val
  = VVar Name
  | VApp Val Val
  | VLam Env Name Tm
  | VBind Env Name Tm Tm
  | VReturn Val
  | VHello
  | VTt
  deriving (Eq, Show)

type Env = [(Name, Val)]

--------------------------------------------------------------------------------

instance IsString Tm where fromString = Var
($$)  = App; infixl 7 $$
(>>>) = Bind "_"; infixl 1 >>>

--------------------------------------------------------------------------------

impossible :: HasCallStack => a
impossible = error "impossible"

-- Pure closed evaluation
eval :: Env -> Tm -> Val
eval e = \case
  Var x      -> fromJust $ lookup x e
  App t u    -> case eval e t of
                  VLam e' x t -> eval ((x, eval e u):e') t
                  _           -> impossible
  Lam x t    -> VLam e x t
  Let x t u  -> eval ((x, eval e t):e) u
  Bind x t u -> VBind e x t u
  Return t   -> VReturn (eval e t)
  Hello      -> VHello
  Tt         -> VTt

-- Perform 1 layer of IO
runIO :: Val -> IO Val
runIO = \case
  VReturn t     -> pure t
  VBind e x t u -> do {t <- runIO (eval e t); runIO (eval ((x, t):e) u)}
  VHello        -> VTt <$ putStrLn "hello"
  _             -> impossible

run :: Tm -> IO Val
run = runIO . eval []

--------------------------------------------------------------------------------

p1 :: Tm
p1 =
  Let "f" (Lam "m" $ Hello >>> "m") $
  "f" $$ Hello
