
{-# language Strict, LambdaCase, BlockArguments, OverloadedStrings #-}
{-# options_ghc -Wincomplete-patterns #-}

module Notes where

import Data.Maybe
import Data.String

type Name = String

data Tm
  = Var Name
  | App Tm Tm
  | Lam Name Tm
  | Let Name Tm Tm
  deriving Show

{-
We want to apply the following transformations everywhere

  let x = y in t   -- where y is a variable
  ~>
  t[y/x]

  let x = (let y = t in u) in v
  ~>
  let y = t in let x = u in v

  (let x = t in u) v
  ~>
  let x = t in u v

  (\x -> t) u
  ~>
  let x = u in t

This is possible purely with NbE in an efficient and very easy way, where we
don't have to manually handle variables and substitution. Instead, every binder
looks like a plain function.

nThe main limitation is that we can't look "under" a binder to check whether its
body is of a particular form. However, many things can be implemented without
looking under binders, and we can work around some limitations by including more
information in values and Val binders (which I don't demonstrate here).
-}

data Val
  = VVar Name
  | VApp Val Val
  | VLam Name (Val -> Val)
  | VLet Name Val (Val -> Val)

type Env = [(Name, Val)]

vapp :: Val -> Val -> Val
vapp t u = case t of
  VLam x t   -> vlet x u t
  VLet x t v -> vlet x t \x -> vapp (v x) u
  t          -> VApp t u

vlet :: Name -> Val -> (Val -> Val) -> Val
vlet x t u = case t of
  VVar _        -> u t
  VLet x' t' u' -> vlet x' t' \x' -> vlet x (u' x') u
  t             -> VLet x t u

eval :: Env -> Tm -> Val
eval e = \case
  Var x     -> fromJust $ lookup x e
  App t u   -> vapp (eval e t) (eval e u)
  Lam x t   -> VLam x \v -> eval ((x, v):e) t
  Let x t u -> vlet x (eval e t) \v -> eval ((x, v):e) u

fresh :: [Name] -> Name -> Name
fresh ns x | elem x ns = fresh ns (x ++ "'")
           | otherwise = x

quote :: [Name] -> Val -> Tm
quote ns = \case
  VVar x     -> Var x
  VApp t u   -> App (quote ns t) (quote ns u)
  VLam x t   -> let x' = fresh ns x in Lam x' (quote (x':ns) (t (VVar x')))
  VLet x t u -> let x' = fresh ns x in Let x' (quote ns t) (quote (x':ns) (u (VVar x')))

-- transform closed terms
transform :: Tm -> Tm
transform = quote [] . eval []

-- Examples
--------------------------------------------------------------------------------

instance IsString Tm where
  fromString = Var

($$) = App
infixl 2 $$

p1 = Let "zero" (Lam "s" $ Lam "z" $ "z") $
     (Lam "x" "x") $$ "zero" -- beta redex of varible (gets reduced)

p2 = Let "zero" (Lam "s" $ Lam "z" $ "z") $
     (Let "id" (Lam "x" "x") "id") $$ "zero"  -- push app inside let

p3 = (Lam "x" "x") $$ (Lam "s" $ Lam "z" $ "z") -- beta-redex of non-variable (turns into let)

p4 = Let "zero" (Let "id" (Lam "x" "x") (Lam "s" $ Lam "z" "z")) "zero" -- let-of-let
