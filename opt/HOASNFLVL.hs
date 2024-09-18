
{-# language Strict, LambdaCase, BlockArguments, ViewPatterns #-}
{-# options_ghc -Wincomplete-patterns #-}

type Ix  = Int
type Lvl = Int

data Tm
  = Var Lvl
  | App Tm Tm
  | Lam Tm
  | Let Tm Tm
  | Inl Tm
  | Inr Tm
  | Case Tm Tm Tm
  deriving Show

anf :: [Lvl] -> Tm -> Lvl -> (Lvl -> Lvl -> Tm) -> Tm
anf e t l k = case t of
  Var x      -> k l (e !! (length e - x - 1))
  App t u    -> anf e t l \t l -> anf e u l \u l ->
                Let (App (Var t) (Var u)) (k l (l + 1))
  Lam t      -> Let (Lam (anf (l:e) t (l+1) (const . Var))) (k l (l + 1))
  Let t u    -> anf e t l \t l -> anf (t:e) u l k
  Inl t      -> anf e t l \t l -> Let (Inl (Va t)) (k l (l + 1))
  Inr t      -> anf e t l \t l -> Let (Inr (Var t)) (k l (l + 1))
  Case t u v -> anf e t l \t l -> Case (Var t) (anf (l:e) u (l+1) k)
                                               (anf (l:e) v (l+1) k)

anf0 :: Tm -> Tm
anf0 t = anf [] t 0 (const . Var)
