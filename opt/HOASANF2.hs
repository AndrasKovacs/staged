
{-# language Strict, LambdaCase, BlockArguments, ViewPatterns #-}
{-# options_ghc -Wincomplete-patterns #-}

type Ix  = Int
type Lvl = Int

data Tm
  = Var Ix
  | App Tm Tm
  | Lam Tm
  | Let Tm Tm
  | Inl Tm
  | Inr Tm
  | Case Tm Tm Tm
  deriving Show

data Val
  = VVar Lvl
  | VApp Lvl Lvl
  | VLam (Lvl -> Val)
  | VLet Val (Lvl -> Val)
  | VInl Lvl
  | VInr Lvl
  | VCase Lvl (Lvl -> Val) (Lvl -> Val)

quoteVar :: Lvl -> Lvl -> Tm
quoteVar l x = Var (l - x - 1)

quote :: Lvl -> Val -> Tm
quote l = \case
  VVar x      -> quoteVar l x
  VApp t u    -> App (quoteVar l t) (quoteVar l u)
  VLam t      -> Lam (quote (l + 1) (t l))
  VLet t u    -> Let (quote l t) (quote (l + 1) (u l))
  VInl t      -> Inl (quoteVar l t)
  VInr t      -> Inr (quoteVar l t)
  VCase t u v -> let l' = l + 1 in
                 Case (quoteVar l t) (quote l' (u l)) (quote l' (v l))

anf :: [Lvl] -> Tm -> (Lvl -> Val) -> Val
anf e t k = case t of
  Var x      -> k (e !! x)
  App t u    -> anf e t \t -> anf e u \u -> VLet (VApp t u) k
  Lam t      -> VLet (VLam \x -> anf (x:e) t VVar) k
  Let t u    -> anf e t \t -> anf (t:e) u k
  Inl t      -> anf e t \t -> VLet (VInl t) k
  Inr t      -> anf e t \t -> VLet (VInr t) k
  Case t l r -> anf e t \t -> VLet (VLam k) \join ->
                VCase t (\x -> anf (x:e) l (VApp join))
                        (\x -> anf (x:e) r (VApp join))

anf0 :: Tm -> Tm
anf0 t = quote 0 $ anf [] t VVar
