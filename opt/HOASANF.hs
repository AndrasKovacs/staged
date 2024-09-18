{-# language Strict, LambdaCase, BlockArguments #-}
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
  | VApp Val Val
  | VLam (Val -> Val)
  | VLet Val (Val -> Val)
  | VInl Val
  | VInr Val
  | VCase Val (Val -> Val) (Val -> Val)

quote :: Lvl -> Val -> Tm
quote l = \case
  VVar x      -> Var (l - x - 1)
  VApp t u    -> App (quote l t) (quote l u)
  VLam t      -> Lam (quote (l + 1) (t (VVar l)))
  VLet t u    -> Let (quote l t) (quote (l + 1) (u (VVar l)))
  VInl t      -> Inl (quote l t)
  VInr t      -> Inr (quote l t)
  VCase t u v -> let l' = l + 1; x = VVar l in
                 Case (quote l t) (quote l' (u x)) (quote l' (v x))

anf :: [Val] -> Tm -> (Val -> Val) -> Val
anf e t k = case t of
  Var x      -> k (e !! x)
  App t u    -> anf e t \t -> anf e u \u -> VLet (VApp t u) k
  Lam t      -> VLet (VLam \x -> anf (x:e) t id) k
  Let t u    -> anf e t \t -> anf (t:e) u k
  Inl t      -> anf e t \t -> VLet (VInl t) k
  Inr t      -> anf e t \t -> VLet (VInr t) k
  Case t l r -> anf e t \t -> VLet (VLam k) \join ->
                VCase t (\x -> anf (x:e) l (VApp join))
                        (\x -> anf (x:e) r (VApp join))

anf0 :: Tm -> Tm
anf0 t = quote 0 $ anf [] t id
