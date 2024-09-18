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
  = VRet Lvl
  | VApp Lvl Lvl (Lvl -> Val)
  | VLam (Lvl -> Val) (Lvl -> Val)
  | VInl Lvl (Lvl -> Val)
  | VInr Lvl (Lvl -> Val)
  | VCase Lvl (Lvl -> Val) (Lvl -> Val)

data ANF
  = ARet Lvl
  | AInl Lvl ANF
  | AInr Lvl ANF
  | ALam ANF ANF
  | AApp Lvl Lvl ANF
  | ACase Lvl ANF ANF
  deriving Show

quoteLvl :: Lvl -> Lvl -> Lvl
quoteLvl l x = l - x - 1

quoteBind :: Lvl -> (Lvl -> Val) -> ANF
quoteBind l t = quote (l + 1) (t l)

quote :: Lvl -> Val -> ANF
quote l = \case
  VRet x      -> ARet (quoteLvl l x)
  VApp x y t  -> AApp (quoteLvl l x) (quoteLvl l y) (quoteBind l t)
  VLam t u    -> ALam (quoteBind l t) (quoteBind l u)
  VInl x t    -> AInl (quoteLvl l x) (quoteBind l t)
  VInr x t    -> AInr (quoteLvl l x) (quoteBind l t)
  VCase x t u -> ACase (quoteLvl l x) (quoteBind l t) (quoteBind l u)

anf :: [Lvl] -> Tm -> (Lvl -> Val) -> Val
anf e t k = case t of
  Var x      -> k (e !! x)
  App t u    -> anf e t \t -> anf e u \u -> VApp t u k
  Lam t      -> VLam (\x -> anf (x:e) t VRet) k
  Let t u    -> anf e t \t -> anf (t:e) u k
  Inl t      -> anf e t \t -> VInl t k
  Inr t      -> anf e t \t -> VInr t k
  Case t l r -> anf e t \t -> VLam k \join ->
                VCase t (\x -> anf (x:e) l \l -> VApp join l VRet)
                        (\x -> anf (x:e) r \r -> VApp join r VRet)

anf0 :: Tm -> ANF
anf0 t = quote 0 $ anf [] t VRet
