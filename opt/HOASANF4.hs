{-# language Strict, LambdaCase, BlockArguments #-}
{-# options_ghc -Wincomplete-patterns #-}

-- Defunctionalized ANF conversion (hard to read...)

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
  | VApp Lvl Lvl Cont
  | VLam Cont Cont
  | VInl Lvl Cont
  | VInr Lvl Cont
  | VCase Lvl Cont Cont

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

quoteCont :: Lvl -> Cont -> ANF
quoteCont l k = quote (l + 1) (apply k l)

quote :: Lvl -> Val -> ANF
quote l = \case
  VRet x      -> ARet (quoteLvl l x)
  VApp x y t  -> AApp (quoteLvl l x) (quoteLvl l y) (quoteCont l t)
  VLam t u    -> ALam (quoteCont l t) (quoteCont l u)
  VInl x t    -> AInl (quoteLvl l x) (quoteCont l t)
  VInr x t    -> AInr (quoteLvl l x) (quoteCont l t)
  VCase x t u -> ACase (quoteLvl l x) (quoteCont l t) (quoteCont l u)

data Cont
  = CApp1 [Lvl] Tm Cont
  | CApp2 Lvl Cont
  | CRet
  | CLet [Lvl] Tm Cont
  | CLam [Lvl] Tm
  | CInl Cont
  | CInr Cont
  | CCase1 [Lvl] Cont Tm Tm
  | CCase2 [Lvl] Lvl Tm Tm
  | CCaseLeft [Lvl] Lvl Tm
  | CCaseRight [Lvl] Lvl Tm
  | CCaseNoJoin [Lvl] Cont Tm Tm
  deriving Show

apply :: Cont -> Lvl -> Val
apply c arg = case c of
  CApp1 e u k            -> anf e u (CApp2 arg k)
  CApp2 t k              -> VApp t arg k
  CRet                   -> VRet arg
  CLet e u k             -> anf (arg:e) u k
  CLam e t               -> anf (arg:e) t CRet
  CInl k                 -> VInl arg k
  CInr k                 -> VInr arg k
  CCase1 e k l r         -> VLam k (CCase2 e arg l r)
  CCase2 e t l r         -> VCase t (CCaseLeft e arg l) (CCaseRight e arg r)
  CCaseLeft e join l     -> anf (arg:e) l (CApp2 join CRet)
  CCaseRight e join r    -> anf (arg:e) r (CApp2 join CRet)
  CCaseNoJoin e k l r    -> VCase arg (CLet e l k) (CLet e r k)

anf :: [Lvl] -> Tm -> Cont -> Val
anf e t k = case t of
  Var x      -> apply k (e !! x)
  App t u    -> anf e t (CApp1 e u k)
  Lam t      -> VLam (CLam e t) k
  Let t u    -> anf e t (CLet e u k)
  Inl t      -> anf e t (CInl k)
  Inr t      -> anf e t (CInr k)

  -- the extra magic: only introduce a join point if the continuation is big
  Case t l r -> case k of
    k@CRet           -> anf e t (CCaseNoJoin e k l r)
    k@(CApp2 _ CRet) -> anf e t (CCaseNoJoin e k l r)
    k                -> anf e t (CCase1 e k l r)

anf0 :: Tm -> ANF
anf0 t = quote 0 $ anf [] t CRet
