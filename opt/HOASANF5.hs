
{-# language Strict, LambdaCase, BlockArguments, FunctionalDependencies, ImplicitParams, UndecidableInstances #-}
{-# options_ghc -Wincomplete-patterns -funbox-strict-fields #-}

type Ix    = Int
type Lvl   = Int
type Arity = Int

data Tm
  = Var Ix
  | App Tm Tm
  | Lam Tm
  | LetV Tm Tm
  | LetC Arity Tm Tm
  | Inl Tm
  | Inr Tm
  | Case Tm Tm Tm
  deriving Show

data SAtom
  = SApply Lvl [Lvl]
  | SInl Lvl
  | SInr Lvl
  deriving Show

data SComp
  = SLam (Lvl -> SComp)
  | SBody SANF

data SANF
  = SLetV SAtom (Lvl -> SANF)
  | SLetC SComp (Lvl -> SANF)
  | SRet Lvl
  | SCase Lvl (Lvl -> SANF) (Lvl -> SANF)

data AComp
  = ALam AComp
  | ABody ANF
  deriving Show

data AAtom
  = AApply Ix [Ix]
  | AInl Ix
  | AInr Ix
  deriving Show

data ANF
  = ALetV AAtom ANF
  | ALetC AComp ANF
  | ARetAtom AAtom
  | ARet Ix
  | ACase Ix ANF ANF
  deriving Show

--------------------------------------------------------------------------------

anfComp :: [Lvl] -> Arity -> Tm -> [Lvl] -> SComp
anfComp e ar t args = case ar of
  0  -> SBody (anf e t (reverse args) SRet)
  ar -> SLam \v -> anfComp (v:e) (ar - 1) t (v:args)

anf :: [Lvl] -> Tm -> [Lvl] -> (Lvl -> SANF) -> SANF
anf e t args k = case t of
  Var x       -> case args of [] -> k (e !! x)
                              _  -> SLetV (SApply (e !! x) args) k
  App t u     -> anf e u [] \u -> anf e t (u:args) k
  Lam t       -> case args of []     -> error "impossible"
                              v:args -> anf (v:e) t args k
  LetV t u    -> anf e t [] \t -> anf (t:e) u args k
  LetC ar t u -> SLetC (anfComp e ar t []) \t -> anf (t:e) u args k
  Inl t       -> anf e t [] \t -> SLetV (SInl t) k
  Inr t       -> anf e t [] \t -> SLetV (SInr t) k
  Case t l r  -> anf e t [] \t ->
                 SLetC (SLam \x -> SBody (k x)) \k ->
                 let join x = SLetV (SApply k [x]) SRet in
                 SCase t (\x -> anf (x:e) l args join) (\x -> anf (x:e) r args join)

--------------------------------------------------------------------------------

class Quote a b | a -> b where
  quote :: (?lvl :: Lvl) => a -> b

instance Quote a b => Quote (Lvl -> a) b where
  quote k = let t = k ?lvl in let ?lvl = ?lvl + 1 in quote t

instance Quote a b => Quote [a] [b] where
  quote = map quote

instance Quote SAtom AAtom where
  quote = \case
    SApply x args -> AApply (quote x) (quote args)
    SInl x        -> AInl (quote x)
    SInr x        -> AInr (quote x)

instance Quote SComp AComp where
  quote = \case
    SLam k  -> ALam (quote k)
    SBody t -> ABody (quote t)

instance Quote Lvl Ix where
  quote l = ?lvl - l - 1

instance Quote SANF ANF where
  quote = \case
    SLetV t k   -> ALetV (quote t) (quote k)
    SLetC t k   -> ALetC (quote t) (quote k)
    SRet x      -> ARet (quote x)
    SCase x l r -> ACase (quote x) (quote l) (quote r)

--------------------------------------------------------------------------------

anf0 :: Arity -> Tm -> AComp
anf0 ar t = let ?lvl = 0 in quote $ anfComp [] ar t []
