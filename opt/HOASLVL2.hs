
{-# language Strict, LambdaCase, BlockArguments, ViewPatterns, ImplicitParams #-}
{-# options_ghc -Wincomplete-patterns #-}

type Ix  = Int
type Lvl = Int

-- ANF conversion where "semantic values" are simply Lvl-s
-- closures are not needed, because Lvl-s are stable under weakening!

data Tm
  = Var Lvl
  | App Tm Tm
  | Lam Tm
  | Let Tm Tm
  | Inl Tm
  | Inr Tm
  | Case Tm Tm Tm
  deriving Show

type Fresh  = (?fresh :: Lvl)
type Env    = (?env   :: [Lvl])
type Conv a = Fresh => Env => a

bind :: Conv (Lvl -> a) -> Conv a
bind act =
  let x = ?fresh     in
  let ?fresh = x + 1 in
  let ?env = x : ?env in
  act x

def :: Lvl -> Conv (Lvl -> a) -> Conv a
def x act = let ?env = x : ?env in act ?fresh

vlet :: Tm -> Conv (Lvl -> Tm) -> Conv Tm
vlet t u = Let t (bind u)

vlam :: Conv (Lvl -> Tm) -> Conv Tm
vlam t = Lam (bind t)

vcase :: Lvl -> Conv (Lvl -> Tm) -> Conv (Lvl -> Tm) -> Conv Tm
vcase t l r = Case (Var t) (bind l) (bind r)

vapp x y = App (Var x) (Var y)
vinl x   = Inl (Var x)
vinr x   = Inr (Var x)

anf :: Tm -> Conv (Lvl -> Tm) -> Conv Tm
anf t k = case t of
  Var x      -> k (?env !! (length ?env - x - 1))
  App t u    -> anf t \t -> anf u \u -> vlet (vapp t u) k
  Lam t      -> vlet (vlam \_ -> anf t Var) k
  Let t u    -> anf t \t -> def t \_ -> anf u k
  Inl t      -> anf t \t -> vlet (vinl t) k
  Inr t      -> anf t \t -> vlet (vinr t) k
  Case t l r -> anf t \t -> case vlam k of
                  Lam (Var _) -> vcase t (\_ -> anf l k) (\_ -> anf r k)
                  k           -> vlet k \k ->
                                 vcase t (\_ -> anf l (vapp k)) (\_ -> anf r (vapp k))

anf0 :: Tm -> Tm
anf0 t =
  let ?fresh = 0
      ?env   = [] in
  anf t Var

t = Lam $ Case (Var 0) (Inl $ Var 1) (Inr $ Var 1)
