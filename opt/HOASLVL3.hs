
{-# language Strict, LambdaCase, BlockArguments, ViewPatterns, ImplicitParams #-}
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

data ANF
  = ARet Lvl
  | AInl Lvl ANF
  | AInr Lvl ANF
  | ALam ANF ANF
  | AApp Lvl Lvl ANF
  | ACase Lvl ANF ANF
  deriving Show

------------------------------------------------------------

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

ainl :: Lvl -> Conv (Lvl -> ANF) -> Conv ANF
ainl x k = AInl x (bind k)

ainr :: Lvl -> Conv (Lvl -> ANF) -> Conv ANF
ainr x k = AInr x (bind k)

alam :: Conv (Lvl -> ANF) -> Conv (Lvl -> ANF) -> Conv ANF
alam t k = ALam (bind t) (bind k)

aapp :: Lvl -> Lvl -> Conv (Lvl -> ANF) -> Conv ANF
aapp x y k = AApp x y (bind k)

acase :: Lvl -> Conv (Lvl -> ANF) -> Conv (Lvl -> ANF) -> Conv ANF
acase x l r = ACase x (bind l) (bind r)

------------------------------------------------------------

anf :: Tm -> Conv (Lvl -> ANF) -> Conv ANF
anf t k = case t of
  Var x      -> k (?env !! (length ?env - x - 1))
  App t u    -> anf t \t -> anf u \u -> aapp t u k
  Lam t      -> alam (\_ -> anf t ARet) k
  Let t u    -> anf t \t -> def t \_ -> anf u k
  Inl t      -> anf t \t -> ainl t k
  Inr t      -> anf t \t -> ainr t k
  Case t l r -> anf t \t -> alam k \k ->
                let join x = aapp k x ARet in
                acase t (\_ -> anf l join) (\_ -> anf r join)
  -- Case t l r -> anf t \t -> case bind k of
  --                 ARet _ -> acase t (\_ -> anf l k) (\_ -> anf r k) -- "backtracking" is quite bad!!
  --                 bk     -> ALam bk $ bind \k ->
  --                           let join x = aapp k x ARet in
  --                           acase t (\_ -> anf l join) (\_ -> anf r join)

anf0 :: Tm -> ANF
anf0 t = let ?fresh = 0; ?env = [] in anf t ARet

-- t =
--   Let (Lam $ App (Case (Var 0) (Inl (Var 1)) (Inr (Var 1))) (Lam $ App (Var 1) (Var 1))) $
--   Var 0

-- t = Lam $ (Case (Var 0) (Inr (Var 1)) (Inl (Var 1)))
  -- App (Var 0) (Case (Var 0) (Inl (Var 1)) (Inr (Var 1)))
