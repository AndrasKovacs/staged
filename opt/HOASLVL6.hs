
{-# language Strict, LambdaCase, BlockArguments, ImplicitParams #-}
{-# options_ghc -Wincomplete-patterns #-}

module HOASLVL6 where

-- ANF conversion with join points, tail calls & labeled switch branches

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
  = ARet Lvl             -- return
  | AJoin ANF ANF        -- declare join point
  | AJump Lvl Lvl        -- tailcall join point
  | ALam ANF ANF         -- allocate closure
  | AInl Lvl ANF         -- allocate Inl
  | AInr Lvl ANF         -- allocate Inr
  | ACall Lvl Lvl ANF    -- call a closure
  | ATailCall Lvl Lvl    -- tail call a closure
  | ASwitch Lvl Lvl Lvl  -- switch and jump to joint point
  deriving Show

------------------------------------------------------------

type Fresh  = (?fresh :: Lvl)
type Env    = (?env   :: [Lvl])
type Args a = Fresh => Env => a

def :: Lvl -> Args a -> Args a
def x act = let ?env = x : ?env in act

bind :: Args (Lvl -> a) -> Args a
bind act =
  let x      = ?fresh   in
  let ?fresh = x + 1    in
  let ?env   = x : ?env in
  act x

ret :: Lvl -> ANF
ret = ARet

join :: Args (Lvl -> ANF) -> Args (Lvl -> ANF) -> Args ANF
join t k = AJoin (bind t) (bind k)

jump :: Lvl -> Lvl -> ANF
jump = AJump

lam :: Args (Lvl -> ANF) -> Args (Lvl -> ANF) -> Args ANF
lam t k = ALam (bind t) (bind k)

inl :: Lvl -> Args (Lvl -> ANF) -> Args ANF
inl x k = AInl x (bind k)

inr :: Lvl -> Args (Lvl -> ANF) -> Args ANF
inr x k = AInr x (bind k)

call :: Lvl -> Lvl -> Args (Lvl -> ANF) -> Args ANF
call x y k = ACall x y (bind k)

tailcall :: Lvl -> Lvl -> ANF
tailcall = ATailCall

switch :: Lvl -> Lvl -> Lvl -> ANF
switch = ASwitch

------------------------------------------------------------

applyEnv :: Env => Lvl -> Lvl
applyEnv x = ?env !! (length ?env - x - 1)

anf :: Fresh => Env => Tm -> (Fresh => Lvl -> ANF) -> ANF
anf t k = case t of
  Var x      -> k $ applyEnv x
  App t u    -> anf u \u -> anf t \t -> call t u k
  Lam t      -> lam (\_ -> anfTail t) k
  Let t u    -> anf t \t -> def t $ anf u k
  Inl t      -> anf t \t -> inl t k
  Inr t      -> anf t \t -> inr t k
  Case t l r -> anf t \t -> join k \k ->
                            join (\_ -> anf l (jump k)) \l ->
                            join (\_ -> anf r (jump k)) \r ->
                            switch t l r

anfTail :: Fresh => Env => Tm -> ANF
anfTail = \case
  Var x      -> ret $ applyEnv x
  App t u    -> anf u \u -> anf t \t -> tailcall t u
  Lam t      -> lam (\_ -> anfTail t) ret
  Let t u    -> anf t \t -> def t $ anfTail u
  Inl t      -> anf t \t -> inl t ret
  Inr t      -> anf t \t -> inr t ret
  Case t l r -> anf t \t -> join (\_ -> anfTail l) \l ->
                            join (\_ -> anfTail r) \r ->
                            switch t l r

anf0 :: Tm -> ANF
anf0 t = let ?fresh = 0; ?env = [] in anfTail t

------------------------------------------------------------

t = Lam $ App (Var 0) (Case (Var 0) (Inl (Var 1)) (Inr (Var 1)))
