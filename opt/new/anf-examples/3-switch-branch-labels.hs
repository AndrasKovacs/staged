
{-# language Strict, LambdaCase, BlockArguments, ImplicitParams #-}
{-# options_ghc -Wincomplete-patterns #-}

module HOASLVL6 where

{-
This almost the same as the previous example file, expect that
switch branches are not ANF-es anymore, but instead calls to jump points.

This is a lot closer to the machine. For example, LLVM works very similarly,
with a label for each branch.

However, being close to the machine is not necessarily good if we want to
have an IR for middle end optimization.

If switches have ANF branches, then more information is *lexically* visible in
those branches. Take this example:

   let x = f y;
   case x of
     True -> case x of
       True  -> ...
       False -> ...
     False -> ...

We have a duplicate case on x. In this case, an optimizer can relatively easily
track that in each branch, the scrutinee variable is equal to the branch LHS. So
in the True branch, x is equal to True, therefore the second "case x of" can be
beta-reduced to its own True branch.

Consider the same example with mandatory branch labelling:

  let x = f y;
  join l1 =
    join l2 = ...
    join l3 = ...
    case x of
      True  -> jump l2
      False -> jump l3
  join l4 = ...
  case x of
    True  -> jump l1
    False -> jump l4

Now, if we look at the "case x" inside "l1", it is not apparent from the
enclosing lexical scope that "x" must be True. In other words, by factoring out
l1 into a join point, we lose the information that l1 is called from exactly one
site where x is True.
-}

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
  | ACall Lvl Lvl ANF    -- call a closure
  | ATailCall Lvl Lvl    -- tail call a closure
  | AInl Lvl ANF         -- allocate Inl
  | AInr Lvl ANF         -- allocate Inr
  | ASwitch Lvl Lvl Lvl  -- switch, jump to a join point in each branch
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
