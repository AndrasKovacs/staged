
{-# language Strict, LambdaCase, BlockArguments, ImplicitParams #-}
{-# options_ghc -Wincomplete-patterns #-}

{-
This is a significantly more complete example for ANF conversion.

  - The output language is a properly enforced ANF
  - We avoid code size blowup.
  - We do very basic optimization of code output by tracking *tail contexts*.
    This means that we specialize ANF-conversion for the case when the
    continuation is trivial (i.e. just returns a variable).

Moreover, we use a good amount of Haskell magic to tidy up the implementation
and to be a bit more defensive.
-}

------------------------------------------------------------

type Lvl = Int -- De Bruijn level

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
  = ARet Lvl             -- return a var
  | AJoin ANF ANF        -- let-bind a join point
  | AJump Lvl Lvl        -- call a join point with arg
  | ALam ANF ANF         -- let-bind a closure
  | ACall Lvl Lvl ANF    -- let-bind the result of a closure application
  | ATailCall Lvl Lvl    -- tail call a closure
  | AInl Lvl ANF         -- let-bind an Inl
  | AInr Lvl ANF         -- let-bind an Inr
  | ASwitch Lvl ANF ANF  -- case switch
  deriving Show

------------------------------------------------------------

{-
The weird stuff with "?" below is the ImplicitParams feature. You can look at
the docs:

   https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/implicit_parameters.html

It is a rarely used feature, but I have found it to be extremely useful. In some
of my use cases, it was so important that I was basically locked to Haskell
because of it, because my code would have been unacceptably bloated in a
different language.

I use ImplicitParams to make the threading of fresh variable counters and
environments implicit.

This would be possible with the use of Reader monads too, but ImplicitParams is
often more flexible, because it automatically picks out the set of arguments
that's needed, without having to convert between different Reader-s.

It is also nice that we do not have to use monadic syntax.
-}


type Fresh  = (?fresh :: Lvl)
type Env    = (?env   :: [Lvl])
type Args a = Fresh => Env => a

-- When I write "define x act", that means that I do "act" in
-- an extended variable environment, where the next var is mapped to "x".
define :: Lvl -> (Env => a) -> (Env => a)
define x act = let ?env = x : ?env in act

-- Locally grab a fresh variable.
bind :: Args (Lvl -> a) -> Args a
bind act =
  let x = ?fresh     in
  let ?fresh = x + 1 in
  let ?env = x : ?env in
  act x

-- Smart constructors which handle binding in arguments
------------------------------------------------------------

ret :: Lvl -> ANF -- this is not smart, I just include it to make my code look nicer
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

switch :: Lvl -> Args (Lvl -> ANF) -> Args (Lvl -> ANF) -> Args ANF
switch x l r = ASwitch x (bind l) (bind r)

------------------------------------------------------------

{-
Notice that I have two functions, with anfTail being an extra. I use anfTail
when I know that the continuation is trivial, i.e. "ret".  This lets me choose
between ordinary "call" and "tailcall" for functions, and also lets me skip a
superfluous join point for "case".

Distinguishing "call" and "tailcall" is not always a good thing to do. If we
don't plan to further optimize the ANF IR, it's usually useful, because tail and
non-tail calls are treated differently in backends. But if we plan to optimize
the ANF, the tail calls are often destroyed (or created) by optimization, so
it's probably better to not bother tracking them, and only detect tail calls
when we translate to a lower-level representation.
-}

applyEnv :: Env => Lvl -> Lvl
applyEnv x = ?env !! (length ?env - x - 1)

anf :: Fresh => Env => Tm -> (Fresh => Lvl -> ANF) -> ANF
anf t k = case t of
  Var x      -> k (applyEnv x)
  App t u    -> anf u \u -> anf t \t -> call t u k
  Lam t      -> lam (\_ -> anfTail t) k
  Let t u    -> anf t \t -> define t $ anf u k
  Inl t      -> anf t \t -> inl t k
  Inr t      -> anf t \t -> inr t k
  Case t l r -> anf t \t -> join k \k -> switch t (\_ -> anf l (jump k)) (\_ -> anf r (jump k))

anfTail :: Fresh => Env => Tm -> ANF
anfTail = \case
  Var x      -> ret (applyEnv x)
  App t u    -> anf u \u -> anf t \t -> tailcall t u
  Lam t      -> lam (\_ -> anfTail t) ret
  Let t u    -> anf t \t -> define t $ anfTail u
  Inl t      -> anf t \t -> inl t ret
  Inr t      -> anf t \t -> inr t ret
  Case t l r -> anf t \t -> switch t (\_ -> anfTail l) (\_ -> anfTail r)

anf0 :: Tm -> ANF
anf0 t = let ?fresh = 0; ?env = [] in anfTail t

------------------------------------------------------------

t = Lam $ App (Var 0) (Case (Var 0) (Inl (Var 1)) (Inr (Var 1)))
