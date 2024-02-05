
### Staged effect handlers

We work in a two-level type theory where the object theory only has first-order
functions and simple types.

    U1  : U1
	VTy : U1             -- value types
	Ty  : U1             -- object types, indlucing functions
	VTy ≤ Ty             -- cumulative subtyping
	↑   : Ty → U1
	<_> : A → ↑A
	~_  : ↑A → A
	_→_ : VTy → Ty → Ty

- Object-level data can only store values (s.t. their type is in `VTy`).
- We can eliminate from object-level data to `Ty`.

Without closures, the usual object-level free monad is not very
useful:

    data Free (F : VTy → VTy) (A : VTy) : VTy where
	  pure : A → Free F A
	  free : F (Free F A) → Free F A

The problem is that `State` and `Reader` can't be expressed because we can't
store functions in commands.

class Monad1 (M : U1 → U1) where
  pure  : A → M A
  (>>=) : M A → (A → M B) → M B

class RelMonad (M : VTy → U1) where
  pure   : A → M A
  (>>=)  : M A → (A → M B) → M B
  (>>=') : M A → (A → M B) → M B

Gen : U1 → U1
Gen A = {R : Ty} → (A → R) → R

glet : {A : Ty}{B : U1} → A → (A → Gen B) → Gen B
glet a f {R} con = let x := a; f x con;

runGen : {A : Ty} → Gen A → A
runGen f = f id

instance Monad1 Gen where
  pure a con = con a
  (>>=) ma f con = ma (λ a. f a con)

State : VTy → VTy → U1
State S A = S → Gen (A, S)

get : State S S
get s = pure (s, s)

put : S → State S ()
put s _ = pure (s, ())

instance RelMonad (State S) where
  pure : A → State S A
  pure a s = pure (a, s)

  -- Fully inlined variant of binding.
  (>>=) : State S A → (A → State S B) → State S B
  (>>=) f g s = do
    (a, s) <- f s
    g a s

  -- Non-inline, sequenced binding.
  (>>=') : State S A → (A → State S B) → State S B
  (>>=') f g s = do
    (a, s) <- f s
    glet a λ a.
    glet s λ s.
    g a s

State0 : VTy → VTy → Ty
State0 s a = s → (a, s)

up : State0 S A → State S A
up f s = pure (fst (f s), snd (f s))

down : State S A → State0 S A
down f s = case runGen (f s) of (a, s) → (a, s)
