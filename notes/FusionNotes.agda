{-# OPTIONS --type-in-type #-}

module FusionNotes where

{-
Idea: doing computation at compile time instead of at runtime

VTy : MTy   -- value types       (inductive data)
CTy : MTy   -- computation types (functions)
MTy : MTy   -- meta types        (meta TT types)

VTy ≤ CTy   (implicit cumulativity)

^_  : VTy → MTy        -- lifting
^_  : CTy → MTy
<_> : A → ^A           -- quote
~_  : ^A → A           -- splice

Nat  : VTy
^Nat  (has no elimination rule)

Solution: Church-encodings

- initial encoding:

  ^Nat ~ ((N : CTy) → (^N → ^N) → ^N → ^N)      βη

   List : VTy → VTy

  ^(List A) ~ ((L : CTy) → (^A → ^L → ^L) → ^L → ^L)
            ~ ((L : CTy) → (A → L → L) → L → L)

  A     : algebraic/inductive signature      e.g. (L : Set, cons : A → L → L, nil : L)
  A-Alg : MTy                                e.g. (List A)-Alg = (L : CTy) × (cons : A → L → L) × (nil : L)
  U : A-Alg → CTy  (underlying set functor)  e.g. U (L, cons, nil) = L


  - initial:  IChurch : Sig → MTy
              IChurch A = (α : A-Alg) → U α         IChurch ListSig = (α : ((L : CTy) × (cons : A → L → L) × (nil : L))) → U α
                                                                    = ((L : CTy) → (A → L → L) → L → L)

  - terminal: TChurch : CoSig → MTy
              TChurch A = (α : A-Alg) × U α

-- data List a = Nil | Cons a (List a)         ListSig   = (L : Set, cons : A → L → L, nil : L)
-- codata List a = Nil | Cons a (List a)       CoListSig = (L : Set, destruct : L → Maybe (A, L))

-- (more elegant: F-algebras, F-coalgebras)

CMaybe : MTy → MTy
CMaybe A = (M : MTy) → (A → M) → M → M                    -- Church Maybe

CoList : VTy → MTy
CoList A = (CL : VTy) × (next: CL → Maybe (A × CL)) × CL
         = (CL : VTy) × (next: CL → CMaybe (A × CL)) × CL

-- high-level: we want to optimize modulo positive η-rules
--  Bool : nf modulo η: exponential time
--  Boolᴺ → Bool

-- Compiler knows about *some* η-rules
--   e.g. GHC case-commutation

       runtime               meta             runtime
-- positive types  -->   IChurch rep   -->   residualize       (CPS transformations)
                   -->   TChurch rep   -->   residualize       (state machine transformation)


-- Initial Church encoding:
  + It works on all inductive types
  + It's relatively easy to efficiently compile
  - Only folding functions   (zip doesn't work for lists)

  "push" iterator, "internal" iterator

-- Terminal encoding:
  + More general for streams/dataflow stuff  (zip works fine)
  - In many cases doesn't make practical sense
    - For finite types it's just the same as initial encoding, but more complicated to use
  - Compiling state machines is pretty complicated

        (State : VTy) × (step : State → .....) × (seed : State)

                  step : (A + B + C + D + E) → (A + B + C + D + E + ⊤)

             --> turn it to tail-recursive loop
             -->   we have to turn lots of _+_ case splits into a *mutual* set of functions

             mutual
              f : A → B
              g : A → C

             fg : A + A → B + C
             fg (inj₁ a) = ... call either f or g recursively
             fg (inj₂ a) = ... call either f or g recursively

             -- job: do this transformation in reverse (not easy)

  "pull" stream, "external" iterator

-- Example:
   Rust iterator:
      - terminal encoding
      - *not* abstract (does not use sigma types) (internal state leaks to the outside)
          -- map : (A → B) → Stream S A → Stream S B
          --   easy to extend with typeclass magic

What I want:

  - constraints: only simple type theory at runtime, no closures

-}




--------------------------------------------------------------------------------



-- Comp/Val/Meta, 2LVLTT setting
-- fold fusion, stream fusion comparison
-- (external/internal, push/pull, initial/terminal, abstract/transparent)
-- closure-freedom
-- monadic bind
-- sigma-freedom
-- call-pattern spec
-- stream : skip-stream or CoList?
-- hybrid: affine stream + push lists + asymmetric zip
-- Robinson & Lippmeier : Machine Fusion (with split optimization!)
--
-- What are fusing functions?
--   - Finite positive types can be mechanically CPS'd into fusing defs
--               (but it is a lot weaker than full positive η!)
--   - Inifite types can't
--   - Auto-fusion for infinite types = supercompilation

--------------------------------------------------------------------------------

open import Data.List using (_∷_; [])
import Data.List as L

open import Data.Nat
open import Data.Product
  renaming (proj₁ to ₁; proj₂ to ₂) hiding (map; zip)
open import Function
import Data.Bool as B

--------------------------------------------------------------------------------

infix 3 _==_
_==_ : ℕ → ℕ → B.Bool
zero  == zero  = B.true
zero  == suc y = B.false
suc x == zero  = B.false
suc x == suc y = x == y

caseℕ : ∀ {A : Set} → ℕ → A → (ℕ → A) → A
caseℕ zero    z s = z
caseℕ (suc n) z s = s n

-- map (_+_ 3) ∘ countDown

-- λ x L c n →
--   fix
--   (λ go n₁ →
--      caseℕ n₁ (c 3 n) (λ n₂ → c (suc (suc (suc (suc n₂)))) (go n₂)))
--   x

module InitialList where
  -- Initial Church lists
  --------------------------------------------------------------------------------

  List : Set → Set
  List A = (L : Set) → (A → L → L) → L → L

  nil : ∀ {A} → List A
  nil L c n = n

  cons : ∀ {A} → A → List A → List A
  cons a as L c n = c a (as L c n)

  -- map : ∀ {A B} → (A → B) → List A → List B
  -- map f [] = []
  -- map f (a:as) = f a : map f as

  map : ∀ {A B} → (A → B) → List A → List B
  map f as L c n =
    as L (λ a bs → c (f a) bs) n
    -- foldr (λ a bs → c (f a) bs) n as

  -- {-# NON_TERMINATING #-}
  -- fix : ∀ {A B : Set} → ((A → B) → A → B) → A → B
  -- fix f a = f (fix f) a

  postulate
    fix : ∀ {A B : Set} → ((A → B) → A → B) → A → B

  -- countDown 5 = [5, 4, 3, 2, 1, 0]
  -- "push" iteration: start with a loop, inline stuff later inside
  countDown : ℕ → List ℕ
  countDown n = λ L cons nil →
    fix (λ go n → caseℕ n
          (cons zero nil)
          (λ n → cons (suc n) (go n)))
        n

  take : ∀ {A} → ℕ → List A → List A
  take n as L cons nil = as _
    (λ a k n → caseℕ n
        nil
        (λ n → cons a (k n)))
    (λ _ → nil)
    n

  reverse : ∀ {A} → List A → List A
  reverse as L c n = as _ (λ a k acc → k (c a acc)) (λ acc → acc) n

  -- optimized by "call pattern specialization" in GHC
  drop : ∀ {A} → ℕ → List A → List A
  drop n as L cons nil = as _
    (λ a k n → caseℕ n
      (cons a (k 0))        -- drop 0 recursive calls drop 0 until the list ends
      (λ n → k n))          -- recursive call drop (n - 1)
    (λ _ → nil)
    n

  filter : ∀ {A} → (A → B.Bool) → List A → List A
  filter f as L cons nil = as _
    (λ a as → B.if f a then cons a as else as)
    nil

  foldl : ∀ {A B} → (B → A → B) → B → List A → B
  foldl f b as = as _ (λ a k b → k (f b a)) (λ b → b) b

  sum : List ℕ → ℕ
  sum = foldl _+_ 0


  downList : ∀ {A} → List A → L.List A   -- CPS to usual list
  downList as = as _ L._∷_ L.[]

  upList : ∀ {A} → L.List A → List A     -- usual list to CPS
  upList as L cons nil = L.foldr cons nil as

  -- List is a monad
  bind : ∀ {A B} → List A → (A → List B) → List B
  bind as f L cons nil = as _
    (λ a bs → f a L cons bs)
    nil

  pure : ∀ {A} → A → List A
  pure a L cons nil = cons a nil

  module GenericListFun where

    -- (α : A-Alg) → U A
    -- CPS A := (α : A-Alg) → U A

    -- goal: define functions in (CPS A → CPS B)

    -- ((α : A-Alg) → U α) → ((β : B-Alg) → U β)

    -- input:  (f : A-Alg → B-Alg) × (∀ α. U (f α) = U α)
    -- output: ((β : B-Alg) → U β) → ((α : A-Alg) → U α)

    -- F-alg: (A : Set) × (F A → A)
    --    fusingFoldr : (∀ A. (F A → A) → G A → A) → (∀ A. (F A → A) → A) → (∀ A. (G A → A) → A)
    --

    fusibleFoldr : ∀ {A B} → (∀ {L} → (B → L → L) → L → (A → L → L) × L) → List A → List B
    fusibleFoldr f as L cons nil = as L (f cons nil .₁) (f cons nil .₂)

    map' : ∀ {A B} → (A → B) → List A → List B
    map' f = fusibleFoldr λ cons nil → (λ a bs → cons (f a) bs) , nil

    filter' : ∀ {A} → (A → B.Bool) → List A → List A
    filter' f = fusibleFoldr (λ c n → (λ a l → B.if f a then c a l else l) , n)

    -- what about returning in comp types?
    -- List A → B → List C
    -- (∀ L. (A → L → L) → L → L) → B → (∀ L. (C → L → L) → L → L)
    -- (∀ L. (A → L → L) → L → L) → (∀ L. B → (C → L → L) → L → L)
    -- (

    -- A → ((α : ListAlg) → U α)

    -- (∀ X. G X → F (A → X)) → CPS F → CPS G

    -- A → (∀ X. (F X → X) → X)
    -- ∀ X. A → (F X → X) → X

    -- fuse : (∀ X. (G X → X) → F X → X) → Fix F → Fix G
    -- fuse : (∀ X. (G X → X) → F (A → X) → (A → X)) → Fix F → A → Fix G


    -- -- fusing with +1 arg
    -- fusibleFoldr1 : ∀ {A B C : Set}
    --              → (∀ {L} → (C → L → L) → L → (A → (B → L) → (B → L)) × (B → L))
    --              → List A → B → List C
    -- fusibleFoldr1 {A} {B} {C} f as b L cons nil =
    --   as (B → L) (f cons nil .₁) (f cons nil .₂) b

    -- take' : ∀ {A} → ℕ → List A → List A
    -- take' n as = fusibleFoldr1 (λ cons nil → (λ a k n → caseℕ n nil λ n → cons a (k n)) , λ _ → nil) as n


    -- fusibleFoldr11 : ∀ {A B C : Set}
    --              → (∀ {L} → (C → L → L) → L → (A → (B → L) → (B → L)) × (B → L))
    --              → List A → B → List C
    -- fusibleFoldr11 {A} {B} {C} f as b L cons nil =
    --   as (B → L) (f cons nil .₁) (f cons nil .₂) b

      -- fusibleFoldr1 (λ n cons nil → (λ a l → caseℕ n nil λ n → {!cons!}) , nil) as n


    -- TODO: - look at fusibleUnfold
    --       - what if we have multiple indexed/mutual so5rts

-- Streams
--------------------------------------------------------------------------------

-- TODO here

-- data Step a s = Stop | Yield a s

Step : Set → Set → Set
Step A S = (Step : Set)(stop : Step)(yield : A → S → Step) → Step

record Stream (A : Set) : Set where
  constructor stream
  field
    State : Set
    next  : State → Step A State
    seed  : State
open Stream public

map : ∀ {A B} → (A → B) → Stream A → Stream B
State (map f as) =
  State as

next  (map f as) s M stop yield =
  next as s _
    stop
    λ a s → yield (f a) s

seed (map f as) =
  seed as

open import Data.Sum

append : ∀ {A} → Stream A → Stream A → Stream A
State (append as as') =
  State as ⊎ State as'       -- does not compute on meta level (state *must* be runtime represented)
                             -- 1. Let the compiler optimize by call pattern spec
                             -- 2. Do a deeply embedded rep of state machines, write an optimizer/compiler for it on meta level
                             --    (Robinson & Lippmeier : Machine Fusion (with split optimization!))
next (append as as') (inj₁ s ) Step stop yield =
  next as s _
    (next as' (seed as') _
      stop
      λ a s' → yield a (inj₂ s'))
    λ a s → yield a (inj₁ s)
next (append as as') (inj₂ s') Step stop yield =
  next as' s' _
    stop
    λ a s' → yield a (inj₂ s')
seed (append as as') =
  inj₁ (seed as)

zip : ∀ {A B} → Stream A → Stream B → Stream (A × B)
State (zip as bs) = State as × State bs
next  (zip as bs) = {!!}   -- try to make two steps
seed  (zip as bs) = seed as , seed bs

filter : ∀ {A} → (A → B.Bool) → Stream A → Stream A
State (filter f as) = State as
next  (filter f as) s Step stop yield =
  next as s _
    stop
    λ a s → B.if f a then yield a s
                     else {!skip s!}  -- scanning forward is *tail recursive*
                     -- where find = tailrec forward (fine, because fusible w/o loss of efficiency)
                     -- in general, skipping over stream elements can be done in a loop

          -- scan forward until the next output yield (recursively)
          -- Rust iterators: exactly this solution
          --    using recursion inside stepping functions

seed  (filter f as) = seed as

  --
  --   Haskell stream fusion: use Skip streams
  --   data Step s a = Stop | Yield a s | Skip s
  --   codata SkipStream a = Stop | Yield a (SkipStream a) | Skip (SkipStream a)
  --   Stream (Maybe a) ~ SkipStream a

  -- Skip is unnecessary if we have enough fusion guarantees (staging)

-- binding streams?
--------------------------------------------------------------------------------

-- not available in GHC
-- Another paper: "Staged Stream Fusion to Completeness"

-- bind can be implemented without closures if you have sigma types

open import Data.Maybe

bind : ∀ {A B} → Stream A → (A → Stream B) → Stream B
State (bind as f) = State as × Maybe (∃ (State ∘ f))    -- (s, nothing) : computing next outer index
                                                        -- (s, just s') : computing next inner index
next  (bind as f) = {!!}
seed  (bind as f) = seed as , nothing
-- we don't have Σ at runtime in my lang....


-- Proposal for stream fusion
--------------------------------------------------------------------------------

-- Inductive encoding for lists + coinductive encoding for "affine" streams

-- streams:    unfold, iterate over data structures, ranges, enumeration, filter, take, drop, zip
-- inductive:  full feature set

-- assymmetric zip:    ChurchList A → Stream B → ChurchList (A × B)
--                     Stream B → ChurchList A → ChurchList (A × B)

-- pull : Stream A → ChurchList A

-- do x ← pull $ range 0 100
--    y ← pull $ zip (enumFrom 0) (toStream xs)
--    return $ x + y

-- no closures, no compiler magic required

-- example for Lippmeier magic:
--     xs : Stream Int
--     zip (map (+10) xs) xs      -- nonlinear xs usage (machine is duplicated)
--     map (λ x → (x + 10, x)) xs


-- automatic fusion?

-- map : ∀ {A B} → (A → B) → List A → List B
-- map f []     = []
-- map f (a:as) = f a : map f as
--           length (map f as)
--           case map f as of [] -> _; _ -> ...

{-
map : ∀ {A B} → (A → B) → List A → List B
map f as L c n = as L (λ a bs → c (f a) bs) n
-}

-- syntactic translation from runtime TT to 2LTT
--   whenever A  : VTy
--            Aᴹ : (A-Alg : MTy) × (U : A-Alg → CTy)
--     t  : Tm Γ (A : VTy)
--     tᴹ : Tm Γᴹ ((α : A-Alg) → U α)

--  trueᴹ  := church true
--  falseᴹ := ...
--  ifᴹ    := ..

-- interesting: usual CPS translation: *everything* incl function is CPS'd
--              wrap everything in Chruch Id functor
--              CId A = (Id : Set) → (A → Id) → Id

-- only for finite types
--   try to do it anyway: supercompilation

-- restrict "foldr" to only the definitions which are fusable?

-- we can't eliminate recursive results of folding

-- How to define fusibleFoldr?
