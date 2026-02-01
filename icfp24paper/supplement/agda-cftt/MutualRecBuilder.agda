
module MutualRecBuilder where

{-
Builder for mutually recursive blocks, based on "Mutually Recursive Definition Builders" by Kazutaka Matsuda:
  https://trendsfp.github.io/papers/tfp26-paper-16.pdf
-}

open import Lib
open import Object
open import Gen
open import Agda.Builtin.TrustMe


-- Action for building a mutually recursive block
data MutBuilder (R : CTy) : Set where
  MkLetRec : ∀ b → (↑C b → MutBuilder (R ×C b)) → MutBuilder R
  MkLet    : ∀ {b} → ↑ b → (↑ b → MutBuilder R) → MutBuilder R
  MkBody   : ↑C R → MutBuilder R

-- Compute the type of the block, which is a nested computation product type.
blockTy : ∀ {R} → MutBuilder R → CTy
blockTy (MkLetRec b f)  = blockTy (f loop∘) -- instantiate binder with bottom value (doesn't matter, since
                                        -- object types are non-dependent)
blockTy (MkLet x f)     = blockTy (f x)
blockTy {R}(MkBody γ)   = R

-- POSTULATE: object types in a MutBuilder cannot depend on object term binders.
generativeBlockTy : ∀ {R b}(f : ↑ b → MutBuilder R) → ∀ x y → blockTy {R} (f x) ≡ blockTy {R} (f y)
generativeBlockTy f x y = primTrustMe

castInp : ∀ {R b x y} (f : ↑ b → MutBuilder R) → ↑C (blockTy (f x)) → ↑C (blockTy (f y))
castInp f r = tr ↑C (generativeBlockTy f _ _) r

-- map over the mutual recursive definitions in a MutBuilder
mapBlock : ∀ {R R'} → (↑C R → ↑C R') → MutBuilder R → MutBuilder R'
mapBlock σ (MkLetRec b f) = MkLetRec b λ x → mapBlock (λ r → σ (fst∘ r) ,C x) (f x)
mapBlock σ (MkLet x f)    = MkLet x λ x → mapBlock σ (f x)
mapBlock σ (MkBody r)     = MkBody (σ r)

projectInp : ∀ {R b} (f : MutBuilder R) → (↑C R → ↑C b) → ↑C (blockTy {R} f) → ↑C b
projectInp (MkLetRec b f) proj = projectInp (f loop∘) (proj ∘ fst∘)
projectInp (MkLet x f)    proj = projectInp (f x) proj
projectInp (MkBody r)     proj = proj

sndInp : ∀ {R b} f → ↑C (blockTy {R ×C b} f) → ↑C b
sndInp f = projectInp f snd∘

-- interpret a builder as a code generator for the block
runMutBuilder : ∀ {R}(f : MutBuilder R) → ↑C (blockTy f) → Gen (↑C (blockTy f))
runMutBuilder (MkLetRec b f) r = do r ← runMutBuilder (f (sndInp (f loop∘) r)) (castInp f r)
                                    pure (castInp f r)
runMutBuilder (MkLet x f)    r = do x ← genLet x
                                    r ← runMutBuilder (f x) (castInp f r)
                                    pure (castInp f r)
runMutBuilder (MkBody r)     _ = pure r

record MutBuilderM (A : Set) : Set where
  constructor mbm
  field
    unMutBuilderM : ∀ {R} → (A → MutBuilder R) → MutBuilder R
open MutBuilderM public

runMutBuilderM : ∀ {A} → MutBuilderM A → MutBuilder ⊤C
runMutBuilderM (mbm f) = f (λ _ → MkBody ttC)

blockTyM : ∀ {A} → MutBuilderM A → CTy
blockTyM = blockTy ∘ runMutBuilderM

instance
  ApplicativeMBM : Applicative MutBuilderM
  ApplicativeMBM .Applicative.pure  a = mbm λ k → k a
  ApplicativeMBM .Applicative._<*>_ (mbm f) (mbm g) = mbm λ k → f λ f → g λ a → k (f a)

  MonadMBM : Monad MutBuilderM
  MonadMBM .Monad._>>=_ (mbm f) g = mbm λ k → f λ a → unMutBuilderM (g a) k



-- API
----------------------------------------------------------------------------------------------------

mkLetRec1 : ∀ a {R} → (↑C a → MutBuilderM (↑C a × R)) → MutBuilderM R
mkLetRec1 a f = mbm λ k → MkLetRec a λ x → unMutBuilderM (f x) λ {(a , r) → mapBlock (_,C a) (k r)}

mkLetRec2 : ∀ {a b R} → (↑C a → ↑C b → MutBuilderM (↑C a × ↑C b × R)) → MutBuilderM R
mkLetRec2 {a}{b} f = mkLetRec1 a λ x → mkLetRec1 b λ y → do x , y , r ← f x y; pure (y , x , r)

mkLet : ∀ {a} → ↑ a → MutBuilderM (↑ a)
mkLet x = mbm (MkLet x)

-- generate a mutually recursive block
buildMutBlock : ∀ {M}{{_ : MonadGen M}}{A}(f : MutBuilderM A) → M (↑C (blockTyM f))
buildMutBlock f = genLetRec (runGen ∘ runMutBuilder (runMutBuilderM f))



-- Examples
----------------------------------------------------------------------------------------------------

-- f = λ n. g (n + 1)
-- g = λ n. f (n + 1)

action1 : MutBuilderM ⊤
action1 =
  mkLetRec1 (ℕ∘ ⇒ V ℕ∘) λ f →
  mkLetRec1 (ℕ∘ ⇒ V ℕ∘) λ g →
  pure (  (Λ λ n → f ∙ (n +∘ lit∘ 1))   -- reversed order
        , (Λ λ n → g ∙ (n +∘ lit∘ 1))
        , tt )

run1 : ↑V ⊤∘
run1 = runGen do
  fs <- buildMutBlock action1
  pure tt∘

run1Test :
  run1 ≡ (LetRec ((⊤C ×C (ℕ∘ ⇒ V ℕ∘)) ×C (ℕ∘ ⇒ V ℕ∘))
           (λ x →
                  (ttC
               ,C (Λ (λ n → snd∘ x ∙ (n +∘ lit∘ 1))))
               ,C (Λ (λ n → snd∘ (fst∘ x) ∙ (n +∘ lit∘ 1))))
           (λ _ → tt∘))
run1Test = refl


action2 : MutBuilderM ⊤
action2 =
  mkLetRec1 (ℕ∘ ⇒ V ℕ∘) λ f →
  mkLetRec1 (ℕ∘ ⇒ V ℕ∘) λ g → do
    foo ← mkLet (lit∘ 200)
    pure (  (Λ λ n → f ∙ (n +∘ lit∘ 1))
          , (Λ λ n → g ∙ (n +∘ foo))
          , tt )

run2 : ↑V ⊤∘
run2 = runGen do
  fs <- buildMutBlock action2
  pure tt∘

run2Test :
  run2 ≡
    (LetRec ((⊤C ×C (ℕ∘ ⇒ V ℕ∘)) ×C (ℕ∘ ⇒ V ℕ∘))
      (λ x → Let (lit∘ 200) λ a →
               (ttC ,C
                Λ (λ n → snd∘ x ∙ (n +∘ a))) ,C
                Λ (λ n → snd∘ (fst∘ x) ∙ (n +∘ lit∘ 1))))
      (λ _ → tt∘)
run2Test = refl

----------------------------------------------------------------------------------------------------
