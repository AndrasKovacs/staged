
module MutualRecBuilder where

{-
Builder for mutually recursive blocks, based on
  https://trendsfp.github.io/papers/tfp26-paper-16.pdf
-}

open import Lib
open import Object
open import SOP
open import Gen
open import Agda.Builtin.TrustMe


-- Action for building a mutually recursive block
data MutBuilder (R : CTy)(A : Ty) : Set where
  MkLetRec : ∀ b → (↑C b → MutBuilder (R ×C b) A) → MutBuilder R A
  MkLet    : ∀ {b} → ↑ b → (↑ b → MutBuilder R A) → MutBuilder R A
  MkBody   : ↑C R → ↑ A → MutBuilder R A

-- Compute the input type of the block, which is a nested computation product type.
inpTy : ∀ {R A} → MutBuilder R A → CTy
inpTy (MkLetRec b f)  = inpTy (f loop∘) -- instantiate binder with bottom value (doesn't matter, since
                                        -- object types are non-dependent)
inpTy (MkLet x f)     = inpTy (f x)
inpTy {R}(MkBody γ a) = R

-- POSTULATE: object types in a MutBuilder cannot depend on object term binders.
generativeInpTy : ∀ {R b A}(f : ↑ b → MutBuilder R A) → ∀ x y → inpTy {R} (f x) ≡ inpTy {R} (f y)
generativeInpTy f x y = primTrustMe

castInp : ∀ {R b A x y} (f : ↑ b → MutBuilder R A) → ↑C (inpTy (f x)) → ↑C (inpTy (f y))
castInp f r = tr ↑C (generativeInpTy f _ _) r

-- map over the mutual recursive definitions in a MutBuilder
mapBlock : ∀ {R R' A} → (↑C R → ↑C R') → MutBuilder R A → MutBuilder R' A
mapBlock σ (MkLetRec b f) = MkLetRec b λ x → mapBlock (λ r → σ (fst∘ r) ,C x) (f x)
mapBlock σ (MkLet x f)    = MkLet x λ x → mapBlock σ (f x)
mapBlock σ (MkBody r a)   = MkBody (σ r) a

projectInp : ∀ {R b A} (f : MutBuilder R A) → (↑C R → ↑C b) → ↑C (inpTy {R} f) → ↑C b
projectInp (MkLetRec b f) proj = projectInp (f loop∘) (proj ∘ fst∘)
projectInp (MkLet x f)    proj = projectInp (f x) proj
projectInp (MkBody r x)   proj = proj

sndInp : ∀ {R b A} f → ↑C (inpTy {R ×C b} {A} f) → ↑C b
sndInp f = projectInp f snd∘

-- interpret a builder as a code generator
runMutBuilder : ∀ {R A}(f : MutBuilder R A) → ↑C (inpTy f) → Gen (↑C (inpTy f) × ↑ A)
runMutBuilder (MkLetRec b f) r = do r , a ← runMutBuilder (f (sndInp (f loop∘) r)) (castInp f r)
                                    pure (castInp f r , a)
runMutBuilder (MkLet x f)    r = do x ← genLet x
                                    r , a ← runMutBuilder (f x) (castInp f r)
                                    pure (castInp f r , a)
runMutBuilder (MkBody r x)   _ = pure (r , x)

record MutBuilderM (A : Set) : Set where
  constructor mbm
  field
    unMutBuilderM : ∀ {R B} → (A → MutBuilder R B) → MutBuilder R B
open MutBuilderM public

instance
  ApplicativeMBM : Applicative MutBuilderM
  ApplicativeMBM .Applicative.pure  a = mbm λ k → k a
  ApplicativeMBM .Applicative._<*>_ (mbm f) (mbm g) = mbm λ k → f λ f → g λ a → k (f a)

  MonadMBM : Monad MutBuilderM
  MonadMBM .Monad._>>=_ (mbm f) g = mbm λ k → f λ a → unMutBuilderM (g a) k

  -- Run a Gen action and insert its results as a new let-definition.
  MonadGenMBM : MonadGen MutBuilderM
  MonadGenMBM .MonadGen.liftGen ma = mbm λ k → {!unGen ma!}


-- API
----------------------------------------------------------------------------------------------------

letr1 : ∀ a {R} → (↑C a → MutBuilderM (↑C a × R)) → MutBuilderM R
letr1 a f = mbm λ k → MkLetRec a λ x → unMutBuilderM (f x) λ {(a , r) → mapBlock (_,C a) (k r)}

letr2 : ∀ {a b R} → (↑C a → ↑C b → MutBuilderM (↑C a × ↑C b × R)) → MutBuilderM R
letr2 {a}{b} f = letr1 a λ x → letr1 b λ y → do x , y , r ← f x y; pure (y , x , r)



-- generate a mutually recursive block
runMutBuilderM : ∀ {M}{{_ : MonadGen M}}{A} → MutBuilderM A → M A
runMutBuilderM = {!!}



-- local'' : ∀ {A} → MBM (↑ A) → ↑ A
-- local'' {A}(mbm f) = LetRec _ (λ γ → runGen do γ , _ ← collect (f ⊤C A (ret ttC)) γ; pure γ)
--                               (λ γ → runGen do _ , r ← collect (f ⊤C A (ret ttC)) γ; pure r)

-- example : MutBuilder ⊤C (V ℕ∘)
-- example = letr (⊤∘ ⇒ V ⊤∘) λ f →
--           letr (⊤∘ ⇒ C (⊤∘ ⇒ V ⊤∘)) λ g →
--           letr (⊤∘ ⇒ C (⊤∘ ⇒ C (⊤∘ ⇒ V ⊤∘))) λ h →
--           ret (((ttC ,C g ∙ tt∘) ,C h ∙ tt∘) ,C Λ λ _ → Λ λ _ → Λ λ x → x) (lit∘ 0)


-- module Try1 where

  -- data MutBuilder : CTy → Ty → Set where
  --   MkLetRec : ∀ {Γ A} b → (↑C b → MutBuilder (Γ ×C b) A) → MutBuilder Γ A
  --   MkLet    : ∀ {Γ A b} → ↑ b → (↑ b → MutBuilder Γ A) → MutBuilder Γ A
  --   MkDefRec : ∀ {Γ A b} → ↑C b → MutBuilder Γ A → MutBuilder (Γ ×C b) A
  --   MkBody   : ∀ {A} → ↑ A → MutBuilder ⊤C A

  -- recTy : ∀ {Γ A} → MutBuilder Γ A → CTy → CTy
  -- recTy (MkLetRec b f) acc = recTy (f loop∘) (acc ×C b)
  -- recTy (MkLet x f)    acc = recTy (f x) acc
  -- recTy (MkDefRec x f) acc = recTy f acc
  -- recTy (MkBody x)     acc = acc

  -- -- POSTULATE
  -- genTel : ∀ {Γ a A}(f : ↑ a → MutBuilder Γ A) → ∀ x y acc → recTy (f x) acc ≡ recTy (f y) acc
  -- genTel f x y acc = primTrustMe


  -- projTel : ∀ {Γ a A} f → (↑C Γ → ↑C a) → ↑C (tel Γ {A} f) → ↑C a
  -- projTel (letr a f)   proj = projTel (f loop∘) (proj ∘ fst∘)
  -- projTel (embLet x f) proj = projTel (f x) proj
  -- projTel (ret γ a)    proj = proj

  -- sndTel : ∀ {Γ a A} f → ↑C (tel (Γ ×C a) {A} f) → ↑C a
  -- sndTel f = projTel f snd∘

  -- collect : ∀ {Γ A}(f : MutBuilder Γ A) → ↑C (tel _ f) → Gen (↑C (tel _ f) × ↑ A)
  -- collect


  -- collect {Γ}{A}(letr a f) inp = do
  --   γ , a ← collect (f (sndTel (f loop∘) inp)) (tr ↑C (genTel f _ _) inp)
  --   pure (tr ↑C (genTel f _ _) γ , a)
  -- collect {Γ}{A} (embLet x f) inp = do
  --   x ← genLet x
  --   γ , a ← collect (f x) (tr ↑C (genTel f _ _) inp)
  --   pure (tr ↑C (genTel f _ _) γ , a)
  -- collect (ret γ a) inp =
  --   pure (γ , a)

  -- record MBM (A : Set) : Set where
  --   constructor mbm
  --   field
  --     unMBM : ∀ Γ R → (A → MutBuilder Γ R) → MutBuilder Γ R
  -- open MBM public

  -- instance
  --   AMBM : Applicative MBM
  --   AMBM .Applicative.pure  a = mbm λ Γ R k → k a
  --   AMBM .Applicative._<*>_ (mbm f) (mbm g) = mbm λ Γ R k → f Γ R λ f → g Γ R λ a → k (f a)

  --   MonadMBM : Monad MBM
  --   MonadMBM .Monad._>>=_ (mbm f) g = mbm λ Γ R k → f Γ R λ a → unMBM (g a) Γ R k

  -- letr1 : ∀ a {R} → (↑C a → MBM (↑C a × R)) → MBM R
  -- letr1 a {R} f = mbm λ Γ R' k → letr a λ x → unMBM (f x) (Γ ×C a) R' λ ar → substBuilder (λ γ → γ ,C ar .₁) (k (₂ ar))

  -- letr2 : ∀ {a b R} → (↑C a → ↑C b → MBM (↑C a × ↑C b × R)) → MBM R
  -- letr2 {a}{b} f = letr1 a λ a → letr1 b λ b → do a , b , r ← f a b; pure (b , a , r)

  -- local'' : ∀ {A} → MBM (↑ A) → ↑ A
  -- local'' {A}(mbm f) = LetRec _ (λ γ → runGen do γ , _ ← collect (f ⊤C A (ret ttC)) γ; pure γ)
  --                               (λ γ → runGen do _ , r ← collect (f ⊤C A (ret ttC)) γ; pure r)

  -- example : MutBuilder ⊤C (V ℕ∘)
  -- example = letr (⊤∘ ⇒ V ⊤∘) λ f →
  --           letr (⊤∘ ⇒ C (⊤∘ ⇒ V ⊤∘)) λ g →
  --           letr (⊤∘ ⇒ C (⊤∘ ⇒ C (⊤∘ ⇒ V ⊤∘))) λ h →
  --           ret (((ttC ,C g ∙ tt∘) ,C h ∙ tt∘) ,C Λ λ _ → Λ λ _ → Λ λ x → x) (lit∘ 0)
