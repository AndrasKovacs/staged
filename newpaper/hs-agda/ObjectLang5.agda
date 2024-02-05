
module ObjectLang5 where

open import Relation.Binary.PropositionalEquality
  renaming (subst to tr; sym to infix 5 _⁻¹; trans to infixr 4 _◼_;
            cong to ap)
  hiding ([_])

open import Data.Product
  renaming (proj₁ to ₁; proj₂ to ₂)
  hiding (_<*>_)

open import Data.Maybe hiding (ap)
open import Data.Nat hiding (_+_; _*_)
open import Function
open import Data.Unit
open import Data.Empty

--------------------------------------------------------------------------------

return : ∀ {i}{A : Set i} → A → Maybe A
return = just

infixl 1 _>>_
_>>_ : ∀ {i j}{A : Set i}{B : Set j} → Maybe A → Maybe B → Maybe B
ma >> mb = ma >>= λ _ → mb

infixl 4 _<$>_
_<$>_ : ∀ {i j}{A : Set i}{B : Set j} → (A → B) → Maybe A → Maybe B
f <$> ma = do
  a ← ma
  just (f a)

infixl 4 _<*>_
_<*>_ : ∀ {i j}{A : Set i}{B : Set j} → Maybe (A → B) → Maybe A → Maybe B
mf <*> ma = do
  f ← mf
  a ← ma
  just (f a)

-- Syntax
--------------------------------------------------------------------------------

mutual
  data ValTy : Set where
    one   : ValTy
    _+_   : ValTy → ValTy → ValTy
    _*_   : ValTy → ValTy → ValTy
    μ     : Functor → ValTy

  data Functor : Set where
    K   : ValTy → Functor
    Id  : Functor
    _+_ : Functor → Functor → Functor
    _*_ : Functor → Functor → Functor

  data CompTy : Set where
    _⇒_ : ValTy → Ty → CompTy
  infixr 4 _⇒_

  data Ty : Set where
    V : ValTy → Ty
    ℂ : CompTy → Ty

infixr 5 _+_
infixr 6 _*_

⟦_⟧ : Functor → ValTy → ValTy
⟦ K A   ⟧ _ = A
⟦ Id    ⟧ A = A
⟦ F + G ⟧ A = ⟦ F ⟧ A + ⟦ G ⟧ A
⟦ F * G ⟧ A = ⟦ F ⟧ A * ⟦ G ⟧ A


data Con : Set where
  ∙   : Con
  _▶_ : Con → Ty → Con
infixl 3 _▶_

variable
  Γ Δ : Con
  A B C : Ty
  a b c : ValTy
  α β γ : CompTy
  F G : Functor

data Var : Con → Ty → Set where
  zero : Var (Γ ▶ A) A
  suc  : Var Γ A → Var (Γ ▶ B) A

data Tm (Γ : Con) : Ty → Set where
  varC   : Var Γ (ℂ α) → Tm Γ (ℂ α)
  varV   : Var Γ (V a) → Tm Γ (V a)
  letV   : Tm Γ (V a) → Tm (Γ ▶ V a) B → Tm Γ B
  letC   : Tm (Γ ▶ ℂ α) (ℂ α) → Tm (Γ ▶ ℂ α) B → Tm Γ B
  lam    : Tm (Γ ▶ V a) B → Tm Γ (ℂ (a ⇒ B))
  app    : Tm Γ (ℂ (a ⇒ B)) → Tm Γ (V a) → Tm Γ B
  pair   : Tm Γ (V a) → Tm Γ (V b) → Tm Γ (V (a * b))
  fst    : Tm Γ (V (a * b)) → Tm Γ (V a)
  snd    : Tm Γ (V (a * b)) → Tm Γ (V b)
  tt     : Tm Γ (V one)
  inl    : Tm Γ (V a) → Tm Γ (V (a + b))
  inr    : Tm Γ (V b) → Tm Γ (V (a + b))
  split  : Tm Γ (V (a + b)) → Tm (Γ ▶ V a) C → Tm (Γ ▶ V b) C → Tm Γ C
  wrap   : Tm Γ (V (⟦ F ⟧ (μ F))) → Tm Γ (V (μ F))
  unwrap : Tm Γ (V (μ F)) → Tm Γ (V (⟦ F ⟧ (μ F)))


-- Fuel-based operational semantics
--------------------------------------------------------------------------------

mutual
  data Val : ValTy → Set where
    tt     : Val one
    pair   : Val a → Val b → Val (a * b)
    inl    : Val a → Val (a + b)
    inr    : Val b → Val (a + b)
    wrap   : Val (⟦ F ⟧ (μ F)) → Val (μ F)

  data Env : Con → Set where
    nil    : Env ∙
    defV : Env Γ → Val a → Env (Γ ▶ V a)
    defC : Env Γ → Tm (Γ ▶ ℂ α) (ℂ α) → Env (Γ ▶ ℂ α)

data Args : Ty → ValTy → Set where
  []   : Args (V a) a
  app  : Args B c → Val a → Args (ℂ (a ⇒ B)) c

lookupV : Var Γ (V a) → Env Γ → Val a
lookupV zero    (defV γ v) = v
lookupV (suc x) (defV γ _) = lookupV x γ
lookupV (suc x) (defC γ _) = lookupV x γ

mutual
  call : Var Γ (ℂ α) → ℕ → Env Γ → Args (ℂ α) b → Maybe (Val b)
  call zero    zero    (defC _ _) _  = nothing
  call zero    (suc k) (defC γ t) as = eval t k (defC γ t) as
  call (suc x) k       (defV γ _) as = call x k γ as
  call (suc x) k       (defC γ _) as = call x k γ as

  eval : Tm Γ A → ℕ → Env Γ → Args A b → Maybe (Val b)
  eval (varV x)      k γ []         = return (lookupV x γ)
  eval (varC x)      k γ as         = call x k γ as
  eval (letV t u)    k γ as         = do v ← eval t k γ []; eval u k (defV γ v) as
  eval (letC t u)    k γ as         = eval u k (defC γ t) as
  eval (lam t)       k γ (app as v) = eval t k (defV γ v) as
  eval (app t u)     k γ as         = do v ← eval u k γ []
                                         eval t k γ (app as v)
  eval (pair t u)    k γ []         = pair <$> eval t k γ [] <*> eval u k γ []
  eval (fst t)       k γ []         = do pair v _ ← eval t k γ []
                                         return v
  eval (snd t)       k γ []         = do pair _ v ← eval t k γ []
                                         return v
  eval tt            k γ []         = return tt
  eval (inl t)       k γ []         = inl <$> eval t k γ []
  eval (inr t)       k γ []         = inr <$> eval t k γ []
  eval (split t l r) k γ as         = do v ← eval t k γ []
                                         case v of λ where
                                           (inl v) → eval l k (defV γ v) as
                                           (inr v) → eval r k (defV γ v) as
  eval (wrap t)      k γ []         = wrap <$> eval t k γ []
  eval (unwrap t)    k γ []         = do wrap v ← eval t k γ []
                                         return v

--------------------------------------------------------------------------------

Ren : Con → Con → Set
Ren Γ Δ = ∀ {A} → Var Δ A → Var Γ A

wk : Ren Γ Δ → Ren (Γ ▶ A) Δ
wk σ = suc ∘ σ

idr : Ren Γ Γ
idr = id

renameTop : Ren Γ Δ → Var Γ A → Ren Γ (Δ ▶ A)
renameTop σ x zero    = x
renameTop σ x (suc y) = σ y

keep : Ren Γ Δ → Ren (Γ ▶ A) (Δ ▶ A)
keep σ = renameTop (wk σ) zero

_[_] : Tm Γ A → Ren Δ Γ → Tm Δ A
varC x      [ σ ] = varC (σ x)
varV x      [ σ ] = varV (σ x)
letV t u    [ σ ] = letV (t [ σ ]) (u [ keep σ ])
letC t u    [ σ ] = letC (t [ keep σ ]) (u [ keep σ ])
lam t       [ σ ] = lam (t [ keep σ ])
app t u     [ σ ] = app (t [ σ ]) (u [ σ ])
pair t u    [ σ ] = pair (t [ σ ]) (u [ σ ])
fst t       [ σ ] = fst (t [ σ ])
snd t       [ σ ] = snd (t [ σ ])
tt          [ σ ] = tt
inl t       [ σ ] = inl (t [ σ ])
inr t       [ σ ] = inr (t [ σ ])
split t l r [ σ ] = split (t [ σ ]) (l [ keep σ ]) (r [ keep σ ])
wrap t      [ σ ] = wrap (t [ σ ])
unwrap t    [ σ ] = unwrap (t [ σ ])

--------------------------------------------------------------------------------

data RenEnv : ∀ {Γ Δ} → (σ : Ren Γ Δ) → Env Γ → Env Δ → Set where
  identity   : ∀ {γ} → RenEnv (idr {Γ}) γ γ
  renameTopV : ∀ {σ : Ren Γ Δ}{γ γ'}(x : Var Γ (V a)) → RenEnv σ γ γ' → RenEnv (renameTop σ x) γ (defV γ' (lookupV x γ))
  dropV      : ∀ {σ : Ren Γ Δ}{γ γ'}{v : Val a} → RenEnv σ γ γ' → RenEnv (wk σ) (defV γ v) γ'
  keepC      : ∀ {σ : Ren Γ Δ}{γ γ'}{t : Tm (Δ ▶ ℂ α) (ℂ α)} → RenEnv σ γ γ' → RenEnv (keep σ) (defC γ (t [ keep σ ])) (defC γ' t)

ren-lookupV : ∀ {σ : Ren Δ Γ}(x : Var Γ (V a)) {γ γ'} (p : RenEnv σ γ γ') → lookupV (σ x) γ ≡ lookupV x γ'
ren-lookupV x       identity         = refl
ren-lookupV zero    (renameTopV y p) = refl
ren-lookupV (suc x) (renameTopV y p) = ren-lookupV x p
ren-lookupV x       (dropV p)        = ren-lookupV x p
ren-lookupV (suc x) (keepC p)        = ren-lookupV x p

mutual
  ren-call : ∀ {σ : Ren Δ Γ}(x : Var Γ (ℂ α)) k {γ γ'} (p : RenEnv σ γ γ'){b} as → call {b = b}(σ x) k γ as ≡ call x k γ' as
  ren-call x       k       identity                  as = refl
  ren-call (suc x) k       (renameTopV y p)          as = ren-call x k  p as
  ren-call x       k       (dropV p)                 as = ren-call x k  p as
  ren-call zero    zero    (keepC p)                 as = refl
  ren-call zero    (suc k) (keepC {σ = σ} {t = t} p) as = ren-eval (keep σ) t k (keepC p) as
  ren-call (suc x) k       (keepC p)                 as = ren-call x k p as

  ren-eval : ∀ (σ : Ren Δ Γ)(t : Tm Γ A) k {γ γ'} (p : RenEnv σ γ γ'){b} as → eval {b = b}(t [ σ ]) k γ as ≡ eval t k γ' as
  ren-eval σ (varC x) k      p as = ren-call x k p as
  ren-eval σ (varV x) k      p [] = ap just (ren-lookupV x p)
  ren-eval σ (letV t u) k    p as = {!!}
  ren-eval σ (letC t u) k    p as = {!!}
  ren-eval σ (lam t) k       p as = {!!}
  ren-eval σ (app t u) k     p as = {!!}
  ren-eval σ (pair t u) k    p as = {!!}
  ren-eval σ (fst t) k       p as = {!!}
  ren-eval σ (snd t) k       p as = {!!}
  ren-eval σ tt k            p as = {!!}
  ren-eval σ (inl t) k       p as = {!!}
  ren-eval σ (inr t) k       p as = {!!}
  ren-eval σ (split t l r) k p as = {!!}
  ren-eval σ (wrap t) k      p as = {!!}
  ren-eval σ (unwrap t) k    p as = {!!}


{-

to show: Rel σ γ γ' → evalₖ t[σ] γ as = eval t γ' as

Not bad idea, but my program equivalence is tooo strictttt!!!!

-}


--------------------------------------------------------------------------------


{-

(t ~ₙ t') = ∀ (k ≤ n) γ γ' (γ~ : γ ~ₙ γ') as. evalₖ t γ as ≡ evalₖ t' γ' as

(t ▶~₀ t')     = ⊤                 -- the term equivalence one step later
(t ▶~(n+1) t') = t ~ₙ t'

(defVal γ v  ~ₙ defVal γ' v')  = (γ ~ₙ γ') ∧ (v = v')
(defComp γ x ~ₙ defComp γ' t') = (γ ~ₙ γ') ∧ (t ▶~ₙ t')
(defRec γ x  ~ₙ defRec γ' t')  = (γ ~ₙ γ') ∧ (t ▶~ₙ t')
(defComp γ x ~ₙ defRec γ' x')  = ⊥
(defRec γ x  ~ₙ defComp γ' x') = ⊥


Equivalence of variables is just equality

--------------------------------------------------------------------------------

RESTRICTION for _~_
   goal : ∀ n. t ~(suc n) t' → t ~ₙ t'
      p : t ~(suc n) t'
      k ≤ n
      γ γ' (γ~ : γ ~ₖ γ') as
      -----------------------
      goal: evalₖ t γ as ≡ evalₖ t' γ' as
      p k γ γ' γ~ as   OK

   Pointwise for Env~
   by case distinction for ~↓

--------------------------------------------------------------------------------

REFLEXIVITY: ∀ t. t ~ t
  induction on t

  case var:
    ∀ n. var x ~ₙ var x
    ∀ n (k ≤ n) γ γ' γ~ as. evalVarₖ γ x as ≡ evalVarₖ γ' x as

    Induction on x.
    Only interesting cases are defComp and defRec lookup

    case γ = defComp γ t, and γ' = defComp γ' t', γ~ = (γ~ : γ ~ₖ γ', t~ : t ▶~ₖ t')
      if k = 0 then
        both sides nothing OK
      id k = suc k then
        goal:  evalₖ t γ as ≡ evalₖ t' γ' as
        OK by t~ : t ~ₖ t'  and (wk γ~ : γ ~ₖ γ')

    case γ = defRec γ t, γ' = defRec γ' t', γ~ = (γ~ : γ ~ₖ γ', t~ : t ▶~ₖ t')
      if k = 0 then OK
      if k = suc k then
        goal: evalₖ t (defRec γ t) as ≡ evalₖ t' (defRec γ' t') as
        t~ (wk γ~ , t~) as OK   -- NOT good here!   we neeed t ▶~ₖ t', but t~ is t ~ₖ t' !!!


  case (let t u), at (V A) type
     t~ : t ~ t
     u~ : u ~ u
     k ≤ n
     γ γ' γ~ as
     goal :
       (do v ← evalₖ t γ  []; evalₖ u (defVal γ  v) as) ≡
       (do v ← evalₖ t γ' []; evalₖ u (defVal γ' v) as)

     by t~, I know that evalₖ t γ  [] ≡ evalₖ t γ'  []
     case evalₖ t γ  [] of
       nothing OK
       just v
         goal : evalₖ u (defVal γ v) as) ≡ evalₖ u (defVal γ' v) as)
         OK by u~, γ~ and v = v

  case (let t u) at ℂ A type
     t~ : t ~ t
     u~ : u ~ u
     k ≤ n
     γ γ' γ~ as
     goal :
       evalₖ u (defComp γ t) as ≡ evalₖ u (defComp γ' t) as
       OK by u~, γ~ and t~

   TODO REST

--------------------------------------------------------------------------------

SYMMETRY for _~_
  t ~ t' → ∀ n. t' ~ₙ t
  Assume (t~ : t ~ t')
  By strong induction on n.
    hyp : ∀ k < n. t' ~ₖ t
    k ≤ n
    γ γ' γ~ as.
    goal : evalₖ γ t' as = evalₖ γ' t as
    γ~ can be inverted using hyp (casing on k, applying hyp to terms)
    hence OK by t~ (γ~⁻¹) as

--------------------------------------------------------------------------------

TRANSITIVITY for _~_
  (p : t ~ t') (q : t' ~ t'') → (∀ n. t ~ₙ t'')
  NO INDUCTION
  k ≤ n
  γ γ' γ~ as.
  goal : evalₖ γ t as = evalₖ γ' t'' as
  p k γ~ : evalₖ γ t as = evalₖ γ' t' as
  q k (refl~ γ') : evalₖ γ' t' as = evalₖ γ' t'' as
  OK by transitivity of the two

--------------------------------------------------------------------------------

CONGRUENCE for _~_:

  case VAR: x = x' → x ~ x  OK by reflexivity

  case (let t u) at type V A
     t~ : t ~ t'
     u~ : u ~ u'
     k ≤ n
     γ γ' γ~ as
     goal:
       (do v ← evalₖ t γ  []; evalₖ u (defVal γ  v) as) ≡
       (do v ← evalₖ t' γ' []; evalₖ u' (defVal γ' v) as)
     OK

  case (let t u) at type ℂ A
     t~ : t ~ t'
     u~ : u ~ u'
     k ≤ n
     γ γ' γ~ as
     goal:
       (evalₖ u (defComp γ t) as) =
       (evalₖ u' (defComp γ' t') as)
     OK

  case (letrec t u)
     t~ : t ~ t'
     u~ : u ~ u'
     k ≤ n
     γ γ' γ~ as
     goal:
       (evalₖ u  (defRec γ  t ) as) =
       (evalₖ u' (defRec γ' t') as)
     OK

  case (lam t)
     t~ : t ~ t'
     k ≤ n
     γ γ' γ~ (as = app as v)
     goal:
       (evalₖ t (defVal γ v) as) =
       (evalₖ t' (defVal γ' v) as)
     OK

  case (app t u)
    t~ : t ~ t'
    u~ : u ~ u'
    k ≤ n
    γ γ' γ~ as
    goal:
      (do v ← evalₖ γ u []; evalₖ γ t (app as v))
      =
      (do v ← evalₖ γ' u' []; evalₖ γ' t' (app as v))
    OK

  case (fst t)
    t~ : t ~ t'
    k ≤ n
    γ γ' γ~
    goal:
      (do pair v _ ← evalₖ γ t []; return v)
      =
      (do pair v _ ← evalₖ γ' t' []; return v)
  OK

  TODO REST


--------------------------------------------------------------------------------

RENAMING

Ren : Con → Con → Set
id  : Ren Γ Γ
_,_ : Ren Γ Δ → Var Γ A → Ren Γ (Δ ▶ A)

action on terms defined as usual
_[_] : Tm Γ A → Ren Δ Γ → Tm Δ A



-}
