


module ObjectLang4 where

{- Large product, large unit, letrec -}

open import Relation.Binary.PropositionalEquality
  renaming (subst to tr; sym to infix 5 _⁻¹; trans to infixr 4 _◼_;
            cong to ap)
  hiding ([_])

open import Data.Product
  renaming (proj₁ to ₁; proj₂ to ₂)
  hiding (_<*>_)

open import Data.Maybe
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
  var    : Var Γ A → Tm Γ A
  let'   : Tm Γ A → Tm (Γ ▶ A) B → Tm Γ B
  letrec : Tm (Γ ▶ ℂ α) (ℂ α) → Tm (Γ ▶ ℂ α) B → Tm Γ B
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
    nil       : Env ∙
    defVal    : Env Γ → Val a → Env (Γ ▶ V a)
    defComp   : Env Γ → Tm Γ (ℂ α) → Env (Γ ▶ ℂ α)
    defRec    : Env Γ → Tm (Γ ▶ ℂ α) (ℂ α) → Env (Γ ▶ ℂ α)

data Args : Ty → ValTy → Set where
  []   : Args (V a) a
  app  : Args B c → Val a → Args (ℂ (a ⇒ B)) c

-- Fuel only counts function variable calls! (recursive or not)

mutual
  evalVar : Var Γ A → ℕ → Env Γ → Args A b → Maybe (Val b)
  evalVar zero    k       (defVal env v)  [] = return v
  evalVar zero    (suc k) (defComp env t) sp = eval t k env sp
  evalVar zero    zero    (defComp env t) sp = nothing
  evalVar zero    (suc k) (defRec env t)  sp = eval t k (defRec env t) sp
  evalVar zero    zero    (defRec env t)  sp = nothing
  evalVar (suc x) k       (defVal env _)  sp = evalVar x k env sp
  evalVar (suc x) k       (defComp env _) sp = evalVar x k env sp
  evalVar (suc x) k       (defRec env _)  sp = evalVar x k env sp

  bindLet : Tm Γ A → ℕ → Env Γ → Maybe (Env (Γ ▶ A))
  bindLet {A = V _} t k env = defVal env <$> eval t k env []
  bindLet {A = ℂ _} t k env = return (defComp env t)

  evalSplit : ∀{d}(l : Tm (Γ ▶ V a) C)(r : Tm (Γ ▶ V b) C) → Val (a + b) → ℕ → Env Γ → Args C d → Maybe (Val d)
  evalSplit l r (inl v) k e args = eval l k (defVal e v) args
  evalSplit l r (inr v) k e args = eval r k (defVal e v) args

  eval : Tm Γ A → ℕ → Env Γ → Args A b → Maybe (Val b)
  eval (var x)       k env args       = evalVar x (k) env args
  eval (let' t u)    k env args       = do env ← bindLet t k env
                                           eval u k env args
  eval (letrec t u)  k env args       = eval u k (defRec env t) args
  eval (lam t)       k env (app as v) = eval t k (defVal env v) as
  eval (app t u)     k env args       = do v ← eval u k env []
                                           eval t k env (app args v)
  eval (pair t u)    k env []         = pair <$> eval t k env [] <*> eval u k env []
  eval (fst t)       k env []         = do pair v _ ← eval t k env []
                                           return v
  eval (snd t)       k env []         = do pair _ v ← eval t k env []
                                           return v
  eval tt            k env []         = return tt
  eval (inl t)       k env []         = inl <$> eval t k env []
  eval (inr t)       k env []         = inr <$> eval t k env []
  eval (split t l r) k env args       = do v ← eval t k env []
                                           evalSplit l r v k env args

  eval (wrap t)      k env []         = wrap <$> eval t k env []
  eval (unwrap t)    k env []         = do wrap v ← eval t k env []
                                           return v

--------------------------------------------------------------------------------

{-
(t ~₀       t') = ⊤
(t ~(suc n) t') = ∀ (k ≤ n) γ γ' (γ~ : γ ~ₖ γ') as. evalₖ t γ as ≡ evalₖ t' γ' as

(defVal γ x  ~ₙ defVal γ' x')  = (γ ~ₙ γ') × x ≡ x'
(defComp γ x ~ₙ defComp γ' x') = (γ ~ₙ γ') × (x ~ₙ x') k
(defRec γ x  ~ₙ defRec γ' x')  = (γ ~ₙ γ') × (x ~ₙ x') k
(defComp γ x ~ₙ defRec γ' x')  = ⊥
(defRec γ x  ~ₙ defComp γ' x') = ⊥


RESTRICTION for _~_

∀ n. t ~(suc n) t' → t ~ₙ t'
case n = 0: OK
case n = (suc n)
   p : t ~(suc (suc n)) t'
   k ≤ n
   γ γ' (γ~ : γ ~ₖ γ') args
   -------------------------
   eval t k γ args = eval t' k γ' args
   OK by (p k γ~ args)

Restriction for ~ₑ is pointwise


REFLEXIVITY: ∀ t n. t ~ₙ t

case n = 0: OK
case n = (suc n):
   hyp : ∀ t. t ~ₙ t

   to show: ∀ t. t ~(suc n) t
            ∀ (k ≤ n) γ γ' (γ~ : γ ~ₖ γ') as. evalₖ t γ as ≡ evalₖ t γ' as

   induction on t:

   case var
     ∀ (k ≤ n) γ γ' (γ~ : γ ~ₖ γ') as. evalVarₖ t γ as ≡ evalVarₖ t γ' as

     if k = 0 OK (because val-s are equal)
     if k = suc k:
       if defComp, defComp, we have γ~ = (γ~ : γ ~(k+1) γ', t~ : t ~(k+1) t')
       goal:
         evalVar zero (defComp γ t) as ≡ evalVar zero (defComp γ' t') as
         evalₖ γ t as ≡ evalₖ γ' t' as
         OK by t~ k (wkₖ γ~)

       if defRec, defRec, we have (γ~ : γ ~(k+1) γ' , t~ : t ~(k + 1) t' )
       goal:
         evalₖ (rec γ t) t as ≡ evalₖ (rec γ' t') t' as
         OK by t~ k (rec γ t) (rec γ' t') (wkₖ γ~, wkₖ t~)

   case (let t u) at (ℂ (A → B)) type:

     ∀ (k ≤ n) γ γ' (γ~ : γ ~ₖ γ') as. evalₖ u (defComp γ t) as ≡ evalₖ u (defComp γ' t) as

     by IH, we have t ~(n+1) t  and u ~(n+1) u, so also t ~ₖ t and u ~ₖ u by restriction

     (u ~ₖ u) (defComp γ t, defComp γ' t) (γ~, wk (t ~ₙ t)) as OK

   case (let t u) at (V A) type:

     ∀ (k ≤ n) γ γ' (γ~ : γ ~ₖ γ') as
       (do v ← evalₖ t γ []; evalₖ (defVal γ v) t as)
       ≡
       (do v ← evalₖ t γ' []; evalₖ (defVal γ' v) t as)

     I get from IH that t ~ₙ t, hence t ~ₖ t, hence evalₖ t γ [] ≡ evalₖ t γ' []
     then match on evalₖ t γ []
     then dispatch u ~ₖ u
     OK

   case (letrec t u) at (ℂ A) type

     ∀ (k ≤ n) γ γ' (γ~ : γ ~ₖ γ') as. evalₖ u (defRec γ t) as ≡ evalₖ u (defRec γ' t) as

   case (app t u)

     ∀ (k ≤ n) γ γ' (γ~ : γ ~ₖ γ') as.
       (do v ← evalₖ u γ []; evalₖ t γ (app as v))
       ≡
       (do v ← evalₖ u γ' []; evalₖ t γ' (app as v))

     evalₖ u γ [] ≡ evalₖ u γ' [] by IHu
     then just use IHt, args can be instantiated to exactly the same value
     OK

   TODO REST
   It looks like we only need case distinction on top-level n, no full induction


SYMMETRY for _~_

  ∀ n t t'. t ~ₙ t' → t' ~ₙ t

  case n=0 OK
  case n=suc n
     p : t ~(suc ₙ) t'
     k ≤ n
     γ γ' (γ~ : γ ~ₖ γ') as
     ----------------------
     evalₖ γ t' as ≡ evalₖ γ' t as

     (wkₖ p)⁻¹ : t' ~ₖ t         (_⁻¹ from strong induction on n)
     (wkₖ p)⁻¹ γ γ' γ~ as OK


TRANSITIVITY for _~_

  ∀ n t t' t''. t ~ₙ t' → t' ~ₙ t'' → t ~ₙ t''

  case n=0 OK
  case n=suc n
     p : t ~(suc ₙ) t'
     q : t' ~(suc ₙ) t''
     k ≤ n
     γ γ' (γ~ : γ ~ₖ γ') as
     ----------------------
     evalₖ γ t as ≡ evalₖ γ' t'' as

     (wkₖ p) ◼ (wkₖ q) : t ~ₖ t''         -- strong induction
     ((wkₖ p) ◼ (wkₖ q)) γ γ' γ~ as OK


EQUIVALENCE for variables is just equality

FULL EQUIVALENCE:
  t ~ t' := ∀n. t ~ₙ t'
  γ ~ γ' := ∀n. γ ~ₙ γ'

CONGRUENCE for _~_:

  case VAR: x = x' → x ~ x  OK by reflexivity

  case (let t u) :
     t~ : t ~ t'
     u~ : u ~ u'
     n=suc n
     goal : let t u ~ₙ let t' u'






-}
