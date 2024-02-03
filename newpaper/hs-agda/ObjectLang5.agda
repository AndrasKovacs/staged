
module ObjectLang5 where

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
        t~ (wk γ~ , t~) as OK

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




-}
