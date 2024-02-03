
module ObjectInterpreter where

open import Relation.Binary.PropositionalEquality
  renaming (subst to tr; sym to infix 5 _⁻¹; trans to infixr 4 _■_;
            cong to ap)
  hiding ([_])

open import Data.Product
  renaming (proj₁ to ₁; proj₂ to ₂)

open import Function
open import Data.Unit

mutual
  data ValTy : Set where
    one : ValTy
    _+_ : ValTy → ValTy → ValTy
    _*_ : ValTy → ValTy → ValTy
    μ   : Functor → ValTy

  data Functor : Set where
    K   : ValTy → Functor
    Id  : Functor
    _+_ : Functor → Functor → Functor
    _*_ : Functor → Functor → Functor

infixr 5 _+_
infixr 6 _*_

⟦_⟧ : Functor → ValTy → ValTy
⟦ K A   ⟧ _ = A
⟦ Id    ⟧ A = A
⟦ F + G ⟧ A = ⟦ F ⟧ A + ⟦ G ⟧ A
⟦ F * G ⟧ A = ⟦ F ⟧ A * ⟦ G ⟧ A

data Ty : Set where
  val : ValTy → Ty
  _⇒_ : ValTy → Ty → Ty
infixr 4 _⇒_

data Con : Set where
  ∙   : Con
  _▶_ : Con → Ty → Con
infixl 3 _▶_

data Var : Con → Ty → Set where
  zero : ∀ {Γ A} → Var (Γ ▶ A) A
  suc  : ∀ {Γ A B} → Var Γ A → Var (Γ ▶ B) A

data Tm (Γ : Con) : Ty → Set where
  var    : ∀ {A} → Var Γ A → Tm Γ A
  let'   : ∀ {A B} → Tm Γ A → Tm (Γ ▶ A) B → Tm Γ B
  fix    : ∀ {A B} → Tm (Γ ▶ (A ⇒ B)) (A ⇒ B) → Tm Γ (A ⇒ B)
  lam    : ∀ {A B} → Tm (Γ ▶ val A) B → Tm Γ (A ⇒ B)
  app    : ∀ {A B} → Tm Γ (A ⇒ B) → Tm Γ (val A) → Tm Γ B
  pair   : ∀ {A B} → Tm Γ (val A) → Tm Γ (val B) → Tm Γ (val (A * B))
  fst    : ∀ {A B} → Tm Γ (val (A * B)) → Tm Γ (val A)
  snd    : ∀ {A B} → Tm Γ (val (A * B)) → Tm Γ (val B)
  tt     : Tm Γ (val one)
  inl    : ∀ {A B} → Tm Γ (val A) → Tm Γ (val (A + B))
  inr    : ∀ {A B} → Tm Γ (val B) → Tm Γ (val (A + B))
  split  : ∀ {A B C} → Tm Γ (val (A + B)) → Tm (Γ ▶ val A) C → Tm (Γ ▶ val B) C → Tm Γ C
  wrap   : ∀ {F} → Tm Γ (val (⟦ F ⟧ (μ F))) → Tm Γ (val (μ F))
  unwrap : ∀ {F} → Tm Γ (val (μ F)) → Tm Γ (val (⟦ F ⟧ (μ F)))

--------------------------------------------------------------------------------

data Val : ValTy → Set where
  tt     : Val one
  pair   : ∀ {A B} → Val A → Val B → Val (A * B)
  inl    : ∀ {A B} → Val A → Val (A + B)
  inr    : ∀ {A B} → Val B → Val (A + B)
  wrap   : ∀ {F} → Val (⟦ F ⟧ (μ F)) → Val (μ F)

data Env : Con → Set where
  nil    : Env ∙
  defVal : ∀ {Γ A} → Env Γ → Val A → Env (Γ ▶ val A)
  defFun : ∀ {Γ A B} → Env Γ → Tm Γ (A ⇒ B) → Env (Γ ▶ (A ⇒ B))

data Spine : Ty → ValTy → Set where
  []   : ∀ {A} → Spine (val A) A
  _∷_  : ∀ {A B C} → Val A → Spine B C → Spine (A ⇒ B) C
infixr 4 _∷_

mutual
  evalVar : ∀ {Γ A} → Var Γ A → (∀ {B} → Env Γ → Spine A B → Val B)
  evalVar zero    (defVal env v) [] = v
  evalVar zero    (defFun env t) sp = eval t env sp
  evalVar (suc x) (defVal env _) sp = evalVar x env sp
  evalVar (suc x) (defFun env _) sp = evalVar x env sp

  extEnv : ∀ {Γ A} → Env Γ → Tm Γ A → Env (Γ ▶ A)
  extEnv {A = val _} env t = defVal env (eval t env [])
  extEnv {A = _ ⇒ _} env t = defFun env t

  {-# NON_TERMINATING #-}
  eval : ∀ {Γ A} → Tm Γ A → (Env Γ → ∀ {B} → Spine A B → Val B)
  eval (var x)       env sp       = evalVar x env sp
  eval (let' t u)    env sp       = eval u (extEnv env t) sp
  eval (fix t)       env sp       = eval t (defFun env (fix t)) sp
  eval (lam t)       env (v ∷ sp) = eval t (defVal env v) sp
  eval (app t u)     env sp       = eval t env (eval u env [] ∷ sp)
  eval (pair t u)    env []       = pair (eval t env []) (eval u env [])
  eval (fst t)       env []       = case (eval t env []) of λ {(pair v u) → v}
  eval (snd t)       env []       = case (eval t env []) of λ {(pair v u) → u}
  eval tt            env []       = tt
  eval (inl t)       env []       = inl (eval t env [])
  eval (inr t)       env []       = inr (eval t env [])
  eval (split t l r) env sp       = case (eval t env []) of λ {
                                      (inl v) → eval l (defVal env v) sp;
                                      (inr v) → eval r (defVal env v) sp}
  eval (wrap t)      env []       = wrap (eval t env [])
  eval (unwrap t)    env []       = case (eval t env []) of λ {(wrap v) → v}

--------------------------------------------------------------------------------

{-
- handwawy version: show that eta + case commutation is valid, argue that
  they can be used to implement saturation translation

- less handwave: prove here that eta + case commutation is valid
  weakening is needed for this

TODO maybe: define saturation translation as an "open" NbE version of the eval above.
-}

data Wk : Con → Con → Set where
  wkid : ∀ {Γ} → Wk Γ Γ
  drop : ∀ {A Γ Δ} → Wk Γ Δ → Wk (Γ ▶ A) Δ
  keep : ∀ {A Γ Δ} → Wk Γ Δ → Wk (Γ ▶ A) (Δ ▶ A)

wkVar : ∀ {Γ Δ A} → Var Δ A → Wk Γ Δ → Var Γ A
wkVar x       wkid     = x
wkVar x       (drop σ) = suc (wkVar x σ)
wkVar zero    (keep σ) = zero
wkVar (suc x) (keep σ) = suc (wkVar x σ)

wk : ∀ {Γ Δ A} → Tm Δ A → Wk Γ Δ → Tm Γ A
wk (var x)       σ = var (wkVar x σ)
wk (let' t u)    σ = let' (wk t σ) (wk u (keep σ))
wk (fix t)       σ = fix (wk t (keep σ))
wk (lam t)       σ = lam (wk t (keep σ))
wk (app t u)     σ = app (wk t σ) (wk u σ)
wk (pair t u)    σ = pair (wk t σ) (wk u σ)
wk (fst t)       σ = fst (wk t σ)
wk (snd t)       σ = snd (wk t σ)
wk tt            σ = tt
wk (inl t)       σ = inl (wk t σ)
wk (inr t)       σ = inr (wk t σ)
wk (split t u v) σ = split (wk t σ) (wk u (keep σ)) (wk v (keep σ))
wk (wrap t)      σ = wrap (wk t σ)
wk (unwrap t)    σ = unwrap (wk t σ)

-- η : ∀ {Γ A B}(t : Tm Γ (A ⇒ B)) → eval t ≡ eval (lam (app (wk t (drop wkid)) (var zero)))
-- η = {!!}
{-

eval (lam (app (wk t (drop wkid)) (var zero)))

  λ env sp. eval (app (wk t (drop wkid)) (var zero)) (defVal env (hd sp)) (tl sp)

    t = (wk t (drop wkid))
    u = var zero
    env = (defVal env (hd sp))
    sp = (tl sp)

  eval (wk t (drop wkid)) (defVal env (hd sp)) (eval (var zero) (defVal env (hd sp)) [] ∷ (tl sp))
  eval (wk t (drop wkid)) (defVal env (hd sp)) (hd sp ∷ tl sp)

  eval (wk t (drop wkid)) (defVal env (hd sp)) sp
  eval t (evalWk (drop wkid) (defVal env (hd sp)) sp
  eval t env sp OK
-}

-- foo : ∀ {Γ A B C D}(t : Tm Γ (val (A + B)))(l : Tm (Γ ▶ val A) (C ⇒ D))
--                                            (r : Tm (Γ ▶ val B) (C ⇒ D))
--                                            (u : Tm Γ (val C))
--       → eval (app (split t l r) u)
--       ≡ eval (split t (app l (wk u (drop wkid))) (app r (wk u (drop wkid))))
-- foo t l r u = {!!}

wkAble : ∀ {Γ Δ} → Wk Γ Δ → Env Γ → Set
wkAble wkid     e            = ⊤
wkAble (drop σ) (defVal e x) = wkAble σ e
wkAble (drop σ) (defFun e x) = wkAble σ e
wkAble (keep σ) (defVal e x) = wkAble σ e
wkAble (keep σ) (defFun e x) = wkAble σ e × (∃ λ x' → x ≡ wk x' σ)

evalWk : ∀ {Γ Δ} → (σ : Wk Γ Δ) → (e : Env Γ) → wkAble σ e → Env Δ
evalWk wkid     γ            p               = γ
evalWk (drop σ) (defVal γ x) p               = evalWk σ γ p
evalWk (drop σ) (defFun γ x) p               = evalWk σ γ p
evalWk (keep σ) (defVal γ x) p               = defVal (evalWk σ γ p) x
evalWk (keep σ) (defFun γ x) (p , x' , refl) = defFun (evalWk σ γ p) x'

------

lem1 : ∀ {Γ Δ A}(σ : Wk Γ Δ)(env : Env Γ)(x : Var Δ A) {B} (sp : Spine A B) (p : wkAble σ env)
       → evalVar (wkVar x σ) env sp ≡ evalVar x (evalWk σ env p) sp
lem1 wkid     env x sp p = refl
lem1 (drop σ) (defVal env x₁) x sp p = {!!} -- OK
lem1 (drop σ) (defFun env x₁) x sp p = {!!} -- OK
lem1 (keep σ) (defVal env x) zero sp p = {!!}  -- OK
lem1 (keep σ) (defFun env x) zero sp (p , x' , refl) = {!!} -- OK
lem1 (keep σ) (defVal env x₁) (suc x) sp p = {!!} -- OK
lem1 (keep σ) (defFun env x₁) (suc x) sp (p , x' , refl) = {!!} -- OK

{-
To show:
  given  p : wkAble σ env
  eval (wk t σ) env sp = eval t (evalWk σ env) sp

INDUCTION on t:

CASE
var x -- OK, see lem1

CASE let

    eval (wk (let t u) σ) env sp
  = eval (let' (wk t σ) (wk u (keep σ))) env sp
  = eval (wk u (keep σ)) (extEnv env (wk t σ)) sp

    if val let:
        = eval (wk u (keep σ)) (defVal env (eval t (wk σ env) [])) sp
        = eval u (evalWk (keep σ) (defVal env (eval t (wk σ env) []))) sp
        = eval u (defVal (evalWk σ env) (eval t (wk σ env) [])) sp

    if fun let:
        eval (wk u (keep σ)) (defFun env (wk t σ)) sp
        eval u (evalWk (keep σ) (defFun env (wk t σ))) sp   -- using t as wkAble witness
        eval u (defFun (evalWk σ env) t) sp

  RHS:
  eval u (extEnv (evalWk σ env) t) sp

    if val let:
        eval u (defVal (evalWk σ env) (eval t (evalWk σ env) [])) sp OK

    if fun let:
       eval u (defFun (evalWk σ env) t) sp  -- OK


CASE fix
  eval (wk t σ) env sp = eval t (evalWk σ env) sp

  LHS:
       eval (wk (fix t) σ) env sp
     = eval (fix (t[keep σ])) env sp
     = eval (t[keep σ]) (defFun env (fix (t[keep σ]))) sp  -- using (fix t as wkAble witness)
     = eval t (evalWk (keep σ) (defFun env (fix (t[keep σ]))) sp -- using (fix t as wkAble witness)
     = eval t (defFun (evalWk σ env) (fix t)) sp

  RHS:
    eval t (defFun (evalWk σ env) (fix t)) sp
    OK

CASE lam
  eval ((lam t)[σ]) env (v :: sp) = eval (lam t) (evalWk σ env) (v :: sp)

  LHS
      eval ((lam t)[σ]) env (v ∷ sp)
    = eval (lam (t[keep σ])) env (v ∷ sp)
    = eval (t[keep σ]) (defVal env v) sp
    = eval t (evalWk (keep σ) (defVal env v)) sp
    = eval t (defVal (evalWk σ env) v) sp

  RHS
      eval (lam t) (evalWk σ env) (v :: sp)
   = eval t (defVal (evalWk σ env) v) sp OK

CASE app

  eval (wk (app t u) σ) env sp = eval (app t u) (evalWk σ env) sp

  LHS
     eval ((app t u)[σ]) env sp
     eval (app t[σ] u[σ]) env sp
     eval t[σ] env (eval u[σ] env [] ∷ sp)
     eval t (evalWk σ env) (eval u[σ] env [] ∷ sp)
     eval t (evalWk σ env) (eval u (evalWk σ env) [] ∷ sp)

  RHS
     eval (app t u) (evalWk σ env) sp
     eval t (evalWk σ env) (eval u (evalWk σ env) [] ∷ sp) OK

CASE pair

  eval (pair t u)[σ] env [] = eval (pair t u) (evalWk σ env) []

  LHS
    eval (pair t u)[σ] env []
    eval (pair t[σ] u[σ]) env []
    pair (eval t[σ] env []) (eval u[σ] env [])
    pair (eval t (evalWk σ env) []) (eval u (evalWk σ env) []) OK

CASE split
   eval (split t l r)[σ] env sp = eval (split t l r) (evalWk σ env) sp

   LHS

     eval (split t l r)[σ] env sp
     eval (split t[σ] l[keep σ] r[keep σ]) env sp

     case (eval t[σ] env []) of
       (inl v) → eval l[keep σ] (defVal env v) sp
       (inr v) → eval r[keep σ] (defVal env v) sp

     case (eval t (evalWk σ env) []) of
       (inl v) → eval l (defVal (evalWk σ env) v) sp
       (inr v) → eval r (defVal (evalWk σ env) v) sp

    RHS
    eval (split t l r) (evalWk σ env) sp OK

rest is the same

--------------------------------------------------------------------------------
  extEnv : ∀ {Γ A} → Env Γ → Tm Γ A → Env (Γ ▶ A)
  extEnv {A = val _} env t = defVal env (eval t env [])
  extEnv {A = _ ⇒ _} env t = defFun env t

  {-# NON_TERMINATING #-}
  eval : ∀ {Γ A} → Tm Γ A → (Env Γ → ∀ {B} → Spine A B → Val B)
  eval (var x)       env sp       = evalVar x env sp
  eval (let' t u)    env sp       = eval u (extEnv env t) sp
  eval (fix t)       env sp       = eval t (defFun env (fix t)) sp
  eval (lam t)       env (v ∷ sp) = eval t (defVal env v) sp
  eval (app t u)     env sp       = eval t env (eval u env [] ∷ sp)
  eval (pair t u)    env []       = pair (eval t env []) (eval u env [])
  eval (fst t)       env []       = case (eval t env []) of λ {(pair v u) → v}
  eval (snd t)       env []       = case (eval t env []) of λ {(pair v u) → u}
  eval tt            env []       = tt
  eval (inl t)       env []       = inl (eval t env [])
  eval (inr t)       env []       = inr (eval t env [])
  eval (split t l r) env sp       = case (eval t env []) of λ {
                                      (inl v) → eval l (defVal env v) sp;
                                      (inr v) → eval r (defVal env v) sp}
  eval (wrap t)      env []       = wrap (eval t env [])
  eval (unwrap t)    env []       = case (eval t env []) of λ {(wrap v) → v}
-}
