

module ObjectLang3 where

{- Large product, large unit, letrec -}

open import Relation.Binary.PropositionalEquality
  renaming (subst to tr; sym to infix 5 _⁻¹; trans to infixr 4 _■_;
            cong to ap)
  hiding ([_])

open import Data.Product
  renaming (proj₁ to ₁; proj₂ to ₂)
  hiding (_<*>_)

open import Data.Maybe
open import Data.Nat hiding (_+_; _*_)
open import Function

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
    Close : CompTy → ValTy

  data Functor : Set where
    K   : ValTy → Functor
    Id  : Functor
    _+_ : Functor → Functor → Functor
    _*_ : Functor → Functor → Functor

  data CompTy : Set where
    one : CompTy
    _*_ : CompTy → CompTy → CompTy
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

  close  : Tm Γ (ℂ α) → Tm Γ (V (Close α))
  open'  : Tm Γ (V (Close α)) → Tm Γ (ℂ α)
  Pair   : Tm Γ (ℂ α) → Tm Γ (ℂ β) → Tm Γ (ℂ (α * β))
  Fst    : Tm Γ (ℂ (α * β)) → Tm Γ (ℂ α)
  Snd    : Tm Γ (ℂ (α * β)) → Tm Γ (ℂ β)
  Tt     : Tm Γ (ℂ one)


-- Fuel-based operational semantics
--------------------------------------------------------------------------------

mutual
  data Val : ValTy → Set where
    tt     : Val one
    pair   : Val a → Val b → Val (a * b)
    inl    : Val a → Val (a + b)
    inr    : Val b → Val (a + b)
    wrap   : Val (⟦ F ⟧ (μ F)) → Val (μ F)

    close  : Env Γ → Tm Γ (ℂ α) → Val (Close α)

  data Env : Con → Set where
    nil       : Env ∙
    defVal    : Env Γ → Val a → Env (Γ ▶ V a)
    defComp   : Env Γ → Tm Γ (ℂ α) → Env (Γ ▶ ℂ α)
    defRec    : Env Γ → Tm (Γ ▶ ℂ α) (ℂ α) → Env (Γ ▶ ℂ α)

data Args : Ty → ValTy → Set where
  []   : Args (V a) a
  app  : Args B c → Val a → Args (ℂ (a ⇒ B)) c


  fst  : Args (ℂ α) c → Args (ℂ (α * β)) c
  snd  : Args (ℂ β) c → Args (ℂ (α * β)) c

mutual
  evalVar : Var Γ A → ℕ → Env Γ → ∀ {B} → Args A B → Maybe (Val B)
  evalVar zero    k (defVal env v)  [] = return v
  evalVar zero    k (defComp env t) sp = eval t k env sp
  evalVar zero    k (defRec env t)  sp = eval t k (defRec env t) sp  -- here we consume 1 recursion
  evalVar (suc x) k (defVal env _)  sp = evalVar x k env sp
  evalVar (suc x) k (defComp env _) sp = evalVar x k env sp
  evalVar (suc x) k (defRec env _)  sp = evalVar x k env sp

  bindLet : Tm Γ A → ℕ → Env Γ → Maybe (Env (Γ ▶ A))
  bindLet {A = V _} t k env = defVal env <$> eval t k env []
  bindLet {A = ℂ _} t k env = return (defComp env t)

  evalSplit : ∀{d}(l : Tm (Γ ▶ V a) C)(r : Tm (Γ ▶ V b) C) → Val (a + b) → ℕ → Env Γ → Args C d → Maybe (Val d)
  evalSplit l r (inl v) k e args = eval l k (defVal e v) args
  evalSplit l r (inr v) k e args = eval r k (defVal e v) args

  eval : Tm Γ A → ℕ → Env Γ → Args A b → Maybe (Val b)
  eval t             zero    env args       = nothing
  eval (var x)       (suc k) env args       = evalVar x (k) env args
  eval (let' t u)    (suc k) env args       = do env ← bindLet t (suc k) env
                                                 eval u k env args
  eval (letrec t u)  (suc k) env args       = eval u (suc k) (defRec env t) args
  eval (lam t)       (suc k) env (app as v) = eval t (suc k) (defVal env v) as
  eval (app t u)     (suc k) env args       = do v ← eval u (suc k) env []
                                                 eval t (suc k) env (app args v)
  eval (pair t u)    (suc k) env []         = pair <$> eval t (suc k) env [] <*> eval u (suc k) env []
  eval (fst t)       (suc k) env []         = do pair v _ ← eval t (suc k) env []
                                                 return v
  eval (snd t)       (suc k) env []         = do pair _ v ← eval t (suc k) env []
                                                 return v
  eval tt            (suc k) env []         = return tt
  eval (inl t)       (suc k) env []         = inl <$> eval t (suc k) env []
  eval (inr t)       (suc k) env []         = inr <$> eval t (suc k) env []
  eval (split t l r) (suc k) env args       = do v ← eval t (suc k) env []
                                                 evalSplit l r v (suc k) env args

  eval (wrap t)      (suc k) env []         = wrap <$> eval t (suc k) env []
  eval (unwrap t)    (suc k) env []         = do wrap v ← eval t (suc k) env []
                                                 return v

  eval (close t)     (suc k) env []         = return (close env t)

  eval (open' t)     (suc k) env args       = do close env t ← eval t (suc k) env []
                                                 eval t k env args
  eval Tt            (suc k) env ()
  eval (Pair t u)    (suc k) env (fst args) = eval t (suc k) env args
  eval (Pair t u)    (suc k) env (snd args) = eval u (suc k) env args
  eval (Fst t)       (suc k) env args       = eval t (suc k) env (fst args)
  eval (Snd t)       (suc k) env args       = eval t (suc k) env (snd args)

--------------------------------------------------------------------------------

{-

In the paper, we don't present computational products in the beginning, only
when we get to mutual letrec in pull streams.

We also skip closure types.


- Beta rule is for "open values" (as in CBV), for us vars should be enough for saturation translation.

- Idea: formalize renamings, show renaming lemma, use it for var-beta function rule!

- MONOTONICITY : if eval t k env args = just x, then eval t (suc k) env args = just x
  corollary: uniqueness of result value:
     if eval t k env args = just v and eval t k' env args = just v', then v = v'

- REDUCTION:
  for f : ℕ → Maybe Val
      f ~>ₖ v := (∃ (k' < k). f k = just v)              f ~>ₖ v  implies f ~>ₖ' v for k' >
      f ~> v  := ∃ k. f ~>ₖ v


- DIVERGENCE:
  for f : ℕ → Maybe Val
      f diverges := ∀ k. f k = nothing


- TERM EQUIVALENCE, step indexed

  ≈ₖ : Tm Γ A → Tm Γ A → Prop
  t ≈0       t' = ⊤
  t ≈(suc k) t' = ∀ e e' args (e≈ : e ≈ₖ e'). (∃ v. ⟦t⟧ e args ~> v × ⟦t'⟧ e' args ~> v)
                                            ∨ (⟦t⟧ e args diverges × ⟦t'⟧ e' args diverges)

  ≈ₖ : Env Γ → Env Γ → Prop
  nil           ≈ₖ nil             = ⊤
  defVal env v  ≈ₖ defVal env' v'  = env ≈ₖ env' × v = v'
  defComp env t ≈ₖ defComp env' t' = env ≈ₖ env' × t ≈ₖ t'
  defRec env t  ≈ₖ defRec env' t'  = env ≈ₖ env' × t ≈ₖ t'

Reflexive:

  t ≈₀ t : ⊤  OK
  t ≈(suc k) t e e' args (e≈ : e ≈ₖ e').
            (∃ v. ⟦t⟧ e args ~> v × ⟦t⟧ e' args ~> v)
          ∨ (⟦t⟧ e args diverges × ⟦t⟧ e' args diverges)

    by induction hyp:


  -- Sym: OK
  -- Trans: OK

- VAR EQUIVALENCE is simply strict equality



congruences:

  VAR
    x = x'  →    var x ≈ var x'
    holds by reflexivity

  LET COMP
     (t≈ : t ≈ t')
     (u≈ : u ≈ u')
     (k : ℕ)
     goal : let t u ≈ₖ let t' u'

     induction k:
       case 0: OK
       case suc k
         hyp : let t u ≈ₖ let t' u'
         env env' args (env ≈ₖ env')
         goal :
              (eval (let t u) _ env args, eval (let t' u') _ env' args ~> v)
           ∨  (both diverge)

              (eval u _ (defComp env t) args, eval u' _ (defComp env' t') args ~> v)
           ∨  (both diverge)

           t≈ k : t ≈ₖ t'

           u≈ k (defComp env t) (defComp env' t') args (env ≈ₖ env') OK    looks fairly good actually...

  JUST DO THIS FOR EVERYTHING

















- TERM EQUIVALENCE
  t ≈ u   if   ∀ env args. (∃ k k' v. (eval t k env args = just v) × (eval u k' env args = just v))
                         ∨ (∀ k. eval t k env args = nothing × eval u k env args = nothing)
- _≈_ is an equivalence relation

    reflexive: t ≈ t is evident

    symmetric: evident

    transitive:
       assume t ≈ u , u ≈ v
       env args

       4 cases:
         if (t ~> x <~ u) and (u ~> x' <~ v), then x = x', so t ~> x <~ v OK
         if t,u diverge and (u ~> x' <~ v), impossible
         if (t ~> x <~ u) and u,v diverge, impossible
         if t, u and u, v diverge, then t,v diverge, so OK

- VAR EQUIVALENCE
  - same as term equivalence, except with varEval
  - is an equiv relation for the same reasons


- _≈_ IS A CONGRUENCE

  var:
    x ≈ y → var x ≈ var y          OK

  let of value type:

    t, t' : Tm Γ (V a)
    u, u' : Tm (Γ ▶ V a) B

    p : t ≈ t', p' : u ≈ u'
    env, args

    case p env [] of
      left (t, t' ~> v) -> case q (defEnv env v) args of
        left (u, u' ~> v') -> left OK
        right -> right OK
      right (t, t' diverge)
        -> right OK

    OK

  let of comp type:

    t, t' : Tm Γ A
    u, u' : Tm (Γ ▶ A) B

    p : t ≈ t', q : u ≈ u'
    env, args

    need to generalize ≈ so that we have equivalent env inputs!!



-}
