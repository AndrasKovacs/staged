
module Translate where

open import Relation.Binary.PropositionalEquality
  renaming (subst to tr; sym to infix 5 _⁻¹; trans to infixr 4 _■_;
            cong to ap)
  hiding ([_])

open import Data.Product
  renaming (proj₁ to ₁; proj₂ to ₂)

open import Data.Bool
open import Data.Unit
open import Data.List

data ValTy : Set where
  bool : ValTy
  unit : ValTy
  pair : ValTy → ValTy → ValTy
  list : ValTy → ValTy

data Ty : Set where
  fun : ValTy → Ty → Ty
  val : ValTy → Ty

data Con : Set where
  ∙ : Con
  _▶_ : Con → Ty → Con
infixl 3 _▶_

data Var : Con → Ty → Set where
  vz : ∀ {Γ A} → Var (Γ ▶ A) A
  vs : ∀ {Γ A B} → Var Γ A → Var (Γ ▶ B) A

data Tm (Γ : Con) : Ty → Set where
  var    : ∀ {A} → Var Γ A → Tm Γ A
  letrec : ∀ {A B} → Tm (Γ ▶ A) A → Tm (Γ ▶ A) B → Tm Γ B

  true false : Tm Γ (val bool)
  ite : ∀ {A} → Tm Γ (val bool) → Tm Γ A → Tm Γ A → Tm Γ A

  tt : Tm Γ (val unit)

  pair : ∀ {A B} → Tm Γ (val A) → Tm Γ (val B) → Tm Γ (val (pair A B))
  fst : ∀ {A B} → Tm Γ (val (pair A B)) → Tm Γ (val A)
  snd : ∀ {A B} → Tm Γ (val (pair A B)) → Tm Γ (val B)

  nil : ∀ {A} → Tm Γ (val (list A))
  cons : ∀ {A} → Tm Γ (val A) → Tm Γ (val (list A)) → Tm Γ (val (list A))
  case : ∀ {A B} → Tm Γ (val (list A)) → Tm Γ B → Tm (Γ ▶ val A ▶ val (list A)) B
                 → Tm Γ B

  lam : ∀ {A B} → Tm (Γ ▶ val A) B → Tm Γ (fun A B)
  app : ∀ {A B} → Tm Γ (fun A B) → Tm Γ (val A) → Tm Γ B

data Nf (Γ : Con) : Ty → Set where
  var    : ∀ {A} → Var Γ A → Nf Γ A
  letrec : ∀ {A B} → Nf (Γ ▶ A) A → Nf (Γ ▶ A) (val B) → Nf Γ (val B)

  true false : Nf Γ (val bool)
  ite : ∀ {A} → Nf Γ (val bool) → Nf Γ (val A) → Nf Γ (val A) → Nf Γ (val A)

  tt : Nf Γ (val unit)

  pair : ∀ {A B} → Nf Γ (val A) → Nf Γ (val B) → Nf Γ (val (pair A B))
  fst : ∀ {A B} → Nf Γ (val (pair A B)) → Nf Γ (val A)
  snd : ∀ {A B} → Nf Γ (val (pair A B)) → Nf Γ (val B)

  nil : ∀ {A} → Nf Γ (val (list A))
  cons : ∀ {A} → Nf Γ (val A) → Nf Γ (val (list A)) → Nf Γ (val (list A))
  case : ∀ {A B} → Nf Γ (val (list A)) → Nf Γ (val B)
                 → Nf (Γ ▶ val A ▶ val (list A)) (val B)
                 → Nf Γ (val B)

  lam : ∀ {A B} → Nf (Γ ▶ val A) B → Nf Γ (fun A B)
  app : ∀ {A B} → Nf Γ (fun A B) → Nf Γ (val A) → Nf Γ B

-- ite (A → B) b t f ~> λ x. ite B b (t x) (f x)
-- case (

-- λ x. case x
--   Nil -> λ y. n
--   Cons x xs -> λ y. ...
-- exp

-- Functions: type-directed eta-expansion + case commutation
-- Haskell sketch
-- eval (A → B) t = VLam x

{-
Haskell: type-informed quote?

eval
quote
unquote

-- no eta at value types

eval γ x = lookup x γ

eval γ (letrec x:A = t; u) =

  VLetrec (eval (γ,unquote (freshVar A)) t) (λ

eval γ (λ x. t) = λ x. eval (γ, x) t





-}

--------------------------------------------------------------------------------
