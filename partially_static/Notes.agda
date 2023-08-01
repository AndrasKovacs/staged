
module Notes where

open import Relation.Binary.PropositionalEquality
  renaming (sym to infix 6 ⁻¹; trans to infixr 5 _◼_; subst to tr; cong to ap)
open import Data.Product
open import Data.Bool
open import Relation.Nullary

module _ (A B : Set)
         (A+ : A → A → A)
         (A-ass : ∀ x y z → A+ (A+ x y) z ≡ A+ x (A+ y z))
         (B+ : B → B → B)
         (B-ass : ∀ x y z → B+ (B+ x y) z ≡ B+ x (B+ y z))
         where

  choose : Bool → Set
  choose false = A
  choose true  = B

  data AList' : Bool → Set where
    single  : ∀ {b} → choose b → AList' b
    cons    : ∀ {b} → choose b → AList' (not b) → AList' b

  AList = ∃ AList'

  cons' : ∀ b → choose b → AList → AList
  cons' false x (false , single y  ) = _ , single (A+ x y)
  cons' false x (false , cons y ys ) = _ , cons (A+ x y) ys
  cons' false x (true  , xs        ) = _ , cons x xs
  cons' true  x (false , xs        ) = _ , cons x xs
  cons' true  x (true  , single y  ) = _ , single (B+ x y)
  cons' true  x (true  , cons y ys ) = _ , cons (B+ x y) ys

  infixr 5 _++_
  {-# TERMINATING #-}
  _++_ : AList → AList → AList
  (b , single x)  ++ ys = cons' _ x ys
  (b , cons x xs) ++ ys = cons' _ x ((_ , xs) ++ ys)

  lem : ∀ b x xs ys → cons' b x (xs ++ ys) ≡ cons' b x xs ++ ys
  lem false x (false , single y)  ys = {!!}
  lem false x (false , cons y ys) zs = {!!}
  lem false x (true , xs)  ys = refl
  lem true x  (false , xs) ys = refl
  lem true x  (true , xs)  ys = {!!}

  ass : ∀ xs ys zs → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
  ass (b , single x)  (b' , ys) (b'' , zs) = {!!}
  ass (b , cons x xs) (b' , ys) (b'' , zs) = {!!}
