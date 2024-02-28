a
{-
Formalization of the object language. It is helpful to view modules in this order:
-}

open import Lib
open import Syntax
open import Interpreter
open import Renaming
open import Bisimilarity
open import Bisimilarities

{-
We don't formalize call saturation. We do formalize all the βη-rules and
commutation rules in "Bisimilarities" that would be used in call saturation. We
give an informal definition here.

Definition: a *saturated call* is of the form

    x e₁ e₂ ... eₙ

- where "x" is a variable, and eᵢ is either an application of a term or
  a (postfix) computational .Fst or .Snd ,

- moreover, the let-definition of "x" is eta-long, i.e. has all λ-s and computational
  pairs on the outside.

We want to translate terms so that they only contain saturated calls. We do it as follows.

1. Eta-expand all let-definitions of computations.
2. In all "t e₁ e₂ ... eₙ" where "t" is not a variable, eta-expand "t".
3. Perform the following rewrites until fixpoint. We write .Proj for .Fst or .Snd .

   - t u                                  ~> let x := u; t x                          where u is not a variable
   - (λ x. t) y                           ~> t[x↦y]                                   where y is a variable
   - (let x := t; u) y                    ~> let x := t; u y                          where y is a variable
   - (case t of inl x. l; inr y. r) z     ~> case t of inl x. l z; inr y. r z         where z is a variable
   - (t, u) .Fst                          ~> t
   - (t, u) .Snd                          ~> u
   - (let x := t; u) .Proj                ~> let x := t; u .Proj
   - (case t of inl x. l; inr y. r) .Proj ~> case t of inl x. (l .Proj) ; inr y. (r .Proj)

This does a bit more than call saturation, it also sequences all function
applications in an ANF-like fashion.
-}
