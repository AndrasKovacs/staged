
# Partially Static Types and Partial Evaluators

## Introduction

In staged compilation, partially static types is an old and simple concept. At
the most basic level, they're just compile-time (or "current stage") data that
contains object-level (or "future stage") code.

The most common example is static lists containing object code. Let's use
two-level type theory notation, where `U0` is the universe of object types, or
"dynamic" types and `U1` is the universe of static types.

    data List1 (A : U1) : U1 where
	   nil  : List1 A
	   cons : A → List1 A → List1 A

This is simply the list type in `U1`. Naturally, we can store object code in lists.

    foo : List1 (↑Nat0)
	foo = cons <100> (cons <20 + 20> nil)

`Nat0 : U0` is the object-level type of natural numbers.

Partially static lists pop up very quickly in well-typed *staged
interpreters*. Staged interpreters take static syntax for embedded languages as
input, and produce object code as output, at compile time, thereby removing
interpreter overhead.

Let's look at simply-typed lambda calculus, the "hello world" of well-typed
interpreters.

    data Ty : U1 where
	  ι   : Ty
	  _⇒_ : Ty → Ty → Ty

    data Con : U1 where
	  ∙   : Con
	  _,_ : Con → Ty → Con

    data Var : Con → Ty → U1 where
	  zero : Var (Γ, A) A
	  suc  : Var Γ A → Var (Γ, B) A

    data Tm : Con → Ty → U1	where
	  var : Var Γ A → Tm Γ A
	  lam : Tm (Γ, A) B → Tm Γ (A ⇒ B)
	  app : Tm Γ (A ⇒ B) → Tm Γ A → Tm Γ B

We can write several different interpreters.

The **fully static** interpreter simply interprets syntax into the corresponding
type formers in `U1`. This is not staged; everything happens at compile time.

    ⟦_⟧ : Ty → U1
	⟦ι⟧     = ⊥₁
	⟦A ⇒ B⟧ = ⟦A⟧ → ⟦B⟧

	⟦_⟧ : Con → U1
	⟦∙    = ⊤₁
	⟦Γ,A⟧ = ⟦Γ⟧ × ⟦A⟧

	⟦_⟧ : Var Γ A → ⟦Γ⟧ → ⟦A⟧
	⟦zero⟧  (_, α) = α
	⟦suc v⟧ (γ, _) = ⟦v⟧ γ

	⟦_⟧ : Tm Γ A → ⟦Γ⟧ → ⟦A⟧
	⟦var v⟧   γ = ⟦v⟧ γ
	⟦lam t⟧   γ = λ α. ⟦t⟧(γ, α)
	⟦app t u⟧ γ = ⟦t⟧ γ (⟦u⟧ γ)

The classic **staged interpreter** interprets types and terms into the object
level, but interprets contexts as static (heterogeneous) lists:

    ⟦_⟧ : Ty → ↑U0
	⟦ι⟧     = <⊥₀>
	⟦A ⇒ B⟧ = <~⟦A⟧ → ~⟦B⟧>

	⟦_⟧ : Con → U1
	⟦∙    = ⊤₁
	⟦Γ,A⟧ = ⟦Γ⟧ × ↑~⟦A⟧

	⟦_⟧ : Var Γ A → ⟦Γ⟧ → ↑~⟦A⟧
	⟦zero⟧  (γ, α) = α
	⟦suc v⟧ (γ, α) = ⟦v⟧ γ

	⟦_⟧ : Tm Γ A → ↑~⟦Γ⟧ → ↑~⟦A⟧
	⟦var v⟧   γ = ⟦v⟧ γ
	⟦lam t⟧   γ = <λ α. ⟦t⟧ (γ, <α>)>
	⟦app t u⟧ γ = <~(⟦t⟧ γ) ~(⟦u⟧ γ)>

This is a bit noisy, but in a realistic implementation we can infer most of the
staging operations:

    ⟦_⟧ : Ty → ↑U0
	⟦ι⟧     = ⊥₀
	⟦A ⇒ B⟧ = ⟦A⟧ → ⟦B⟧

	⟦_⟧ : Con → U1
	⟦∙    = ⊤
	⟦Γ,A⟧ = ⟦Γ⟧ × ⟦A⟧

	⟦_⟧ : Var Γ A → ⟦Γ⟧ → ⟦A⟧
	⟦zero⟧  (γ, α) = α
	⟦suc v⟧ (γ, α) = ⟦v⟧ γ

	⟦_⟧ : Tm Γ A → ⟦Γ⟧ → ⟦A⟧
	⟦var v⟧   γ = ⟦v⟧ γ
	⟦lam t⟧   γ = λ α. ⟦t⟧ (γ, α)
	⟦app t u⟧ γ = ⟦t⟧ γ (⟦u⟧ γ)

Here, the typing contexts are interpreted as partially static heterogeneous
lists. The staged interpreter is essentially removing two kinds of overheads
from a non-staged version:

1. Environment lookups are performed at compile time.
2. Switching on syntactic constructors is performed at compile time.

For example, if I have

    t : Tm ∙ ((ι ⇒ ι) ⇒ ι ⇒ ι)
	t = lam (lam (app (var (suc zero)) (var zero)))

Then `⟦t⟧ : ⊤₁ → ↑((⊥₀ → ⊥₀) → ⊥₀ → ⊥₀)` computes to the following:

    λ _. <λ f x. f x>

It would be also possible to interpret environments as *dynamic* heterogeneous
lists

    ⟦_⟧ : Con → ↑U0
	⟦_⟧ : Tm Γ A → ↑~⟦Γ⟧ → ↑~⟦A⟧

but that doesn't make practical sense; it would perform variable lookups at
runtime. We'd compute `⟦t⟧ : ↑⊤₀ → ↑((⊥₀ → ⊥₀) → ⊥₀ → ⊥₀)` to a silly-looking

    λ γ. <λ f x. (snd (fst ((~γ, f), x))) (snd ((~γ, f), x))>





















--------------------------------------------------------------------------------
