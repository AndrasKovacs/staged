
# Nameful notation for first-order embedded languages

Nameful notation for languages with binders is universally used. What about
notion for *embedded* languages? A fairly well-known approach is to have
higher-order abstract syntax (HOAS), and reuse metafunctions for
object-theoretic binders, and for metafunctions we already have the usual
nameful notation.

HOAS is a great idea when it works, but it's not always usable. Syntactic
translations which transform contexts in fancy ways are usually not expressible
in HOAS, such as:

- CPS translation, ANF translation
- Closure conversion
- Many optimizations, like dead code elimination
- Parametricity translations

More generally, many syntactic translations can be rephrased as "model
constructions", i.e. mapping models of one theory to models of another (possibly
the same) theory. *Categorical gluing* is a classic example.

When HOAS is not available, historically people have used the following approaches:

- Use a lower-level presentation of theories, with preterms, predicates and
  relations for typing and conversion and traditional inference rule notation.
  Thereafter, either have names as the formal notion of variable, or only use
  names in informal presentation. In either case, it's easy to get things wrong,
  and the amount of formal bureaucracy is rather bad.

- Use algebraic or categorical notions of models. Sometimes we do fine by using
  abstract nonsense, but when that's not available, we have to deal with
  horrible diagrams or horrible De Bruijn-infested terms, so again we are
  compelled to cook up a nameful notation.

My proposal is a nameful notation for theories in an algebraic style, which is
hopefully more rigorous than the handwaving that I've seen and practiced before.

It is also related to "term builders" in some type theory implementations, and
also to the Cocon metaprogramming language [TODO].

## Simply typed example

In Agda we can define a simply-typed syntax like:

    data Ty : Set where ...

    data Con : Set where
	  ∙   : Con
	  _,_ : Con → Ty → Con

	data Var : Con → Ty → Set where
	  z : Var (Γ , A) A
	  s : Var Γ A → Var (Γ , B) A

	data Tm (Γ : Con) : Ty → Set where
	  var : Var Γ A → Tm Γ A
	  app : Tm Γ (A ⇒ B) → Tm Γ A → Tm Γ B
	  lam : Tm (Γ , A) B → Tm Γ (A ⇒ B)

Then we define parallel substitutions:

    data Sub (Γ : Con) : Con → Set where
	  ε   : Sub Γ ∙
	  _,_ : Sub Γ Δ → Tm Γ A → Sub Γ (Δ , A)

Parallel substitutions have an action on terms satisfying a bunch of equations.
If we want to have a *complete* description of the equations and operations that
we want, a *simply typed category with families* (scwf) is a reasonable choice.
In Agda, we can define a conversion relation such that our flavor of
substitution, up to conversion, satisfies the scwf specification. Listing out
the scwf components:

    Con : Set
	Ty  : Set
	Tm  : Con → Ty → Set
	Sub : Con → Con → Set

	∙   : Con
	ε   : Sub Γ ∙
	_   : (σ : Sub Γ ∙) → σ = ε

	id  : Sub Γ Γ
	_∘_ : Sub Δ Ξ → Sub Γ Δ → Sub Γ Ξ
	_   : id ∘ σ = σ
	_   : σ ∘ id = σ
	_   : (σ ∘ δ) ∘ ν = σ ∘ (δ ∘ ν)

	_[_] : Tm Δ A → Sub Γ Δ → Tm Γ A
	_    : t[id]    = t
	_    : t[σ ∘ δ] = t[σ][δ]

	_,_ : Con → Ty → Set
	_,_ : Sub Γ Δ → Tm Γ A → Sub Γ (Δ, A)

	p   : Sub (Γ,A) Γ
	q   : Tm (Γ,A) A
	_   : p ∘ (σ, t) = σ
	_   : q [σ, t] = t
	_   : (p, q) = id
	_   : (σ, t) ∘ δ = (σ ∘ δ, t[δ])


Additionally, for each term former we should have an extra rule for pushing
substitution into it.

There's no separate sort for variables here. It would work fine if we had one,
but we can also just derive variables as:

    vz : Tm (Γ,A) A
	vz = q

	vs : Tm Γ A → Tm (Γ,B) A
	vs t = t[p]

Generally, `q[p∘p∘p∘...∘p]` is the N-th De Bruijn variable, where we have a composition
of N p-s. That's also the same as `q[p][p]...[p]` by the `t[σ∘δ] = t[σ][δ]` rule.
