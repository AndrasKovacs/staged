
# Yet another extension to higher-order pattern unification

## 1. Introduction

*Pattern unification* is the basis of inference in every major dependently typed
language (Agda, Coq, Lean, Idris). In the dependently typed setting,
metavariables can have function types, so it must be possible to solve them to
lambda expressions. This means that we have *higher-order unification* that
involves unknown functions, as opposed to the first-order unification that's
familiar from Hindley-Milner, where the unknown things are plain expression
trees without any computation rules.

Let's look at the basic pattern unification rule. I write `α, β, γ, δ`  for
metavariables, and `x, y, z, f, g, h` for bound variables. If we have the equation

    α spine =? rhs

where `spine` is an iterated application consisting of `t₁, t₂ ... tₙ`
arguments, the equation is solvable if the following conditions hold.

1. `spine` consists of distinct bound variables.
2. Every bound variable occurring `rhs` occurs in `spine`.
3. `α` does not occur in `rhs`.

In this case, the following is a definitionally unique solution:

    α := λ spine. rhs

where `λ spine` is an iterated abstraction of variables. Examples:

- `α x =? x` is solved by `α := λ x. x`
- `α x x =? x` fails condition 1, because variables in the spine are not distinct.
  Now `α := λ x _. x` and `α := λ _ x. x` are two different solutions.
- `α =? x` fails condition 2, there's no solution.
- `α =? suc α` fails condition 3, there's no solution.

There are many extensions and generalizations to the basic algorithm, which are
still decidable and still yield unique solutions.

- This [paper](https://www.cse.chalmers.se/~abela/unif-sigma-long.pdf) describes
  a bunch of extensions, most of which are available in Agda.
- My WITS 23 [abstract](https://andraskovacs.github.io/pdfs/wits23abstract.pdf)
  and [talk](https://andraskovacs.github.io/pdfs/wits23prez.pdf) describe an
  alternative to the above, which is more general and more efficient.

Now I describe yet another extension which is orthogonal to the above ones.  I
call it "spine reduction".

## 2. Spine reduction

Let's have `Bool`, and a simple case-splitting operation `case`, which
we write in postfix spine form. For example,

    f x .(case true false) .(case false true)

means the same as

    case (case (f x) of true → true; false → false) of true → false; false → true

Consider now

    α x .(case true true) =? β x .(case true true)

This clearly does not have a unique solution. We can set `α` to `λ _. true` or
`λ _. false`, and independently `β` as well, and in each case the equation holds.
What about

    α x .(case true true) =? x .(case true true)

Now I claim that there is a unique solution:

    α := λ x. x

To my knowledge, this kind of solution has not been considered in the
literature, nor is it implemented in practice. This works because an eliminator
that's blocked on a rigid variable in the scrutinee, is not so squishy and
wobbly anymore; it's definitionally injective in the other arguments.

Similarly, assume natural numbers and `_+_` defined by matching on the first
argument, and consider

    α x + y =? x + y

This again has the unique solution `α := λ x. x`. But if we have `α x + y =? β x + y`,
there are again multiple solutions:

    α := λ _. zero;     β := λ _. zero
	α := λ _. suc zero; β := λ _. suc zero
	α := λ x. x;        β := λ x. x
	...

We have to be a bit careful though; the positive eliminator whose scrutinee is
blocked by a metavariable, is still rather squishy! Consider:

    α x .(case (β x) true) =? x .(case true true)

This has multiple solutions. In one solution, we do the "obvious" thing:

    α := λ x. x;   β := λ x. x

Here's a less obvious one:

    α := λ _. true;   β := λ x. x .(case true true)

Here, the left hand side reduces to the `true` branch, and we put just the right
thing there to make the equation hold.

Let's look at a basic specification of spine reduction which rules out cases like
the previous one.

**Spine reduction, version 1**

If we have an equation of the form

    α spine .(case t₁ t₂) =? x spine' .(case t₁ t₂)

it's enough to solve

    α spine =? x spine'

Here, by writing the same `t₁` and `t₂` on the two sides, I specified the `case`
branches to be already definitionally equal.

In practice, this means that we have to compare branches for definitional
equality *without* solving any metavariables in the process. This is fairly easy
to implement: just pass a flag to unification which turns off metavariable
solutions.

But we can make the spine reduction a bit more liberal. We want to rule out the
possibility of reducing the `case` in the LHS; if that's possible, we cannot hope
to have a unique solution, since `Bool` has two defini

<!-- **Spine reduction, version 2** -->

<!-- If we have an equation of the form -->

<!--     α spine .(case t₁ t₂) =? x spine' .(case t₁' t₂') -->

<!-- and `t₁` and `t₂` are both definitionally distinct from `x spine' .(case t₁' t₂')` -->
<!-- under any solution of metavariables, it suffices to solve -->

<!--     α spine =? x spine' -->

<!-- The disjointness condition -->



<!-- Let's go back to the previous problematic example: -->

<!--     α x .(case (β x) true) =? x .(case true true) -->

<!-- Here, in the problem `β x =? true`, `β` does not occur under a canonical constructor, -->
<!-- so we cannot proceed. This works: -->

<!--     α x .(case (suc (β x)) zero) =? x .(case (suc zero) zero) -->

<!-- Here, `suc (β x) =? suc zero` has `β` under `suc`. -->

<!-- The restriction seems to be sufficient to rule out canonical solutions for `α`. -->
<!-- If we had such a solution, the LHS `case` split would reduce to one branch. The -->
<!-- equation on the whole could only possibly hold if that branch was convertible to -->
<!-- the whole RHS (since the RHS is rigidly blocked and can't be reduced). -->
