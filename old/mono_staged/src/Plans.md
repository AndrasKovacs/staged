

### Static language

- Dependent TT with type-in-type, Π and native records.
- Decent elaboration support for record, overloaded projection.
- No inductive types.
- Plain non-parametrized cycle-free modules.

We keep the static language very simple. Church-coded types are sufficient for a
huge number of use-cases. Plans for later:

- Richer static types. For something like a lexer/parser generator, or bigger
  EDSL compilers, we need proper data structures.
- Typeclasses.

### Runtime language

- Monomorphic, strict-by-default, effectful.
- Closures (A ~> B) and known functions (A -> B) are distinguished. The universe of runtime types
  is split to
  - Value types: everything except known function types.
  - Computation types: known function types.
  - It is possible to eliminate from value types to computation types. This requires
    eventually lifting lambdas out from eliminators, e.g. `if b then λ x. x else λ x. x + 10`
	becomes `λ x. if b then x else x + 10`.













<!-- - Static language is a type-in-type TT with native record types. No inductive -->
<!--   types yet. Records have decent elaboration support and overloaded projections. -->
<!-- - Runtime language is monomorphic, strict and effectful. -->
<!--   - ADTs, with distinction between unboxed and boxed sums. -->
<!--   - Mutable reference types, immutable + mutable array types -->
<!--   - Usual int, uint, float types and ops. -->
<!--   - No exceptions yet. -->
<!--   - Effects are untyped. -->
