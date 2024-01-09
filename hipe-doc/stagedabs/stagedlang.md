
## Two-stage functional programming from the ground up

This document describes a programming language and develops a programming
toolbox inside it. The language itself is not yet implemented, but a significant
part of it can be emulated in other existing systems, at the cost of some extra
noise and inconvenience. Indeed, I adapted much of the code described here to
Typed Template Haskell and provide it as a supplement.

The main idea is to rephrase as much as possible code optimization as evaluation
of metaprograms.

- Metaprograms are deterministic, transparent and can be run efficiently.
- General-purpose optimizers are less transparent, less robust and slower.

For example:

- We'll develop fusion for sequence types, but unlike in Haskell, fusion is
  formally guaranteed to succeed, and it is also guaranteed to not produce
  any dynamic closures.
- We'll have a monad transformer library, but unlike in Haskell, the vast
  majority of optimization of monadic code happens by evaluation of
  metaprograms, and for the most common monads (State, Reader, Except) it is
  formally guaranteed that monadic code never introduces new dynamic closures
  (although there may be other closures that the programmer explicitly
  introduced).

The language is based on two-level type theory (2LTT), so to keep things short
I'll refer to it simply as 2LTT in this document. Note though that many
different 2LTT flavors are possible.

This 2LTT is strongly focused on compiling to robustly high-performance code.
Hence, programmers have to manage more details than in Haskell or
OCaml. However, it's still a "high-level" garbage-collected language, and we
should have less overall bureaucracy than in Rust, for example.


<!-- Overall, because this 2LTT is more performance-focused than Haskell or OCaml, -->
<!-- there are more details to keep in mind and manage. However, it's still a -->
<!-- "high-level" garbage-collected language, and we have less bureaucracy than in -->
<!-- Rust. -->

<!-- This document describes a programming language based on two-level type theory -->
<!-- (2LTT), which is not yet implemented and which I would personally like to use -->
<!-- for practical programming. Features: -->


<!-- ## Nice Strongly Typed Staged Programming -->

<!-- This document describes a programming language based on two-level type theory -->
<!-- (2LTT), which is not yet implemented and which I would personally like to use -->
<!-- for practical programming. Features: -->

<!-- - Robust performance guarantees, based on types. -->
<!-- - Plenty of control over code generation and memory layouts, in the surface -->
<!--   language. -->
<!-- - Good ergonomics and strong inference (of types, values and staging -->
<!--   annotations) in the surface language. -->

<!-- The main idea is to rephrase as much as possible code optimization as -->
<!-- evaluation of metaprograms. -->

<!-- - Metaprograms are deterministic, transparent and can be run efficiently. -->
<!-- - General-purpose optimizers are less transparent, less robust and slower. -->

<!-- For example, in 2LTT, fusion can be implemented with deterministic metaprograms -->
<!-- and it's guaranteed to work. In Haskell, fusion relies on INLINE pragmas and the -->
<!-- general-purpose simplifier, and it fails frequently. -->

<!-- Overall, because this 2LTT is more performance-focused than Haskell or OCaml, -->
<!-- there are more details to keep in mind and manage. However, it's still a -->
<!-- "high-level" garbage-collected language, and we have less bureaucracy than in -->
<!-- Rust. -->

<!-- In the following I describe language features, how to use them, and how to -->
<!-- compile them. I focus on examples. Programming abstractions and tiny libraries -->
<!-- will be also built up along the way. -->

<!-- Some of the choices of syntax and its sugar are fairly arbitrary and up to -->
<!-- personal taste, and are not set in stone. -->

<!-- Let's call the language simply 2LTT for now, for the sake of brevity. Keep in -->
<!-- mind though that there are many possible variations on the 2LTTs that appeared -->
<!-- in previous papers. -->

<!-- ### Meta level -->

<!-- There's a dependently typed language layer for compile-time programming. -->

<!-- - `MetaTy` is the type of meta-level types. Whenever `A : MetaTy`, `A` only -->
<!--   exists at compile time. -->
<!-- - For simplicity, we have `MetaTy : MetaTy`. -->
<!-- - `MetaTy` supports Π, Σ, ⊤ and indexed inductive types, using the same syntax -->
<!--   as in Agda. -->
<!-- - We use Agda syntax and Agda-style implicit arguments, with some differences. -->

<!-- A basic definition: -->

<!--     id : {A : MetaTy} → A → A -->
<!--       = λ x. x -->

<!-- Unlike in Agda and Haskell, we don't repeat `id` for the type declaration and -->
<!-- the definition. We can also use Coq-style definitions: -->

<!--     id {A : MetaTy}(x : A) = x -->

<!-- By convention, inductive types in `MetaTy` often have `₁` as subscript. That's -->
<!-- to disambiguate meta-level types from object-level ones. For example: -->

<!--     data ℕ₁ : MetaTy where -->
<!--       zero₁ : ℕ₁ -->
<!--       suc₁  : ℕ₁ → ℕ₁ -->

<!-- Meta-level inductive types can only have fields with types in `MetaTy`. -->

<!-- However, the subscripts may be omitted when it's obvious which `ℕ` we use. -->
<!-- Syntax for Π (dependent functions) follows Agda. -->

<!-- There is a built-in notion of anonymous **record types**: -->

<!-- - `[field1 : A, field2 : B, field3 : C]` is a record type with three fields.  We -->
<!--   may have zero or more fields. Types can depends on fields to the left. -->
<!-- - Meta-level record types can only have field types in `MetaTy`. -->
<!-- - There is a field projection notation. If `t : [field1 : A, field2 : B field1]`, then -->
<!--   `t.field1 : A` and `t.field2 : B (t.field1)`. -->
<!-- - Field projections bind stronger than function application, so `B t.field1` is -->
<!--   shorthand for `B (t.field1)`. -->
<!-- - Values with records types are `(t, u, v)`, or `(field = t, field2 = u, field3 = v)`. -->
<!--   The unit type is defined as `[]` with `()` as value. -->
<!-- - Field names can be omitted, like in `[A, B, C]`. An unnamed field can be projected -->
<!--   by number, so `t : [A, B]` implies `t.1 : A` and `t.2 : B`. -->

<!-- ### Object level -->

<!-- A 2LTT program is compiled by first *staging* it, which means running all -->
<!-- metaprograms, which generates object code, and then further processing the -->
<!-- object code. Staging is the part of the pipeline where programmers still have -->
<!-- full control over compilation. This is in contrast to e.g. GHC where programmers -->
<!-- lose precise control immediately after their code is type checked, and then they -->
<!-- have to nag and cajole GHC with INLINE pragmas, compiler options and bang -->
<!-- patterns, to generate desired code. -->

<!-- The object language is very simple and dumb compared to the metalanguage. It is -->
<!-- easy to compile and optimize, but tedious to write code in. Fortunately, the -->
<!-- metalanguage is very expressive and hugely improves the usability of the overall -->
<!-- system, and since metaprograms only exists at compile time, we don't have to -->
<!-- deal with their fancy features in downstream compilation. -->

<!-- Let's get to the details of the object-level. -->

<!-- - `Ty : MetaTy` is the type of object types. Since `Ty` is in `MetaTy`, the -->
<!--   object language can't abstract over object types. It's *simply typed*, there's -->
<!--   no polymorphism and no type dependency. -->
<!-- - `ValTy : MetaTy` is the type of *value types* in the object language. Value -->
<!--   types, for now, are just primitive machine types (like `Int64` and `Double`), -->
<!--   record types and ADTs. -->
<!-- - `ValTy` is a subtype of `Ty`. So whenever `A : ValTy` then `A : Ty` as well. -->
<!-- - `CompTy : MetaTy` is likewise a subtype of `Ty`. It has "computation" types; -->
<!--   at first these are just the function types. -->

<!-- Note that this system is **not** call-by-push-value, despite the naming -->
<!-- commonalities. -->

<!-- The **function type former**: -->

<!--     A : ValTy   B : Ty -->
<!--     ────────────────── -->
<!--       A → B : CompTy -->

<!-- This implies that `→` cannot appear as function input; `→` is a *first-order* -->
<!-- function type. As we'll see, `→` has the extremely useful property that it can -->
<!-- be compiled without closures, only using *lambda-lifting*. We'll also see that -->
<!-- closures are surprisingly rarely needed in ordinary programming! -->

<!-- Field types of object-level records and ADTs must be value types. So the -->
<!-- following is OK: -->

<!--     data List (A : ValTy) : ValTy where -->
<!--       Nil  : List A -->
<!--       Cons : A → List A → List A -->

<!-- The following is not OK: -->

<!--     data Foo : ValTy where -->
<!--       MkFoo : (Bool → Int) → Foo -->

<!-- Object-level ADTs can be parameterized by anything, even meta-level types and -->
<!-- values, but fields must always be object-level value types. Object-level ADTs -->
<!-- also cannot have indices, only parameters. -->

<!-- We can use `unboxed data` to declare unboxed sums, similarly to enum types in Rust. -->

<!--     unboxed data Maybe (A : ValTy) : ValTy where -->
<!--       Nothing : Maybe A -->
<!--       Just    : A → Maybe A -->

<!-- This will be compiled as a padded fixed-size structure with a tag field. Also -->
<!-- like in Rust, recursive fields of unboxed types must be guarded by an indirection. -->
<!-- Normal `data`-declared types are represented as variable-sized data behind an -->
<!-- indirection. -->

<!-- We can use Haskell-style ADT notation as shorthand, and leave parameter types -->
<!-- inferred. However, we always have to mark the return universe of the type: -->

<!--     unboxed data Maybe A : ValTy = Nothing | Just A -->
<!--     data List A : ValTy = Nil | Cons A (List A) -->
<!--     data MetaList (A : MetaTy) : MetaTy = Nil | Cons A (MetaList A) -->

<!-- Example for an object-level definition at the top level, involving an ADT: -->

<!--     mapAdd10 : List Int → List Int -->
<!--       := λ xs. letrec go := λ xs. case xs of -->
<!--                  nil.       nil -->
<!--                  cons x xs. cons (x + 10) (go xs); -->
<!--                go xs -->

<!-- Things to note: -->

<!-- - We use `:=` instead of `=` to define things at the object level. This will be -->
<!--   important to disambiguate meta-level and object-level things; or at least, in -->
<!--   my existing 2LTT implementations it has been a very good design choice to have -->
<!--   this. The reason is that definitions and `let`-s are the main source of *stage -->
<!--   ambiguity*, and if they're disambiguated, we can infer a *huge amount* of -->
<!--   stages and stage annotations everywhere else. -->
<!-- - Recursion must be always introduced with a `letrec`. -->
<!-- - Note that `List Int` is a concrete monotype; it would be not possible to -->
<!--   have a polymorphic `map` function here. -->

<!-- With a bit more syntactic sugar: -->

<!--     mapAdd10 (xs : List Int) : List Int -->
<!--       := letrec go := λ case -->
<!--            nil.       nil -->
<!--            cons x xs. cons (x + 10) (go xs); -->
<!--          go -->

<!-- We have `let` and `letrec` in general. They are delimited with `;`, instead of -->
<!-- the `in` in Haskell. -->

<!--     foo : Int := -->
<!--       let x := 10; -->
<!--       let y := 20; -->
<!--       let z := 30; -->
<!--       x + y + z -->

<!-- `let` and `letrec` can be used to define values of any type (not just value -->
<!-- types). The following works: -->

<!--     foo (x : Int) : Int := -->
<!--       let bar := λ y z. x + y + z; -->
<!--       let baz := bar 10; -->
<!--       bar 20 -->

<!-- **Importantly**: we can compute things in `Ty` by `case` splitting. The -->
<!-- following is legal: -->

<!--     foo : Bool → Int → Int -->
<!--       := λ b. case b of -->
<!--         true.  λ x. x + 10 -->
<!--         false. λ x. x * 10 -->

<!-- I'll talk more about this later. In short, the ability to eliminate to `Ty` from -->
<!-- `ValTy` makes metaprogramming much more convenient, but it also requires a bit -->
<!-- more work in compiling the staging output. -->

<!-- The notation for **object-level record types** is the same as on the meta level. -->
<!-- However, the field assignment notation for object-level records uses `:=`, -->
<!-- following the convention in `let`: -->

<!--     foo : [field1 : Int, field2 : Int] -->
<!--       := (field1 := 10, field2 := 20) -->

<!-- Object-level records don't support type dependency. They are compiled as -->
<!-- *unboxed tuples*. When we want to use boxed records instead, we can just -->
<!-- use a wrapper ADT definition: -->

<!--     data Box (A : ValTy) : ValTy = Box A -->

<!-- The **closure type former**: -->

<!--       A : CompTy              t : A          t : Close A -->
<!--     ───────────────     ─────────────────    ──────────── -->
<!--     Close A : ValTy     close t : Close A     open t : A -->

<!-- Using `Close`, we can pass computations dynamically. We define a synonym for -->
<!-- closure-based functions: -->

<!--     _~>_ : ValTy → ValTy → ValTy -->
<!-- 	A ~> B = Close (A → B) -->

<!-- Now we can write: -->

<!--     map (f : Int ~> Int) : List Int → List Int -->
<!--       := letrec go := λ case -->
<!--             nil. nil -->
<!--             cons x xs. cons (open f x) (go xs); -->
<!--          go -->

<!-- In the following, we call `→` functions "functions", and `~>` functions -->
<!-- "closures". Distinguishing them is an important part of controlling code -->
<!-- generation. -->


<!-- ### Compiling object-level functions -->

<!-- Functions can always be compiled without closures, which is a significant -->
<!-- performance benefit. How can we do that, given that -->

<!--     case b of true.  λ x. x + 10 -->
<!--               false. λ x. x * 10 -->

<!-- is allowed? Also, -->

<!--     foo (x : Int) : Int := -->
<!--       let bar := λ y z. x + y + z; -->
<!--       let baz := bar 10; -->
<!--       let n   := bar 10 20; -->
<!--       baz n -->

<!-- The idea: functions can only ever be used by applying them to all arguments and -->
<!-- extracting the end result value. They also cannot escape their scope. Partial -->
<!-- applications in local `let`-s are considered as syntactic sugar for eta-expanded -->
<!-- definitions. -->

<!-- Hence, every `λ` which is under a `case`, can be floated out. So, we compile -->

<!--     foo : Bool → Int → Int -->
<!--       := λ b. case b of -->
<!--         true.  λ x. x + 10 -->
<!--         false. λ x. x * 10 -->

<!-- to -->

<!--     foo : Bool → Int → Int -->
<!--       := λ b x. case b of -->
<!--         true.  x + 10 -->
<!--         false. x * 10 -->

<!-- In Haskell, such transformation can degrade performance, if the case -->
<!-- scrutinization is expensive, and the function is called multiple times with the -->
<!-- same first argument. In that case, we would prefer to compute the expensive -->
<!-- casing just once, return a closure, and then happily apply that closure multiple -->
<!-- times. -->

<!-- In 2LTT, we'd need to explicitly use closure types to make this -->
<!-- possible. Function types in 2LTT only ever compute when fully applied. It does -->
<!-- not matter how cases and function lambdas are ordered, because whenever a -->
<!-- function is called at runtime, we have all arguments available. -->

<!-- After eta-expansions and `λ`-floating, all local functions are in a -->
<!-- lambda-liftable form. The functions which are only tail-called are kept -->
<!-- around as local join points, otherwise they are lifted out to the top. -->

<!-- So we compile -->

<!--     foo (x : Int) : Int := -->
<!--       let bar := λ y z. x + y + z; -->
<!--       let baz := bar 10; -->
<!--       let n   := bar 10 20; -->
<!--       baz n -->

<!-- to -->

<!--     bar := λ x y z. x + y + z; -->

<!--     foo := λ x. -->
<!--       join baz := λ z. bar x 10 z; -->
<!--       let n := bar x 10 20; -->
<!--       baz n -->

<!-- Here, `baz` is only tail-called, so it's eta-expanded and kept as joint point. -->
<!-- `bar` is called in non-tail position so it's lifted to the top. Of course any -->
<!-- mildly optimizing compiler will inline `baz` as well - I'm only illustrating -->
<!-- lambda-lifting and call saturation here. In summary, function calls will always -->
<!-- be compiled to statically known saturated calls. -->

<!-- ### Compiling closures -->

<!-- The implementation of our closures is closer to Koka and Rust, and differs from -->
<!-- OCaml and Haskell. In OCaml and Haskell the *arity* of a closure is only known -->
<!-- at runtime, and closure application has to dynamically check whether we have -->
<!-- under-application, saturated application or over-application. -->

<!-- In 2LTT, if `f : A ~> B`, then `f` has exactly one argument whose memory layout -->
<!-- is determined by the concrete `A` type, and a call to `f` does not perform any -->
<!-- runtime arity checking. Remember that the object language is monomorphic, so it -->
<!-- is naturally possible to associate different memory layouts to different types. -->

<!-- This implies that multi-argument closures can be efficiently represented as -->
<!-- `[A, B, C] ~> D` or as `Close (A → B → C → D)`; these versions actually yield -->
<!-- exactly the same performance and calling convention at runtime. If we use -->
<!-- `A ~> B ~> B`, that is meant as `A ~> (B ~> C)`, a closure returning a closure. -->

<!-- CPS is significantly more efficient in 2LTT than in GHC or OCaml: -->
<!-- - There is close to no dynamic overhead on closure calls, compared to making -->
<!--   static calls. GHC and OCaml have the arity checking overhead, and GHC -->
<!--   additionally needs to check for thunks. -->
<!-- - Continuation arguments can be unboxed, and generally have varying memory -->
<!--   representation. In GHC and OCaml, closure arguments can only be generic heap -->
<!--   objects. In short, CPS prevents unboxing there. -->

<!-- ### Staging operations -->

<!-- Let's talk about staging now. So far, object-level and meta-level features were -->
<!-- separate, without much interaction to speak of. Staging operations make it -->
<!-- possible to move between stages. -->

<!-- 1. If `A : Ty`, then `↑A : MetaTy`. This is pronounced "lift `A`". `↑A` is the -->
<!--    type of metaprograms which produce `A`-typed object-level expressions. -->
<!-- 2. If `t : A` and `A : Ty`, then `<t> : ↑A`, pronounced "quote `t`". This -->
<!--    is the metaprogram which immediate returns `t` as an expression. -->
<!-- 3. If `t : ↑A`, then `~t : A`, pronounced "splice `t`". Splicing runs -->
<!--    a metaprogram and inserts its resulting expression at some point. -->

<!-- Additionally, we have `<~t> ≡ t` and `~<t> ≡ t` as definitional equalities; this -->
<!-- is often needed for type checking dependently typed metaprograms. -->

<!-- Splicing binds stronger than function application, so `f ~x ~y` means `f (~x) (~y)` -->
<!-- , but it's weaker than field projections, so `~f.x` means `~(f.x)`. -->

<!-- **Example**. Consider the meta identity function: -->

<!--     id : (A : MetaTy) → A → A -->
<!--       = λ A x. x -->

<!-- Thanks to staging operations, this can be used in object code too: -->

<!--     n : Int := ~(id (↑Int) <10>) -->

<!-- Here, `id (↑Int) <10> : ↑Int` is a metaprogram which computes to `<10>` during -->
<!-- staging. Then, we get `~<10>` in object code, which computes to simply `10`. -->

<!-- Now, it would be annoying if we had to write all of these quotes and splices all -->
<!-- the time. Fortunately, almost all of quotes and splices are unambiguously -->
<!-- inferable, with bidirectional elaboration and the disambiguated stages of -->
<!-- definitions (`=` vs `:=`). Something like the following works fine: -->

<!--     id : {A : MetaTy} → A → A -->
<!--       = λ x. x -->

<!--     n := id 10 -->

<!-- How to elaborate `n`: -->

<!-- 1. We try to infer an object type for `id 10`. -->
<!-- 2. We look up the type of `id`. -->
<!-- 3. Since `id` has implicit function type, we insert an implicit application, so -->
<!--    we have `id {?0} 10` at this point, where `?0` is a metavariable. -->
<!-- 4. We infer type for `10`, which is just `Int : ValTy`. -->
<!-- 5. We try to cast `10 : Int : ValTy` to `?0 : MetaTy`. The stages don't match, so -->
<!--    we lift the left side to `<10> : ↑Int : MetaTy`, and unify `↑Int` with `?0`. -->
<!-- 6. Now we have `id {↑Int} <10> : ↑Int` as output. -->
<!-- 7. Since we had `n :=`, an object expression is expected, so we cast the result -->
<!--    down to object-level by inserting a splice. -->
<!-- 8. `n := ~(id {↑Int} <10>)` is the final result. -->

<!-- I have implemented fairly strong bidirectional stage inference in my 2LTT -->
<!-- prototype, so the inference promises I'm making here and later are based on -->
<!-- real-world experience. -->

<!-- **Example**. Coercive subtyping for functions: -->

<!--     id : {A : MetaTy} → A → A -->
<!--       = λ x. x -->

<!--     f : Int → Int -->
<!--       := id -->

<!-- This a bit weird; how can we turn the meta-level `id` to an object function? I -->
<!-- previously implemented this as a form of coercive subtyping, where a -->
<!-- metafunction can be cast down to the object-level by eta-expansion. Coercions -->
<!-- can be inserted by bidirectional elaboration, when there's a mismatch between -->
<!-- inferred and expected types, in the following way. -->

<!--     ──────────────── -->
<!--     coe A ↑A t = <t> -->

<!--     ─────────────── -->
<!--     coe ↑A A t = ~ŧ -->

<!--     ──────────────────── -->
<!--     coe Ty MetaTy A = ↑A -->

<!--          _⇒_ is any kind of function type (object-level, meta) -->
<!--     ─────────────────────────────────────────────────────────────────────────── -->
<!--     coe (A ⇒ B) (A' ⇒ B') t = λ x. coe (B (coe A' A x)) (B' x) (t (coe A' A x)) -->

<!--      [xᵢ : Aᵢ] is object-level or meta-level record -->
<!--     ───────────────────────────────────────────────── -->
<!--     coe [xᵢ : Aᵢ] [xᵢ : Bᵢ] t = (xᵢ = coe Aᵢ Bᵢ t.xᵢ) -->

<!-- In short, we can coerce between functions contra-and-covariantly, and between -->
<!-- records covariantly, as usual. -->

<!-- The `coe Ty MetaTy` rule makes it possible to omit a bunch of `↑`-s in surface -->
<!-- syntax. For example, an explicit type for `map` would be -->

<!--     map : {A B : ValTy} → (↑A → ↑B) → ↑(List A) → ↑(List B) -->

<!-- But `A B : ValTy` implies that the codomain type must be in `MetaTy`, so we can -->
<!-- the drop the lifts in the surface syntax. When the elaborator hits `A` in `(A → -->
<!-- B)`, we have `A : ValTy` with `MetaTy` as expected type, so `A` is first -->
<!-- silently cast to `Ty`, then to `↑A : MetaTy`. -->

<!--     map : {A B : ValTy} → (A → B) → List A → List B -->

<!-- In the previous example, `id` first gets elaborated to `id {?0} : ?0 → ?0 : -->
<!-- MetaTy`, and we want to coerce it to `Int → Int`. This works by eta-expansion, -->
<!-- coercing the argument type backwards, and the result forwards, getting as -->
<!-- elaboration output -->

<!--     f : Int → Int -->
<!--       := λ x. ~(id {Int↑} <x>) -->

<!-- and as staging output -->

<!--     f : Int → Int -->
<!--       := λ x. x -->

<!-- When we coerce `x : Int` to have type `?0 : MetaTy`, since the stages don't -->
<!-- match, the only coercion rule that matches the situation yields `<x> : ↑Int`, at -->
<!-- which point we don't have more coercion rules, so we unify `↑Int` with `?0`. So -->
<!-- at this point we learn what `?0` is, and then we just use `Int ≤ ↑Int` to coerce -->
<!-- the function body in the result. -->

<!-- **Example**. More coercive subtyping. Input: -->

<!--     comp : {A B C : MetaTy} → (B → C) → (A → B) → A → C -->
<!--       = λ f g x. f (g x) -->

<!--     f : Int → Int -->
<!--       := comp ((+) 10) ((+) 20) -->

<!-- Elaboration output: -->

<!--     comp : {A B C : MetaTy} → (B → C) → (A → B) → A → C -->
<!--       = λ f g x. f (g x) -->

<!--     f : Int → Int -->
<!--       := λ x. ~(comp {↑Int}{↑Int}{↑Int} (λ x. <10 + ~x>) (λ x. <10 + ~x>) <x>) -->

<!-- Staging output: -->

<!--     f : Int → Int -->
<!--       := λ x. 10 + (10 + x) -->

<!-- **Example**. List mapping, now for real, fully explicitly. -->

<!--     data List (A : ValTy) : ValTy = Nil | Cons A (List A) -->

<!--     map : {A B : ValTy} → (↑A → ↑B) → ↑(List A) → ↑(List B) -->
<!--       := λ {A}{B} f xs. -->
<!--         <letrec go := λcase -->
<!--              Nil.       Nil -->
<!--              Cons a as. Cons ~(f <a>) (go as); -->
<!--          go ~xs> -->

<!-- The same with stage inference. -->

<!--     map : {A B : ValTy} → (A → B) → List A → List B -->
<!--       := λ f xs. -->
<!--         letrec go := λcase -->
<!--             Nil.       Nil -->
<!--             Cons a as. Cons (f a) (go as); -->
<!--         go xs -->

<!-- **Example**. Input: -->

<!--     foo (x : Int) := map (+x) -->

<!-- Staging output, also after lambda-lifting. -->

<!--     go x xs := case xs of -->
<!--       Nil.       Nil -->
<!--       Cons y xs. Cons (y + x) (go x xs) -->

<!--     foo x xs := go x xs -->

<!-- Note that the recursive function gained an extra argument via lambda-lifting. -->

<!-- **Example**. Computing code from meta-level data. We do exponentiation, where -->
<!-- the exponent is a statically known natural number. -->

<!--     data MNat : MetaTy = Zero | Suc MNat -->

<!--     exp : ↑Int → MNat → ↑Int -->
<!--     exp n e = case e of -->
<!--       Zero.  1 -->
<!--       Suc e. n * exp n e -->

<!-- After elaboration: -->

<!--     data MNat : MetaTy = Zero | Suc MNat -->

<!--     exp : ↑Int → MNat → ↑Int -->
<!--     exp n e = case e of -->
<!--       Zero.  <1> -->
<!--       Suc e. <~n * ~(exp n e)> -->

<!-- Now, `exp <10> (Suc (Suc Zero))` is staged to `<10 * (10 * 1)>`. -->

<!-- ### Functor, Applicative, Monad -->

<!-- These three classes are the backbone of abstraction in Haskell. I make the -->
<!-- choice of having these classes *only* in `MetaTy`. This may be surprising at -->
<!-- first; how will we map over plain lists in `ValTy`? We shall map over lists by -->
<!-- first embedding them into a `MetaTy → MetaTy` functor, mapping there, then -->
<!-- converting back. The meta-functor supports general-purpose fusion. -->

<!-- It becomes a matter of *defensive programming* that we only let users map over -->
<!-- lists by going through a fusible type. This would be weird in the context of -->
<!-- high-performance Haskell, where fusion is very fragile and we often deliberately -->
<!-- avoid it. In contrast, in 2LTT we have formal guarantee of fusion. In Rust it's -->
<!-- also standard practice to map over structures by round-tripping through -->
<!-- iterators. -->

<!--     class Functor (F : MetaTy → MetaTy) where -->
<!--       _<$>_ : {A B : MetaTy} → (A → B) → F A → F B -->

<!--     class Functor F => Applicative F where -->
<!--       pure  : {A : MetaTy} → A → F A -->
<!--       _<*>_ : {A B : MetaTy} → F (A → B) → F A → F B -->

<!--     class Applicative M => Monad M where -->
<!--       (>>=) : {A B : MetaTy} → M A → (A → M B) → M B -->

<!-- **do-notation** is the same way as in Haskell. We also use Haskell's -->
<!-- `BlockArguments` syntax (which is also the default behavior in Agda). -->

<!-- ### Reader -->

<!-- Let's start with one of the simplest monads. -->

<!--     Reader : ValTy → Ty → Ty -->
<!--       := λ R A. R → A -->

<!--     ReaderM : MetaTy → MetaTy → MetaTy -->
<!--       := λ R A. R → A -->

<!--     instance Functor ReaderM where -->
<!--       fmap f g r = f (g r) -->

<!--     instance Applicative ReaderM where -->
<!--       pure a r = a -->
<!--       rf <*> ra = λ r. rf r (ra r) -->

<!--     instance Monad ReaderM where -->
<!--       ra >>= f = λ r. f (ra r) r -->

<!-- We have implicit conversions between `↑(Reader R A)` and `ReaderM (↑R) (↑A)`, by -->
<!-- subtyping. So specifically for `Reader` we don't have to explicitly convert. -->

<!-- **Example**. Elaboration input: -->

<!--     foo : Bool → Reader Int Int -->
<!--       := λ b. case b of -->
<!--         True.  do {n <- ask; pure (n + 10)} -->
<!--         False. do {n <- ask; pure (n + 20)} -->

<!-- Elaboration desugars the `do` and coerces the `ReaderM` block down to `Reader`: -->

<!--     foo : Bool → Reader Int Int -->
<!--       := λ b. case b of -->
<!--         True.  λ r. ~((>>=) {↑Int} {↑Int} (ask {↑Int}) (λ n. pure {↑Int} <~n + 10>) <r>) -->
<!--         False. λ r. ~((>>=) {↑Int} {↑Int} (ask {↑Int}) (λ n. pure {↑Int} <~n + 20>) <r>) -->

<!-- Staging output: -->

<!--     foo : Bool → Int → Int -->
<!--       := λ b. case b of -->
<!--         True.  λ n. n + 10 -->
<!--         False. λ n. n + 20 -->

<!-- After saturation & lambda lifting: -->

<!--     foo : Bool → Int → Int -->
<!--       := λ b n. case b of -->
<!--         True.  n + 10 -->
<!--         False. n + 20 -->

<!-- It's nice here that `Reader` cannot create closures. In comparison, GHC often -->
<!-- trips up in cases like this and misses the lambda floating. -->

<!-- ### State -->

<!-- `State` is another case where we can implicitly convert between meta-state and -->
<!-- object-state. I omit the completely standard definitions. -->

<!--     State (S : ValTy) (A : ValTy) : Ty = S → [A, S] -->
<!--     StateM (S A : MetaTy) : MetaTy = S → [A, S] -->

<!--     instance Functor (StateM S) where ... -->
<!--     instance Applicative (StateM S) where ... -->
<!--     instance Monad (StateM S) where ... -->

<!--     put : S → StateM S [] = ... -->
<!--     get : StateM S S = ... -->

<!-- There is a bit of an issue with `StateM` though. The monadic binding computes -->
<!-- *too much* statically, it behaves like an *inline definition*. -->

<!-- **Example**. Elaboration input. -->

<!--     foo : State Int Int -->
<!--       := do -->
<!-- 	    modify (λ n. n + n) -->
<!-- 	    modify (λ n. n + n) -->
<!--         get -->

<!-- Staging output. -->

<!--     foo : Int → [Int, Int] -->
<!-- 	  := λ n. ((n + n) + (n + n), (n + n) + (n + n)) -->

<!-- Whenever we write `modify`, the current state gets replaced with a new -->
<!-- *expression*. If we modify twice, the expression is expanded twice, and in the -->
<!-- end they are just inlined in the output pair. We need some way to force -->
<!-- evaluation of expressions in the generated code. In `ReaderM` and `StateM` this -->
<!-- is not really possible. -->

<!-- ### The code generation monad -->

<!-- The ability to force object-level evaluation can be nicely encapsulated in a -->
<!-- concrete monad: -->

<!--     Gen : MetaTy → MetaTy -->
<!-- 	Gen A = (R : Ty) → (A → ↑R) → ↑R -->

<!-- 	instance Functor Gen where -->
<!-- 	  (f <$> g) = λ R k. g R (λ a. f (k a)) -->

<!--     instance Applicative Gen where -->
<!-- 	  pure a = λ R k. k a -->

<!--     instance Monad Gen where -->
<!-- 	  (f >>= g) = λ R k. f R (λ a. g a R k) -->

<!-- This means "code generation" monad. It's just a CPS'd identity monad, except -->
<!-- that we can only run it by computing an expression. -->

<!--     runGen : {A : Ty} → Gen ↑A → ↑A -->
<!-- 	  = λ {A} f. f A id -->

<!-- Because `Gen` always returns eventually in some `↑R`, it's possible to insert -->
<!-- object-level let-definitions in `Gen` actions. -->

<!--     gen : {A : Ty} → ↑A → Gen ↑A -->
<!-- 	gen = λ {A} a R k. <let x : A := ~a; ~(k <x>)> -->

<!-- Now, we can use object-let in `Gen` in any position: -->

<!--     foo : ↑Int → Gen ↑Int = do -->
<!-- 	  n <- gen n -->
<!-- 	  n <- gen (n * n * n * n) -->
<!-- 	  pure n -->

<!--     bar : Int → Int -->
<!-- 	  = λ x. runGen (foo (x + x)) -->

<!-- After staging, we get: -->

<!--     bar : Int → Int -->
<!--      = λ x. -->
<!-- 	  let n := x + x; -->
<!-- 	  let n := n * n * n * n; -->
<!-- 	  n -->

<!-- The following function let-binds the results of all expressions -->
<!-- in a metalist: -->

<!--     genList : MetaList ↑A → Gen (MetaList ↑A) -->
<!-- 	  = mapM gen -->

<!-- The following silly example inserts some unnecessary let-bindings: -->

<!-- 	bar : Int → Int -->
<!--      := λ x. runGen do -->
<!-- 	  _ <- genList [x + x, x * x, x + 200] -->
<!-- 	  pure x -->

<!-- After staging: -->

<!--     bar : Int → Int -->
<!--      := λ x. -->
<!-- 	  let x1 := x + x; -->
<!-- 	  let x2 := x * x; -->
<!-- 	  let x3 := x + 200; -->
<!-- 	  x -->

<!-- The `x`-es are renamed to avoid name capture. This is done automatically and -->
<!-- efficiently in the staging implementation. -->

<!-- ### MTL-style abstraction -->

<!-- Now, we can define variants of `Reader` and `State` that support code -->
<!-- generation. I also add `ExceptT`. -->

<!--     ExceptT E M A = M (Either E A) -->
<!-- 	ReaderT R M A = R → M A -->
<!-- 	StateT  S M A = S → M [S, A] -->

<!--     ExceptI E = ExceptT E Gen -->
<!--     ReaderI R = ReaderT R Gen -->
<!-- 	StateI  S = StateT R Gen -->

<!-- The `I` in `ReaderI` etc. means "improved", which refers to the notion of -->
<!-- "binding time improvement" in the partial evaluation literature. Generally -->
<!-- speaking, for most object-level datatypes we will have a "binding-time improved" -->
<!-- variant. These variants are postfixed with `I`. The idea is that they mirror the -->
<!-- object-level counterpart but support more computation at compile time. -->

<!-- We can reproduce the usual MTL-style abstractions: -->

<!--     class (∀ M. Monad M => Monad (T M)) => MonadTrans T where -->
<!--       lift : Monad M => M A → T M A -->

<!--     class Monad M => MonadState S M where ... -->
<!--     class Monad M => MonadReader R M where ... -->
<!-- 	class Monad M => MonadError E M where ... -->

<!-- We also have -->

<!--     class Monad M => MonadGen M where -->
<!-- 	  gen : ↑A → M ↑A -->

<!--     instance MonadGen M => MonadGen (StateT S M) where -->
<!-- 	  gen a = lift (gen a) -->

<!--     instance MonadGen M => MonadGen (ReaderT R M) where -->
<!-- 	  gen a = lift (gen a) -->

<!--     instance MonadGen M => MonadGen (ExceptT E M) where -->
<!-- 	  gen a = lift (gen a) -->

<!--     instance Monad M => MonadState (StateT S M) where ... -->
<!-- 	instance Monad M => MonadReader (ReaderT R M) where ... -->
<!--     instance Monad M => MonadExcept (ExceptTT S M) where ... -->

<!-- As a result, we can define generic state operations that force object-level -->
<!-- evaluation: -->

<!--     put' : {S : Ty} → (MonadState S M, MonadGen M) => S → M [] -->
<!--       = λ s. do -->
<!-- 	   s <- gen s -->
<!-- 	   put s -->

<!--     modify' : {S : Ty} → (MonadState S M, MonadGen M) => (S → S) → M [] -->
<!-- 	  = λ f. do -->
<!-- 	   s <- get -->
<!--        put' (f s) -->

<!-- Similarly, we want a `local` operation that forces computation: -->

<!--     local' : {R : Ty}{A : MetaTy} → (MonadReader R M, MonadGen M) => (R → R) → M A → M A -->
<!-- 	  = λ f ma. do -->
<!-- 	   r <- ask -->
<!-- 	   r <- gen (f r) -->
<!-- 	   local (const r) ma -->

<!-- ### Conversion between meta-monads and object programs -->

<!-- Previously we relied on the ability to silently convert between `Reader` and -->
<!-- `ReaderM`, and `State` and `StateM`. However, the same conversion is not -->
<!-- available between `Reader` and `ReaderI` and likewise for `State`. Generally, -->
<!-- the implicit subtyping for functions and tuples does not cover the -->
<!-- "binding-time-improvement" conversions. -->

<!-- I'm not sure about the best design here. -->

<!-- A very simple option would be to have a class for such conversions: -->

<!--     class Improve (A : Ty) (B : MetaTy) where -->
<!-- 	  up   : ↑A → B -->
<!-- 	  down : B → ↑A -->

<!-- Fully explicitly for `Reader`: -->

<!--     instance Improve (Reader R A) (ReaderI ↑R ↑A) where -->
<!-- 	  up : ↑(R → A) → ↑R → (G : Ty) → (↑A → ↑G) → ↑G -->
<!-- 	    = λ f r G k. k <~f ~r> -->

<!-- 	  down : (↑R → (G : Ty) → (↑A → ↑G) → ↑G) → ↑(R → A) -->
<!-- 	    = λ f. <λ r. ~(f <r> G (λ x. x))> -->

<!-- With more inference and magic: -->

<!--     instance Improve (Reader R A) (ReaderI ↑R ↑A) where -->
<!-- 	  up f = λ r. pure (f r) -->
<!-- 	  down f = λ r. runGen (f r) -->

<!-- We can do the same for `State`. Now we can write: -->

<!--     foo : State Int Int := down do -->
<!-- 	  modify' (λ n. n + n) -->
<!-- 	  modify' (λ n. n + n) -->
<!--       get -->

<!-- After staging: -->

<!--     foo : Int → [Int, Int] := λ n. -->
<!-- 	  let n2 := n + n; -->
<!-- 	  let n3 := n2 + n2; -->
<!-- 	  (n3, n3) -->

<!-- I need to use `up` to refer to `foo` from meta-level code: -->

<!--     fooTwiceInlined : StateI Int Int = do -->
<!-- 	  up foo -->
<!-- 	  up foo -->

<!-- which is simply the meta-level definition that inlines the `foo` body twice, -->
<!-- although it's not a very good definition, since `foo` itself is an object-level -->
<!-- lambda, so the code output will be polluted with unnecessary beta-redexes. It'd -->
<!-- be more idiomatic to define `foo` as a metafunction to begin with. Every -->
<!-- definition that we intend to inline, should be a meta-level definition. -->

<!-- However, it might prove too annoying to write `down` and `up` whenever we switch -->
<!-- between stages. I might consider having implicit magic for `Improve` as well, so -->
<!-- that the conversions can be omitted. -->


<!-- ### Binding-time improvement for non-recursive types -->


<!-- 1. Take the same type at meta-level -->
<!-- 2. Wrap in Gen -->

<!-- A₀ --\-> Gen A₁ -->

<!-- Gen Bool₁ = (∀ R. (Bool₁ → R) → R) -->

<!-- not : Gen Bool₁ → Gen Bool₁ -->
<!--   = λ mb. do b ← mb; case b of True . pure False -->
<!--                                False. pure True -->

<!--   = λ mb R k. mb R (λcase True. k False; False. k True) -->

<!-- not (not mb) = λ R k. mb R (λcase True. k True; False. k False) -->

<!-- down : Gen Bool₁ → Bool₀ -->
<!-- down mb = mb Bool₀ (λcase True₁. True₀; False₁. False₀) -->

<!-- down (not (not mb)) = mb Bool₀ (λcase True₁. True₀; False₁. False₀)    OK -->
<!-- down (not (not mb)) ≡ down mb -->

<!-- ### For recursive types -->

<!--   - Church-encode, but restrict return type to `Ty` -->

<!--   - This version works for everything, but it's less modular because -->
<!--     we can't reuse Gen and the meta-level types -->

<!-- ### List fusion -->

<!-- What should be the binding-time improvement for lists? -->

<!--     ListI : MetaTy → MetaTy -->
<!-- 	  = ? -->

<!-- At least, `ListI` should be a monad, and it should support `Improve`-ment. -->
<!-- Consider first: -->

<!--     ↑(List A) -->

<!-- Hypothetically, if we had higher-order functions and polymorphism in the object -->
<!-- language, we could do a Church-coding inside the `↑` and get something like -->

<!--     ↑((L : Ty) → (A → L → L) → L → L) -->

<!-- Here, `Ty : Ty` is actually an object-level universe. Then, we could commute `↑` -->
<!-- to get -->

<!--     (L : ↑Ty) → (↑A → ↑~L → ↑~L) → ↑~L → ↑~L -->

<!-- This version supports way more static computation than the one we started with! -->
<!-- We don't actually have object-level universes and higher-order functions, but -->
<!-- the improved end-result doesn't need those, so we can still use it: -->

<!--     ListI A = (L : Ty) → (A → ↑L → ↑L) → ↑L → ↑L -->

<!-- This is a monad: -->
<!--     instance Functor ListI where -->
<!-- 	  (f <$> as) L c n = as L (λ a l. c (f a) l) n -->

<!--     instance Applicative ListI where -->
<!-- 	  pure a L c n = c a n -->

<!--     instance Monad ListI where -->
<!-- 	  (as >>= f) L c n = -->
