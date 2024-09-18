
# Using lightweight regions to improve GC latency and performance

When we're working in a GC-d language, and we try to minimize latency, a common
approach is to try to reduce the size of the scanned/moved heap, thereby
shrinking the GC's workload.

For example, we might try these tricks in GHC Haskell:

- Use [pinned bytearrays](TODO) to store unboxed data; these are not copied or
  moved by GC, instead they are mark-sweep collected. If we have a relatively
  small number of large arrays, that's very cheap to GC. As a drawback, we can
  only store unboxed data.
- Use foreign allocation, perhaps with manual allocation and freeing; this is
  even faster than pinned bytearrays but of course rather unsafe.  And we still
  can't store boxed Haskell values.
- Use [compact regions](TODO). This feature allows us to copy Haskell objects to
  a region, maintaining the property that regions only contain internal
  pointers. Compact regions are not scanned or moved by GC, and they remain
  alive as long as there's one live object in them. Compact regions are good for
  storing long-lived or persistent data. We can't store thunks, closures and
  mutable objects though.

Ideally, we can shrink the scanned & moved heap to a point where GC runs are
compatible with low-latency or perhaps realtime applications.








<!-- # A design for combining garbage collection with memory regions -->

<!-- ## Introduction -->

<!-- Latency is a common reason for not using garbage collection. Instead, people -->
<!-- tend to use a mix of manual and region/lifetime memory management. Low-latency -->
<!-- GC is also a thing, but it adds complexity to the RTS implementation and -->
<!-- overheads to mutator execution, and it's difficult to get to the same overall -->
<!-- level of performance. -->

<!-- The root cause of latency issues is simply that the GC has too much work to do. -->
<!-- In this document I propose a system which lets programmers lighten GC workload, -->
<!-- by shrinking the size of the heap that's scanned and/or moved by GC. -->

<!-- It's inspired by **compact regions** in GHC: -->
<!-- http://ezyang.com/papers/ezyang15-cnf.pdf -->

<!-- The API of compact regions is mostly just two operations: creating a new region -->
<!-- and copying existing objects into them. -->

<!-- - Copying is deep, except for pointers that already point to the region; -->
<!--   then it's shallow. This ensures that there are only internal pointers in a -->
<!--   region. -->
<!-- - A region is kept alive as long as there is one live object in it. -->
<!-- - Regions are not scanned or moved by garbage collection. If GC finds a pointer -->
<!--   into the region, the region is kept alive but the pointer is not followed. -->
<!-- - Compact regions cannot contain mutable objects (including thunks) and -->
<!--   closures. -->

<!-- This has two main use cases. First, eliminating the GC cost of long-lived data. -->
<!-- Second, serialization: a compact region can be serialized as it is. We only need -->
<!-- to remember and store the original base address of the region, to be able to -->
<!-- relocate the region to a new address when we deserialize it. I am mostly -->
<!-- interested in the GC performance aspects here. -->

<!-- As to other region-based systems that I've seen, they don't add GC into the mix -->
<!-- or use it differently: -->

<!-- - In Rust, the lifetime system allows arenas and fine-grained object allocation, -->
<!--   but lifetimes are determined statically instead of by dynamic reachability. -->
<!-- - Likewise in MLKit, region lifetimes are inferred at compile time. We can use -->
<!--   [regions and GC at the same -->
<!--   time](https://dl.acm.org/doi/pdf/10.1145/512529.512547), but the GC scans -->
<!--   contents of regions all the same; the benefit of the setup is that region -->
<!--   freeing reclaims a large amount of memory that would otherwise become -->
<!--   collectible garbage. -->

<!-- **Short summary** of the design. -->

<!-- - We have a two-stage language with a simply typed object language and a -->
<!--   dependently typed metalanguage. -->
<!-- - Boxed object-level datatypes are parameterized by *locations*: a location is a -->
<!--   region or the general heap. Constructors of a type allocate into a specified -->
<!--   region. -->
<!-- - The object language supports creation of new regions and universal and -->
<!--   existential region quantification. -->
<!-- - A region is alive if it is itself reachable (regions can be passed as -->
<!--   arguments or stored in existentials) or any object in the region is reachable. -->
<!-- - We use tag-free GC where we generate GC code for each monomorphic datatype. -->
<!-- - Since the object language is monomorphic, types provide fairly precise -->
<!--   information about pointers that are possibly stored in values. -->
<!-- - The per-datatype GC logic exploits this information. For example: -->
<!--   - A value which lives in a region and contains pointers only to regions is -->
<!--     never scanned or moved, because the involved regions are already known to be -->
<!--     alive, purely by the fact that they appear in the *type*. -->
<!--   - A value which lives in a region but which may contain heap pointers or -->
<!--     existential regions, is scanned but not moved. -->
<!--   - A value which lives on the general heap is scanned and moved. -->











<!-- In the following I generally assume a copying precise GC as the basis, to be -->
<!-- extended with extra features. The main goal is to reduce the size of the scanned -->
<!-- heap to a point where stop-the-world collection is still compatible with -->
<!-- realtime applications. Hence, the GC should be generally optimized for -->
<!-- throughput and low mutator overheads. -->

<!-- ## 1. Untyped DAG of regions -->

<!-- The obvious limitation of compact regions is that they are compact. It would be -->
<!-- highly useful to have, say, two regions `A` and `B` such that `A` is compact and -->
<!-- `B` may contain pointers to itself and `A`. If `B` is dead, it gets freed by GC, -->
<!-- but `A` may be still alive. Let's have the following: -->

<!-- - Creating a new region is parameterized over a list of existing *parent -->
<!--   regions*. We say that the region is a *child* of all parents. -->
<!-- - A region `A` is alive if it contains a live object, or if one of its -->
<!--   transitive child regions is alive. -->
<!-- - Adding a heap object to a region `A` works like this: the object is deeply -->
<!--   copied, except for pointers to any transitive parent region of `A`, which are -->
<!--   shallowly copied. -->

<!-- Optionally, we may allow directly allocating objects into a region. This -->
<!-- would look like an extended syntax for algebraic data constructors: -->

<!-- ``` -->
<!--     data List a = Nil | Cons a (List a) -->

<!-- 	enum Bool = True | False -->

<!-- 	main = -->
<!-- 	  let r1 = newRegion [] -->
<!-- 	  let r2 = newRegion [r1] -->
<!-- 	  let foo = Cons@r1 True Nil@r1 -->
<!-- 	  let bar = Cons@r2 False foo -->
<!-- ``` -->
<!-- Here, `Bool` is an unboxed enum type, so there's no point in specifying -->
<!-- regions for its values. -->

<!-- Since a new region can be a child of any collection of existing regions, we get -->
<!-- a DAG (directed acyclic graph) of regions at runtime. -->

<!-- Regions are not indicated in types here, so programmers have to be mindful of -->
<!-- them. GHC's compact regions are the same in this respect. -->

<!-- ### 1.1 Implementation -->

<!-- The lack of region typing yields some runtime overhead. In short, adding or -->
<!-- allocating objects into a region has to dynamically check for pointers into -->
<!-- parent regions. -->

<!-- There are many ways to implement this. My idea for "idiomatic" usage is that -->
<!-- regions should be relatively few and region creation should be relatively rare. -->
<!-- Let's look at one concrete design. -->

<!-- First, about memory layouts: -->

<!-- - Every heap pointer uses a single tag bit to signal if it points to the GC-d heap -->
<!--   or a region. -->
<!-- - Each region has a unique integer id. The id-s get densely reassigned by -->
<!--   by GC at runtime. -->
<!-- - Regions are allocated in chunks of some power-of-2 size. Chunks are aligned on -->
<!--   their size. The start of each chunk contains some metadata including the -->
<!--   region id. -->
<!-- - For each heap object that's in a region and is smaller than the chunk size, we -->
<!--   can find the metadata by masking the address of the object. -->
<!-- - For larger objects like big arrays, we need to store an extra info pointer in -->
<!--   the object itself. -->
<!-- - A region at runtime is a pair of pointers, one pointing to beginning of the -->
<!--   free area and one to the end, in the current chunk. -->
<!-- - At the end of the chunk there is a bitarray indexed by region id-s. The bitarray -->
<!--   marks all the transitive parents of the current region. We call it the *parent set*. -->

<!-- Copying an object into a region: -->

<!-- - If the object is already stored in the region or a transitive parent of the -->
<!--   region, we copy shallowly, else deeply. We check this by looking up the region -->
<!--   id from the object metadata, and then looking up the id in the target region's -->
<!--   parent set. -->

<!-- Creating a new region: -->

<!-- - We get a new id by incrementing a global counter. -->
<!-- - We compute the region's parent set, allocate a chunk, copy metadata to the -->
<!--   start of the chunk and copy the parent set to the end of the chunk. -->

<!-- Garbage collection: -->

<!-- - We maintain a global list of regions or arrange regions into a linked list. -->
<!-- - We traverse the live heap starting from roots. If a heap object is in a region, -->
<!--   we mark its region in the global list as live, and also mark all of its transitive -->
<!--   parents as live. Otherwise we deeply copy the object. -->
<!-- - We free dead regions at the end of a GC run. Then, we reassign region id-s -->
<!--   consecutively for the live region. This requires recomputing all the parent -->
<!--   sets in region chunks, potentially shrinking them. We do this in-place, so the -->
<!--   shrinking of parent sets might leave behind unused space (a very small amount -->
<!--   of space, so we don't care). -->

<!-- Mutable references (like Haskell's `IORef`) pose some difficulty. The problem is -->
<!-- that we generally cannot deep copy mutable references, because that un-shares -->
<!-- mutable state. -->

<!-- - It always possible to *create* and initialize a new mutable reference that -->
<!--   lives in a region. When we initialize the reference, the initial value gets -->
<!--   added to the region as usual. -->

<!-- - We may allow adding mutable references to a region with the possibility of -->
<!--   runtime failure. If a mutable ref is already stored in the target region or -->
<!--   one of its parents, we're fine. -->

<!-- - Alternatively, we could implement a "write barrier", e.g. add written objects -->
<!--   to a "remembered set" for each region, and make GC traverse the remembered -->
<!--   objects of each live region. This could be convenient and fairly efficient -->
<!--   as longs as it's not overused. -->

<!-- ### 1.2 Discussion -->

<!-- I'm not super fond of the runtime overheads of this system. When adding objects -->
<!-- to regions and when running GC, we have to access region metadata for every -->
<!-- pointer. -->

<!-- To be clear, the compiler can optimize away a lot of runtime checks. E.g. when -->
<!-- adding `[1, 2, 3, 4, x, y]` as a linked list literal to a region, we can elide -->
<!-- the region check on all cons cells and all numerals, and we only need to check -->
<!-- `x` and `y` if they are dynamically passed values in the scope. -->

<!-- The system may not be much worse than GHC, where the info pointer of every -->
<!-- object is dereferenced during GC, and where pointers into compact regions are -->
<!-- detected at runtime. But I'd like to do better. -->

<!-- # 2. Typed DAG of regions -->

<!-- Let's remove almost all runtime overheads through typing. -->

<!-- All runtime data types which are dynamically allocated (i.e. not always stored -->
<!-- in registers and on the stack) have a *location* attached. -->

<!-- Locations are either `Hp : Loc` or `Reg r : Loc` where `r : Region`. -->
<!-- `Loc` and `Region` are both *kinds* in this system, i.e. they are not -->
<!-- first-class types. -->

<!-- Lists of unboxed `Int`-s are declared like this: -->
<!-- ``` -->
<!--     data List (l : Loc) := Nil | Cons Int (List l) -->
<!-- ``` -->
<!-- `Region` supports universal and existential quantification, so we can write -->
<!-- ``` -->
<!--     foo : {r r'} -> List (Reg r) -> List (Reg r') -->
<!--     foo ns = case ns of Nil       -> Nil -->
<!-- 	                    Cons n ns -> Cons (n + 100) (foo ns) -->
<!-- ``` -->
<!-- Here, type inference fills in the implicit location parameters of `Nil` and `Cons` occurrences. -->
<!-- We can also write `Nil {Reg r}` and `Cons {Reg r}` if we need to. -->

<!-- We can use some sugar. For one, we may default to `Hp` if regions are not -->
<!-- explicitly marked. -->

<!-- ``` -->
<!--     foo : List -> List -->
<!-- 	foo ns = case ns of Nil -> Nil -->
<!-- 	                    Cons n ns -> Cons (n + 100) (foo ns) -->
<!-- ``` -->
<!-- For another one, we may elide `Reg`: -->
<!-- ``` -->
<!--     foo : {r r'} -> List r -> List r' -->
<!--     foo ns = case ns of Nil       -> Nil -->
<!-- 	                    Cons n ns -> Cons (n + 100) (foo ns) -->
<!-- ``` -->

<!-- These examples are actually in the *runtime language*, but we also have a -->
<!-- *compile time* language fragment. We closely follow my [recent -->
<!-- paper](https://andraskovacs.github.io/pdfs/2ltt_icfp24.pdf). -->

<!-- - The object language *only* supports universals and existentials for -->
<!--   regions. There's no polymorphism for locations and types. -->
<!-- - The object language is polarized; it has value types (`ValTy`) and computation -->
<!--   types (`CompTy`). These are both sub-universes of `Ty`, i.e. we can "forget" -->
<!--   about polarity by casting a value or computation type to `Ty`. -->
<!-- - The metalanguage is dependently typed. Here we can abstract over anything and -->
<!--   write metaprograms which generate object code. -->

<!-- For instance, even though the runtime language has no support for polymorphism over locations, -->
<!-- we can write a `map` which is generic over types and locations: -->

<!-- ``` -->
<!--     data List (l : Loc) (A : ValTy) := Nil | Cons A (List l A) -->

<!--     map : {l l' : Loc}{A B : ValTy} -> (↑A -> ↑B) -> ↑(List l A) → ↑(List l' B) -->
<!-- 	map f as = <letrec go as := case as of Nil -> Nil -->
<!--                                            Cons a as -> Cons ~(f <a>) (go as); -->
<!--                 go ~as> -->
<!-- ``` -->
<!-- Or, with more sugar and stage inference: -->
<!-- ``` -->
<!--     data List l A := Nil | Cons A (List l A) -->

<!--     map : {l l'}{A B : ValTy} -> (A -> B) -> List l A -> List l' B -->
<!-- 	map f = letrec go as := case as of Nil       -> Nil -->
<!--                                        Cons a as -> Cons (f a) (go as) -->
<!--             go -->
<!-- ``` -->
<!-- This `map` may generate multiple different object-level functions when used. -->
<!-- For example, the previous region-polymorphic `foo` is recovered like this: -->
<!-- ``` -->
<!--     foo : {r r'} -> List r Int -> List r' Int -->
<!-- 	foo ns = map (λ n. n + 100) ns -->
<!-- ``` -->
<!-- Here we use stage inference and implicit `Reg`. The heap-allocated version: -->
<!-- ``` -->
<!--     foo : List Hp Int -> List Hp Int -->
<!-- 	foo ns = map (λ n. n + 100) ns -->
<!-- ``` -->
<!-- As we will see a bit later, the actual machine code has to be different for -->
<!-- the region-based and the heap-based function, hence the need for separate -->
<!-- object code. -->

<!-- **Region creation**: the `region` keyword brings a region name into -->
<!-- scope, much like a `let`-construct: -->

<!-- ``` -->
<!--     main : () -->
<!-- 	main := -->
<!-- 	  region r; -->
<!-- 	  region r'; -->
<!-- 	  let xs : List r Int  := [0, 1, 2, 3, 4, 5]; -->
<!-- 	  let ys : List r' Int := map (*10) xs; -->
<!-- 	  () -->
<!-- ``` -->
<!-- Here, since I gave `List r Int` as the first type, the list literal -->
<!-- gets elaborated to `Cons` cells in the `r` region. Afterwards, the -->
<!-- `List r' Int` similarly causes the location arguments of `map` to be -->
<!-- inferred. -->

<!-- We could use some sugar for setting the expected region of an expression: -->
<!-- ``` -->
<!--     main : () -->
<!-- 	main := -->
<!-- 	  region r; -->
<!-- 	  region r'; -->
<!-- 	  let xs := [0, 1, 2, 3, 4, 5] in r; -->
<!-- 	  let ys := map (*10) xs in r'; -->
<!-- 	  () -->
<!-- ``` -->
<!-- This is kinda similar to [interpretation scopes](https://coq.inria.fr/doc/V8.10.2/refman/user-extensions/syntax-extensions.html#scopes) in Coq. Of course, the exact syntax can be whatever we want. -->

<!-- **Copying into regions**. If we want to have a minimalistic foundation, we don't -->
<!-- add any primitive copying operation. Instead, we can define copying for each -->
<!-- object type. Copying lists: -->

<!-- ``` -->
<!--     copy : {l l'}{A : ValTy} -> List l A -> List l' A -->
<!-- 	copy as = letrec go = \case Nil       -> Nil -->
<!-- 	                            Cons a as -> Cons a (go as); -->
<!--               go as -->
<!-- ``` -->

<!-- However, we can't hope to copy **closures** in this way. Let's look at closures. -->

<!-- - There is a type former `Close : Loc -> CompTy -> ValTy`. For -->
<!--   example, I can write `Close Hp (Int -> Int -> Int)` for a heap-allocated -->
<!--   function closure. The `Int -> Int -> Int` is a computation type here. -->
<!-- - I can pack `t : A : CompTy` as `close t : Close l A`, and unpack `t : Close l -->
<!--   A` as `open t : A`. -->

<!-- Let's write an object-level `map` function (implemented by a single piece of -->
<!-- machine code) which takes a closure as argument: -->

<!-- ``` -->
<!--     map : {l l' l''} → Close l (Int -> Int) -> List l' Int -> List l'' Int -->
<!-- 	map f ns = case ns of Nil       -> Nil -->
<!-- 	                      Cons n ns -> Cons (open f n) (map f ns) -->
<!-- ``` -->
<!-- Recall that this `map` does *not* work on heap-allocated inputs. The `Hp` version -->
<!-- would be a separate object-level implementation. -->

<!-- A closure can capture regions just like any other free variable. This makes it possible -->
<!-- to have an "existential" region purely by closure capture. Here's a function which copies -->
<!-- a list into a "private" region, and returns a closure with indexes into the list: -->

<!-- ``` -->
<!--     privateRegion : List Hp Int -> Close Hp (Int -> Int) -->
<!-- 	privateRegion ns = -->
<!-- 	  region r; -->
<!-- 	  let ns' := copy ns in r; -->
<!-- 	  close (λ n. index n ns'); -->
<!-- ``` -->
<!-- This is somewhat convoluted, but it might be useful when we have a large list and -->
<!-- copying it over to a region causes the list to be not traversed or copied again by GC. -->

<!-- In a less convoluted way, we can just pack the list together with an existential region. -->

<!-- ``` -->
<!--     privateRegion : List Hp Int -> ∃ r. List r Int -->
<!-- 	privateRegion ns = region r; (r, copy ns in r); -->
<!-- ``` -->
<!-- The constructor for `∃` is written simply as a pairing. For elimination, it's a positive -->
<!-- `case` or pattern match: -->
<!-- ``` -->
<!--     map : (∃ r. List r Int) -> (∃ r. List r Int) -->
<!-- 	map (r, ns) := -->
<!-- 	  region r'; -->
<!-- 	  let go := \case Nil       -> Nil -->
<!-- 	                  Cons n ns -> Cons (n + 100) (go ns); -->
<!-- 	  (r', go ns) -->
<!-- ``` -->
<!-- We use ample type inference to figure out the regions in `go` from the -->
<!-- `(r', go ns)` expression. -->

<!-- So, back to copying closures! We need to generate at compile time the code that -->
<!-- copies a closure environment from one region to another. Closures may contain -->
<!-- captured existential regions, but otherwise the types of captures are statically -->
<!-- known. -->

<!-- For example -->


<!-- ------------------------------------------------------------ -->
