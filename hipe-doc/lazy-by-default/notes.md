https://www.reddit.com/r/haskell/comments/15ufo6k/laziness_in_haskell_part_2_why_not_strict_haskell/

https://www.reddit.com/r/haskell/comments/wpbs4z/what_things_would_be_awkward_to_do_in_a/ikhwau4/

Purported laziness benefits that are orthogonal to laziness
  - optimization modulo observational equivalence
  - purity (i.e. side effect tracking)
  - imprecise/non-strict exception semantics (see e.g. poison value in LLVM)
  - TCO, stack safety

Unmotivated focus on having a semantics which avoids bottoms.
  - Every correct program is *total*.
  - By bottom I mean unrecoverable errors and divergence.
  - Avoiding those bottoms is *bad*. I want to learn about program errors!
    I absolutely don't want to pay non-trivial runtime cost just to hide my own errors from me.

Use cases for laziness, for which laziness is a bad solution.
  - Short-circuiting: should come from staging and/or fusion
    GHC itself tries *very* hard to not use laziness for short-circuiting
  - Compositionality:
    - very bad & unrealistic examples, like (head . selectionSort)
    - should come from staging and codata

Real use cases for laziness don't benefit significantly from laziness-by-default
  - Lazy amortization must be carefully set up for specific data structures.
  - Essential laziness is not that common (s.t. we don't know if a computation will be demanded)

- No evidence that GHC has an optimization advantage because of laziness.
- Plenty of evidence to the contrary. GHC works very hard to get rid of
  laziness, just to level the playing field, and only recently has GHC become
  able to compile *trivial strict programs* without significant runtime penalty
  from laziness (thanks to tag inference, worker-wrapper-cbv, nested CPR)

- Laziness is easily the least successful Haskell feature in terms of adoption elsewhere.
  - Haskell is the *last* major programming language which is lazy by default.
    No example since 1990!!!
