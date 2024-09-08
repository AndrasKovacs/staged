
# Region graph memory allocation

In Haskell, the garbage collection is not optimized for latency very much, but
there are some features and tricks which can be still used to get good latency.
Generally, if we have a stop-the-world GC, the pauses that we get are determined
by the amount of data that has to be scanned and/or copied in a run. Hence, the
obvious way to improve latency is to reduce the size of that data.

- Instead of using small heap objects containing many heap pointers, we can use
  a smaller number of large pointer-free objects. In Haskell, we can create
  "pinned" byte arrays. These don't contain heap pointers and are not copied
  or scanned by GC, and are freed by mark-sweeping.
- *Compact regions* are regions which only contain pointers pointing to inside
  the region. Compact regions are also not scanned or copied. They are garbage
  collected when there is no live reference to any of the objects inside the
  region. Hence, the whole region is kept alive as long as there's one live
  object in the region.

Pinned arrays and compact regions make it possible to have a large amount of
GC-ed memory which is extremely cheap to GC, at the cost of potentially
retaining more memory than necessary. However, they're kinda rigid and limited
so I was motivated to think of a more convenient generalization.

## Region graphs

An obvious extension of compact regions is to allow regions to contain pointers
to specified other regions. For example, we might want to have two regions:

- Region A is compact
- Region B may contain pointers to its own objects and objects in A.

Now, B can be freed without freeing A. This is not the whole story though; I
want to guarantee that *no* region gets scanned or copied by GC. This is
possible if A is kept alive as long as B is alive, regardless of B containing an
actual pointer to an object in A.

I sketch a hypothetical programming language. Types can be parameterized over
regions:

    data Tree r r' (a : Type r) : Type r' = Leaf a | Node (Tree r r' a) (Tree r r' a)

`Tree r r'` can contain pointers to objects in `r` and `r'` and itself lives in `r'`.

We might want to have a design where `r'` is implicit for all ADTs, and we allow
any ADT to inhabit any region.

There's a distinguished "heap" region which is the ordinary fine-grain GC-d
heap. Elaboration defaults the region of a type to the generic heap.

Regions can be
