
# Ideas for mixed garbage collection and region allocation

GHC has a feature, called **compact regions**
(http://ezyang.com/papers/ezyang15-cnf.pdf), which is an interesting "dynamic"
flavor of region-based memory management.

The API: we can create new compact regions, then copy existing GHC objects into
them.

- This is a deep copy, except that if a pointer already points to the region, we
  only copy the pointer. This ensures that there are only internal pointers in a
  region.
- A region is kept alive as long as there is one live object in it.
- Regions are not scanned or moved by garbage collection. If GC finds a pointer
  into the region, the region is kept alive but the pointer is not followed.
- Compact regions cannot contain mutable objects (including thunks) and
  closures.

Use cases:

- Eliminating the GC cost of long-lived data.
- Serialization: a compact region can be serialized as it is. We only need to
  remember and store the original base address of the region, to be able to
  relocate the region when we deserialize it.

*Latency* is probably the most common reason for not using GC in practice. I'm
going to speculate here about mixing regions and GC to hopefully get a good trade-off



In the following I speculate about ways to use regions which are never scanned
or copied by GC.

I generally assume a copying precise GC as the basis to be extended with extra
features.

## 1. Dynamic DAG of regions

The obvious limitation of compact regions is that they are compact. It would be
highly useful to have, say, two regions `A` and `B` such that `A` is compact and
`B` may contain pointers to itself and `A`. If `B` is dead, it gets freed by GC,
but `A` may be still alive. Let's have the following:

- Creating a new region is parameterized over a list of existing *parent
  regions*. The region is the *child* of its parents.
- A region `A` is alive if either one of its objects is alive, or if one of
  its transitive child regions is alive.
- Adding a heap object to a region `A` works like this: the object is deeply
  copied except for pointers to any transitive parent region of `A`, which are
  shallowly copied.

Optionally, we may allow directly allocating objects into a region. This
would look like an extended syntax for algebraic data constructors:

```
    data List a = Nil | Cons a (List a)

	enum Bool = True | False

	main =
	  let r1 = newRegion []
	  let r2 = newRegion [r1]
	  let foo = Cons@r1 True Nil@r1
	  let bar = Cons@r2 False foo
```
Here, `Bool` is an unboxed enum type, so there's no point in specifying
regions for its values.

Since a new region can be child of any collection of existing regions, we get a
DAG (directed acyclic graph) of regions at runtime.

Regions are not indicated in types here, so programmers have to be mindful of
them. GHC's compact regions are the same in this respect. The lack of typing
yields some runtime overheads as well.

### Implementation

Adding or allocating objects into a region has to dynamically check for pointers
into parent regions.

There are many ways to implement this. My idea for idiomatic usage is that
regions should be relatively few and region creation should be relatively rare.
One design:

- Each region has a unique integer id.
- Garbage collection reassigns id-s contiguously to live regions.
- A region at runtime is a pair of pointers, one pointing to beginning of the
  free area and one to the end, in the current chunk.
- At the end of the chunk there is a bitarray indexed by region id-s. The entry
  for the region itself is set and so are the entries for all transitive
  parents.
- Regions are allocated in chunks of some power-of-2 size. Chunks are aligned on
  their size. The start of each chunk contains some metadata.
- For each heap object that's smaller than the chunk size, we can find its
  metadata by masking the address of the object.
- For larger objects like big arrays, we need to store an extra info pointer in
  the object itself.
- Every heap pointer uses a single tag bit to signal if it points to the GC-d heap
  or a region.
- When we try to copy a pointer into a region, we check if it's a GC-d heap
  pointer. If so, we dereference and recursively copy. Otherwise we look up the
  pointer's metadata, extract its region id from there, and check its entry in
  the target region's bitarray. If the pointer points to a parent region, we
  copy shallowly, else we copy recursively.
- When we create a new region, we get a new id by incrementing a counter and
  then compute the region's bitarray. The existing region's bitarrays are not
  modified, which is fine because they cannot have the new region as a
  parent. This means though that when we index into bitarrays we need to check
  for out-of-bounds access, in which case the entry is taken to be not set.




















------------------------------------------------------------
