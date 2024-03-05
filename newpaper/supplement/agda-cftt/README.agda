
-- Agda implementation of CFTT. Modules are best viewed in the order here.
--------------------------------------------------------------------------------

-- Building: I used Agda 2.6.2.2 and standard library version 1.7

{-
We reproduce a small Haskell-style monad transformer library here, using
instance arguments. The main difference to Haskell is the lack of "functional
dependencies" between class parameters, which compels us to instead use
associated types in MonadReader and MonadState.
We use Agda's "Set" as the equivalent of "MetaTy" in the paper.
-}
open import Lib

{-
The object theory is postulated in a HOAS (higher-order abtract syntax) style,
where meta-level functions are used instead of a primitive notion of
object-level binding.

This is equaivalent both in syntax and semantics to the quote-splice
presentation of CFTT, but it is much easier to embed in Agda.

In general, HOAS-style 2LTT is a viable alternative syntax to quote/splice 2LTT.
If we don't have quote/splice inference, HOAS looks cleaner. However,
quote/splice is more convenient in staging implementation and elaboration
internals, and if we do have stage annotation inference then there isn't much
difference between the surface syntaxes.
-}
open import Object

-- The Gen monad, plus the strict local', put' and modify' functions.
open import Gen

-- The Improve class
open import Improve

-- The MonadJoin class
open import Join

-- The Split class
open import Split

-- An implementation of monadic tail calls.
open import MonadTailCall

-- Sums-of-products of value types. Also postulates generativity.
open import SOP

-- Pull streams. The extra feature compared to the paper is the tracking for
-- whether streams can skip, and using that to optimize zipping.
open import Pull

open import Examples
