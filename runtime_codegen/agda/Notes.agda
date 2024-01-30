
module Notes where

open import Lib
open import Syntax
open import Embedding
open import Substitution

--------------------------------------------------------------------------------

{-
Stage 0   : no beta redexes
Stage suc : all beta redexes possible

Semantic values: at stage 0 everything can be neutral, including functions

evaluation at stage suc:
downshifting stages is possible
-}
