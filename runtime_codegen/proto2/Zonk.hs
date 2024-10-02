
{-
Zonking + erasure

Erasure of
  - types
  - unit
  - icitness

We keep names around for readability downstream.

Translation:
1. Accummulate spines headed by metas or postponing, normalize tham
   Throw error on unsolved meta.
2. Pass type of interpreted term, when it's U or Unit, replace with Erased

-}

module Zonk where

import Common

data Tm
  = Var Ix
  | TopVar Lvl
  | Let Name Tm Tm
  | Lam Name Tm
  | App Tm Tm
  | Erased
  | Quote Tm
  | Splice Tm
  | Return Tm
  | Bind Name Tm Tm
  | New Tm
  | Write Tm Tm
  | Read Tm
  deriving Show
