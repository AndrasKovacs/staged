
module Syntax where

import Common

type Ty = Tm

-- | A `Pruning` represents a spine of variables, which contains a subsequence
--   of all variables in scope. A `Just` represents application to a var, a `Nothing`
--   skips over a var.
type Pruning = [Maybe Icit]

-- | A reversed pruning. Used for pruning Pi domains, where we have to iterate
--   inside-out.
newtype RevPruning = RevPruning Pruning

revPruning :: Pruning -> RevPruning
revPruning = RevPruning . reverse

-- | A "context zipper", used for efficiently creating types for fresh metas.
data Path
  = Here
  | Define Path Name ~Ty Stage ~Tm
  | Bind Path Name ~Ty Stage
  deriving Show

-- | Convert type in context to a closed iterated Pi type.  Note: we need `Tm`
--   and `Ty` in path in order to make this operation efficient. With this, we
--   can simply move things over from `Path` without having to rename or quote
--   anything.
closeTy :: Path -> Ty -> Ty
closeTy mcl b = case mcl of
  Here               -> b
  Bind mcl x a s     -> closeTy mcl (Pi x Expl a b)
  Define mcl x a s t -> closeTy mcl (Let s x a t b V1)

data Tm
  = Var Ix
  | Lam Name Icit Tm Tm Verbosity
  | App Tm Tm Icit Verbosity
  | AppPruning Tm Pruning  -- ^ Used for applying a pruned meta to a mask of the scope.
  | InsertedMeta MetaVar Pruning
  | Pi Name Icit Ty Ty
  | Let Stage Name Ty Tm Tm Verbosity
  | Meta MetaVar

  | U Stage
  | Quote Tm
  | Splice Tm
  | Lift Ty

  | Wk Tm -- ^ Explicit weakening, used in subtyping coercions

  | Nat Stage
  | Zero Stage
  | Suc Stage
  | NatElim Stage
  deriving Show

tQuote :: Tm -> Tm
tQuote (Splice t) = t
tQuote t          = Quote t

tSplice :: Tm -> Tm
tSplice (Quote t) = t
tSplice t         = Splice t

appE0 :: Tm -> Tm -> Tm
appE0 t u = App t u Expl V0

tSuc :: Stage -> Tm -> Tm
tSuc st t = Suc st `appE0` t

tNatElim :: Stage -> Tm -> Tm -> Tm -> Tm -> Tm
tNatElim st p s z t = NatElim st `appE0` p `appE0` s `appE0` z `appE0` t
