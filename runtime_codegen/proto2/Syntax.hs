
module Syntax where

import Common

--------------------------------------------------------------------------------

type Ty = Tm

-- | A `Pruning` represents a spine of variables, which contains a subsequence
--   of all variables in scope. A `Just` represents application to a var, a
--   `Nothing` skips over a var.
type Pruning = [Maybe Icit]

-- | A reversed pruning. Used for pruning Pi domains, where we have to iterate
--   inside-out.
newtype RevPruning = RevPruning Pruning

revPruning :: Pruning -> RevPruning
revPruning = RevPruning . reverse

-- | Information about the local binders, used for efficiently creating types for
--   fresh metas.
data Locals
  = LHere
  | LDefine Locals Name ~Ty ~Tm
  | LBind Locals Name ~Ty
  deriving Show

-- | Convert type in context to a closed iterated Pi type.  Note: we need `Tm`
--   and `Ty` in `Locals` in order to make this operation efficient. With this, we
--   can simply move things over from `Locals` without having to rename or quote
--   anything.
closeTy :: Locals -> Ty -> Ty
closeTy mcl b = case mcl of
  LHere             -> b
  LBind mcl x a     -> closeTy mcl (Pi x Expl a b)
  LDefine mcl x a t -> closeTy mcl (Let x a t b)

-- | Convert a term in context to a closed term by wrapping it in lambdas and
--   let-definitions. The type of the result is given by `closeTy`.
closeTm :: Locals -> Tm -> Tm
closeTm mcl t = case mcl of
  LHere             -> t
  LBind mcl x a     -> closeTm mcl (Lam x Expl t)
  LDefine mcl x a u -> closeTm mcl (Let x a u t)

data Tm
  = Var Ix
  | Lam Name Icit Tm
  | App Tm Tm Icit
  | AppPruning Tm Pruning
  | U
  | Pi Name Icit Ty Ty
  | Let Name Ty Tm Tm
  | Meta MetaVar
  | PostponedCheck CheckVar

  | Box
  | Quote Tm
  | Splice Tm (Maybe String) -- displayed location of the splice

  | Unit
  | Tt

  | Eff
  | Return
  | Bind Name Tm Tm
  | Seq Tm Tm

  | Ref
  | New
  | Write
  | Read
  | Erased String

  | Nat
  | NatLit Integer
  | Suc
  | NatElim

  | RecTy [(Name, Tm)]
  | Rec [(Name, Tm)]
  | Proj Tm Name
  deriving Show

pattern LamI x t = Lam x Impl t
pattern LamE x t = Lam x Expl t
pattern AppE t u = App t u Expl
pattern AppI t u = App t u Impl
pattern NatElim' p s z n = NatElim `AppI` p `AppE` s `AppE` z `AppE` n
pattern Eff' t = Eff `AppE` t
pattern Return' a t = Return `AppI` a `AppE` t
pattern Ref' a = Ref `AppE` a
pattern New' a t = New `AppI` a `AppE` t
pattern Write' a t u = Write `AppI` a `AppE` t `AppE` u
pattern Read' a t = Read `AppI` a `AppE` t
pattern Suc' t = Suc `AppE` t
pattern Box' t = Box `AppE` t


-- | Unfold `AppPruning` to an iterated application to vars. This applies a term to all de Bruijn indices
--   which are `Just` in the mask.
appPruning :: Tm -> Pruning -> Tm
appPruning t pr = go 0 pr where
  go x []              = t
  go x (pr :> Just i)  = App (go (x + 1) pr) (Var x) i
  go x (pr :> Nothing) = go (x + 1) pr
