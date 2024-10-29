
module Cxt (
    module Cxt
  , module Cxt.Type) where

import qualified Data.Map.Strict as M

import Common
import Evaluation
import Pretty
import Syntax
import Value
import Cxt.Type

--------------------------------------------------------------------------------

showVal :: Cxt -> Val -> String
showVal cxt v =
  prettyTm True 0 0 (names cxt) (quote (lvl cxt) v) []

showTm :: Cxt -> Tm -> String
showTm cxt t = prettyTm True 0 0 (names cxt) t []

-- showTm :: Cxt -> Tm -> String
-- showTm cxt t = show t

emptyCxt :: SourcePos -> Cxt
emptyCxt = Cxt [] 0 LHere [] mempty mempty

-- | Extend Cxt with a bound variable.
bind :: Cxt -> Name -> VTy -> Cxt
bind (Cxt env l ls pr topns ns pos) x ~a =
  Cxt (env :> VVar l)
      (l + 1)
      (LBind ls x (quote l a))
      (pr :> Just Expl)
      topns
      (M.insert x (l, a) ns)
      pos

-- | Extend Cxt with a bound variable.
bindTop :: Cxt -> Name -> VTy -> Cxt
bindTop (Cxt env l ls pr topns ns pos) x ~a =
  Cxt (env :> VVar l)
      (l + 1)
      (LBind ls x (quote l a))
      (pr :> Just Expl)
      (M.insert x (l, a) topns)
      ns
      pos

-- | Insert a new binding. This is used when we insert a new implicit lambda in
--   checking.
newBinder :: Cxt -> Name -> VTy -> Cxt
newBinder (Cxt env l ls pr topns ns pos) x ~a =
  Cxt (env :> VVar l)
      (l + 1)
      (LBind ls x (quote l a))
      (pr :> Just Expl)
      topns
      ns                        -- Unchanged! An inserted binder cannot be accessed from
      pos                       -- source syntax

-- | Extend with a definition. We require both terms and values, for efficiency,
--   because when we elaborate let-definitions, we usually already have terms
--   for the definition and its type.
define :: Cxt -> Name -> Tm -> Val -> Ty -> VTy -> Cxt
define (Cxt env l ls pr topns ns pos) x ~t ~vt ~a ~va  =
  Cxt (env :> vt)
      (l + 1)
      (LDefine ls x a t)
      (pr :> Nothing)
      topns
      (M.insert x (l, va) ns)
      pos

valToClosure :: Cxt -> Val -> Val -> Val
valToClosure cxt t u = eval (u:env cxt) $ quote (lvl cxt + 1) t

defineTop :: Cxt -> Name -> Tm -> Val -> Ty -> VTy -> Cxt
defineTop (Cxt env l ls pr topns ns pos) x ~t ~vt ~a ~va  =
  Cxt (env :> vt)
      (l + 1)
      (LDefine ls x a t)
      (pr :> Nothing)
      (M.insert x (l, va) topns)
      ns
      pos
