
module InCxt (
    eval0, eval1
  , Eval.forceF0, Eval.forceFU0, Eval.forceF1, Eval.forceFU1
  , Eval.up, Eval.down
  , unifyCxt
  , Eval.app1, (Eval.$$), (Eval.$$$)
  , quote0, quote1
  -- , unify0, unify1
  , Unif.freshMeta
  , Unif.freshCV
  ) where

import Common
import Cxt
import Syntax
import Values
import qualified Evaluation as Eval
import qualified Unification as Unif

unifyCxt :: Cxt -> Unif.Cxt
unifyCxt cxt = Unif.Cxt (cxt^.lvl) (cxt^.names)
{-# inline unifyCxt #-}

eval0 :: Dbg => Cxt -> Tm0 -> Val0
eval0 cxt t = Eval.eval0 (cxt^.env) t
{-# inline eval0 #-}

eval1 :: Dbg => Cxt -> Tm1 -> Val1
eval1 cxt t = Eval.eval1 (cxt^.env) t
{-# inline eval1 #-}

quote0 :: Dbg => Cxt -> Val0 -> Tm0
quote0 cxt t = Eval.quote0 (cxt^.lvl) UnfoldNone t
{-# inline quote0 #-}

quote1 :: Dbg => Cxt -> Val1 -> Tm1
quote1 cxt t = Eval.quote1 (cxt^.lvl) UnfoldNone t
{-# inline quote1 #-}

-- unify0 :: Dbg => Cxt -> Val0 -> Val0 -> IO ()
-- unify0 cxt t t' = Unif.unify0 (unifyCxt cxt) CSRigid t t'
-- {-# inline unify0 #-}

-- unify1 :: Dbg => Cxt -> Val1 -> Val1 -> IO ()
-- unify1 cxt t t' = Unif.unify1 (unifyCxt cxt) CSRigid t t'
-- {-# inline unify1 #-}
