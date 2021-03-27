
module EvalInCxt (
  eval0, eval1, Eval.forceB, Eval.forceBU,
  Eval.app1, (Eval.$$), (Eval.$$$)) where

import Cxt
import Syntax
import Values
import qualified Evaluation as Eval

eval0 :: Cxt -> Tm -> Val
eval0 cxt t = Eval.eval0 (_env cxt) t
{-# inline eval0 #-}

eval1 :: Cxt -> Tm -> Val
eval1 cxt t = Eval.eval1 (_env cxt) t
{-# inline eval1 #-}
