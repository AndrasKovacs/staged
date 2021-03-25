
module EvalInCxt (
  eval0, eval1, Eval.forceF1, Eval.forceF0,
  Eval.forceFU1, Eval.forceFU0, Eval.app, (Eval.$$), (Eval.$$$)) where

import Cxt
import Syntax
import Values
import qualified Evaluation as Eval

eval0 :: Cxt -> Tm0 -> Val0
eval0 cxt t = Eval.eval0 (_env cxt) t
{-# inline eval0 #-}

eval1 :: Cxt -> Tm1 -> Val1
eval1 cxt t = Eval.eval1 (_env cxt) t
{-# inline eval1 #-}
