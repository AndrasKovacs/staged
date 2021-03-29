
module EvalInCxt (
    eval, eval0, eval1
  , Eval.forceF, Eval.forceFU
  , Eval.app1, (Eval.$$), (Eval.$$$)
  , quote
  ) where

import Common
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

eval :: Cxt -> Tm s -> U s -> Val s
eval cxt t u = Eval.eval (_env cxt) t u
{-# inline eval #-}

quote :: Cxt -> Val s -> Tm s
quote cxt t = Eval.quote (_lvl cxt) DontUnfold t
{-# inline quote #-}
