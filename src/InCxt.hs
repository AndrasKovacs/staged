
module InCxt (
    eval0, eval1
  , Eval.forceF0, Eval.forceFU0, Eval.forceF1, Eval.forceFU1, Eval.forceCV
  , Eval.app1, (Eval.$$), (Eval.$$$)
  , quote0, quote1
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

quote0 :: Cxt -> Val0 -> Tm0
quote0 cxt t = Eval.quote0 (_lvl cxt) DontUnfold t
{-# inline quote0 #-}

quote1 :: Cxt -> Val1 -> Tm1
quote1 cxt t = Eval.quote1 (_lvl cxt) DontUnfold t
{-# inline quote1 #-}
