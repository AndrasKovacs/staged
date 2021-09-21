
module Exceptions (throwIO, catch, throw, module Exceptions) where

import Control.Exception

import Common
import qualified Syntax    as S
import qualified Values    as V
import qualified UnifyCxt  as Unif
import qualified Presyntax as P

-- | Exception thrown while trying to solve a metavar.
data SolutionEx
  = Occurs MetaVar
  | OutOfScope Lvl
  | SpineError
  | NeedExpansion
  | CSFlexSolution
  deriving Show

-- | Exception thrown during unification.
data UnifyEx
  = Unify0 V.Val0 V.Val0
  | Unify1 V.Val1 V.Val1
  | SolutionError V.Val1 V.Val1 SolutionEx
  | UnifyFieldName Name Name
deriving instance Show UnifyEx

-- | Unification exception in local unification context.
data UnifyInner = UnifyInner Unif.Cxt UnifyEx
  deriving Show

-- | Exception thrown during elaboration
data ElabEx
  = UnifyOuter V.Val1 V.Val1 UnifyInner
  | NoSuchFieldName S.Tm1 RawName
  | NoSuchFieldIx   S.Tm1 Int
  | NoSuchArgument RawName
  | NameNotInScope RawName
  | IcitMismatch Icit Icit
  | NegativeFieldIndex
  | NoImplicitLam0
  | ExpectedVal S.Tm1
  | FieldNameMismatch Name Name
  | NoNamedLambdaInference
  | ExpectedNonEmptyRec
  | ExpectedEmptyRec
  | ExpectedEmptyRecCon
  | ExpectedNonEmptyRecCon
  | ExpectedType S.Ty
  | CantInferTuple
  | ExpectedRecord V.Ty
  | ExpectedRuntimeType S.Ty
  | CantInferRec1
  | CantInfer
  deriving Show

-- | Elaboration exception in elaboration context.
data ElabError = ElabError Unif.Cxt P.Tm ElabEx
  deriving Show

instance Exception SolutionEx
instance Exception UnifyEx
instance Exception UnifyInner
instance Exception ElabEx
instance Exception ElabError
