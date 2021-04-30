
module Exceptions (throwIO, catch, throw, module Exceptions) where

import Control.Exception
import qualified Data.ByteString as B

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
  | forall a. (Show a) => UnifyEq a a
  | SolutionError V.Val1 V.Val1 SolutionEx
deriving instance Show UnifyEx

-- | Unification exception in local unification context.
data UnifyInner = UnifyInner Unif.Cxt UnifyEx
  deriving Show

-- | Exception thrown during elaboration
data ElabEx
  = UnifyOuter V.Val1 V.Val1 UnifyInner
  | NoSuchField S.Tm1 RawName
  | NoSuchArgument RawName
  | NameNotInScope RawName
  | IcitMismatch Icit Icit
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
  | ExpectedRecord S.Ty
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
