
module Exceptions (Ex(..), throwIO, catch, throw) where

import qualified Control.Exception as Ex

import Common
import qualified Syntax    as S
import qualified Presyntax as P

throwIO :: Ex -> IO a
throwIO = Ex.throwIO
{-# inline throwIO #-}

catch :: IO a -> (Ex -> IO a) -> IO a
catch = Ex.catch
{-# inline catch #-}

throw :: Ex -> a
throw = Ex.throw
{-# inline throw #-}

data Ex
  = UnifyError0 S.Tm0 S.Tm0
  | UnifyError1 S.Tm1 S.Tm1
  | CVUnifyError CV CV
  | UUnifyError U U
  | forall a. (Show a) => EqUnifyError a a
  | NameNotInScope {-# unpack #-} RawName
  | NoSuchField    {-# unpack #-} RawName
  | NoSuchArgument {-# unpack #-} RawName
  | IcitMismatch Icit Icit
  | NoImplicitLam0
  | ExpectedV
  | FieldNameMismatch Name Name
  | NoNamedLambdaInference
  | CantInfer
  | CantSplice
  | ExpectedNonEmptyRec
  | ExpectedEmptyRec
  | ExpectedEmptyRecCon
  | ExpectedNonEmptyRecCon
  | ExpectedType
  | CantInferTuple
  | ExpectedRecord
  | ExpectedRuntimeType
  | CantInferSigma
  | ExpectedDataCon

  -- raw unification exception
  | CantUnify

  -- renaming
  | OccursCheck MetaVar
  | OutOfScope Lvl

  -- Exception with elaboration context
  | ElabError S.Locals P.Tm Ex

deriving instance Show Ex

instance Ex.Exception Ex
