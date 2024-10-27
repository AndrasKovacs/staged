
module Errors where

import Control.Exception
import Text.Printf
import Data.IORef
import Data.List

import Common
import Cxt
import Syntax
import ElabState

--------------------------------------------------------------------------------

data UnifyException = UnifyException
  deriving (Show, Exception)

data CantUnify
  = ExpectedInferred
  | LamBinderType
  | Placeholder
  deriving Show

data ElabError
  = NameNotInScope Name
  | CantUnify Tm Tm CantUnify
  | InferNamedLam
  | NoNamedImplicitArg Name
  | IcitMismatch Icit Icit
  | UnsolvedMetaInZonk MetaVar Tm
  | DuplicateRecField Name
  | CantInferRecord
  | ExpectedRecTy Tm
  | NoSuchField Name
  | TooManyFields
  | MissingFields [Name]
  | CantInferFieldName
  deriving (Show, Exception)

data Error = Error Cxt ElabError
  deriving (Show, Exception)

displayError :: Error -> IO ()
displayError (Error cxt e) = do

  file <- readIORef sourceCode

  let msg = case e of
        NameNotInScope x ->
          "Name not in scope: " ++ x
        CantUnify t t' ExpectedInferred  ->
          ("Cannot unify expected type\n\n" ++
           "  " ++ showTm cxt t ++ "\n\n" ++
           "with inferred type\n\n" ++
           "  " ++ showTm cxt t')
        CantUnify t t' LamBinderType   ->
          ("Cannot unify expected lambda binder type\n\n" ++
           "  " ++ showTm cxt t ++ "\n\n" ++
           "with given type annotation\n\n" ++
           "  " ++ showTm cxt t')
        CantUnify t t' Placeholder   ->
          ("Cannot unify value\n\n" ++
           "  " ++ showTm cxt t ++ "\n\n" ++
           "with expected value\n\n" ++
           "  " ++ showTm cxt t')
        InferNamedLam ->
          "Cannot infer type for lambda with named argument"
        NoNamedImplicitArg name ->
          "No named implicit argument with name " ++ name
        IcitMismatch i i' -> printf
          ("Function icitness mismatch: expected %s, got %s.")
          (show i) (show i')
        UnsolvedMetaInZonk x a ->
          "Unsolved metavariable. Expected type:\n\n  " ++
          showTm cxt a ++ "\n"
        DuplicateRecField x ->
          "Duplicate record field: " ++ x
        CantInferRecord ->
          "Can't infer type for record"
        ExpectedRecTy a ->
          "Expected a record type, inferred:\n\n  " ++ showTm cxt a
        NoSuchField x ->
          "No such record field: " ++ x
        MissingFields xs ->
          "Missing record field definitions: " ++ (intercalate ", " xs)
        TooManyFields ->
          "Too many record field definitions"
        CantInferFieldName ->
          "Can't infer field name for record expression. Tip: give a field name."

  let locMsg = displayLocation (pos cxt) file

  putStrLn locMsg
  putStrLn msg
