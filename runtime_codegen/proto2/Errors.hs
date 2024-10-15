
module Errors where

import Control.Exception
import Text.Printf
import Data.IORef

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
          "Unsolved metavariable.\n" ++
          "Type: " ++ showTm (emptyCxt (initialPos "")) a


  let locMsg = displayLocation (pos cxt) file

  putStrLn locMsg
  putStrLn msg
