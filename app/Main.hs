
module Main where

import qualified Data.ByteString as B
import qualified Data.Array.Dynamic.L as D
-- import qualified Data.HashMap.Strict  as M

-- import qualified Syntax              as S
-- import qualified Values              as V
import qualified Presyntax           as P
import qualified Evaluation          as Eval

import Common
import Cxt
import ElabState
-- import InCxt
import Exceptions
import Elaboration
import Parser

--------------------------------------------------------------------------------

displayState :: IO ()
displayState = do
  let nl = putStrLn ""

  nl
  putStrLn "Top environment"
  putStrLn (replicate 60 '-')
  nl
  D.for top \case
    TEDef0 a va t vt cv x _ -> do
      putStrLn (show x ++ " : " ++ show a)
      putStrLn ("  = " ++ show t)
    TEDef1 a va t vt x _ -> do
      putStrLn (show x ++ " : " ++ show a)
      putStrLn ("  = " ++ show t)
    TETyCon{} -> do
      putStrLn "<tycon not supported>"
    TEDataCon{} -> do
      putStrLn "<datacon not supported>"

  nl
  putStrLn "Metacontext"
  putStrLn (replicate 60 '-')
  nl
  D.forIx metaCxt \i -> \case
    Unsolved a -> do
      putStrLn ("?" ++ show i ++ " : " ++ show (Eval.quote1 0 DontUnfold a))
    Solved t a -> do
      putStrLn ("?" ++ show i ++ " : " ++ show (Eval.quote1 0 DontUnfold a))
      putStrLn ("  = " ++ show (Eval.quote1 0 DontUnfold t))

  nl
  putStrLn "CV context"
  putStrLn (replicate 60 '-')
  nl
  D.forIx cvCxt \i -> \case
    CVUnsolved   -> do
      putStrLn ("?" ++ show i)
    CVSolved cv  -> do
      putStrLn ("?" ++ show i ++ " = " ++ show cv)

--------------------------------------------------------------------------------

test :: String -> IO ()
test str = do
  reset
  let (src, top) = parseString str
  top <- case top of
    OK a _ _ -> pure a
    Fail     -> putStrLn "parse error" >> undefined
    Err e    -> print e >> undefined
  inferTop src top
  displayState

--------------------------------------------------------------------------------

main :: IO ()
main = do
  elabError (emptyCxt (RawName "foo")) (P.Var (Span (Pos 3) (Pos 0))) $ CantUnify

  -- test "foo = bar"
