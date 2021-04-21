
module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Array.Dynamic.L as D
-- import qualified Data.HashMap.Strict  as M

-- import qualified Syntax              as S
-- import qualified Values              as V
import qualified Presyntax           as P
import qualified Evaluation          as Eval

import Common
-- import Cxt
import ElabState
-- import InCxt
import Exceptions
import Elaboration
import Parser
import Lexer

import System.Exit

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
    Err e    -> putStrLn (prettyError (coerce src) e) >> exitSuccess

  inferTop src top `catch` \case
    ElabError ls t e -> do
      let sp = coerce (unsafeSlice (coerce src) (P.span t))
      B.putStrLn sp
      print e
      exitSuccess
    e -> do
      print e
      exitSuccess

  displayState

p1 = unlines [
  -- "Alg  = [B: MTy, true: B, false: B]",
  -- "Bool = (A : Alg) → A.B",
  -- "id : Bool → Bool = λ b A. b [A.B, A.true, A.false]"

  "NatAlg = [N : MTy, zero : N, suc: N → N]",
  "Nat    = (A : NatAlg) → A.N",
  "zero   = λ (A : NatAlg). A.zero",
  "suc : Nat → Nat = λ n A. A.suc (n A)",
  "id : Nat → Nat = λ n A. n [A.N, A.zero, A.suc]",
  "add : Nat → Nat → Nat = λ a b A. a [A.N, b A, A.suc]"

  -- "Sg : (A : MTy) → (A → MTy) → MTy",
  -- "  = λ A B. [fst : A, snd : B fst]",
  -- "Pointed = Sg MTy (λ A. A)",

  -- "foo : (p : Pointed) → p.fst = λ p. p.snd"

  -- "f : {A} → ^(A → A) = λ x. x",
  -- "g : {A B} → ^(A → B → A) = λ x y. x",
  -- "comp : {A B C : MTy} → (B → C) → (A → B) → A → C",
  -- "  = λ f g x. f (g x)",
  -- "idM : {A : MTy} → A → A",
  -- "  = λ x. x",
  -- "idM2 : {A} → A → A",
  -- "  = λ x. idM x"

  -- "Nat : MTy = (N : MTy) → (N → N) → N → N",
  -- "zero : Nat = λ N s z. z",
  -- "suc : Nat → Nat = λ a N s z. s (a N s z)",
  -- "suc2 : _ = suc"
  -- -- "foo : Nat → Nat = comp suc suc",
  -- -- "bar = id suc2"
  ]

--------------------------------------------------------------------------------

main :: IO ()
main = pure ()


  -- test "foo = bar"
