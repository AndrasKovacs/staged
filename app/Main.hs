
module Main where

-- import qualified Data.ByteString.Char8 as B
import qualified Data.Array.Dynamic.L as D
-- import qualified Data.HashMap.Strict  as M
-- import qualified FlatParse.Stateful as FP
-- import qualified Data.Set as S

-- import qualified Syntax              as S
-- import qualified Values              as V
-- import qualified Presyntax           as P
-- import qualified Evaluation          as Eval

import Common
-- import Cxt
import ElabState
-- import InCxt
import Exceptions
import Elaboration
import Parser
import Lexer
import Pretty

import System.Exit


--------------------------------------------------------------------------------

displayState :: IO ()
displayState = do
  let nl = putStrLn ""

  nl
  putStrLn "Metacontext"
  putStrLn (replicate 60 '-')
  nl
  D.forIx metaCxt \i -> \case
    Unsolved a -> do
      putStrLn ("?" ++ show i ++ " : " ++ showVal1Top a)
      nl
    Solved t a -> do
      putStrLn ("?" ++ show i ++ " : " ++ showVal1Top a)
      putStrLn ("  = " ++ showVal1Top t)
      nl

  nl
  putStrLn "Top environment"
  putStrLn (replicate 60 '-')
  nl
  D.for top \case
    TEDef0 a va t vt cv x _ -> do
      putStrLn (show x ++ " : " ++ showVal1Top va)
      putStrLn ("  := " ++ showTm0Top t)
      nl
      putStrLn "-- STAGED"
      putStrLn (show x ++ " : " ++ showVal1Top va)
      putStrLn ("  := " ++ showTm0Top' t)
      nl
    TEDef1 a va t vt x _ -> do
      putStrLn (show x ++ " : " ++ showTm1Top a)
      putStrLn ("  = " ++ showTm1Top t)
      nl
      -- putStrLn "-- NORMAL FORM"
      -- putStrLn (show x ++ " : " ++ showTm1Top' a)
      -- putStrLn ("  = " ++ showTm1Top' t)
      -- nl
    TETyCon{} -> do
      putStrLn "<tycon not supported>"
    TEDataCon{} -> do
      putStrLn "<datacon not supported>"


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
    e -> do
      putStrLn $ showElabError (coerce src) e
      exitSuccess

  displayState


p1 = unlines [

  "VTy : U1 = U0 Val",
  "CTy : U1 = U0 Comp",
  "the : (A : U1) → A → A = λ A x. x",
  -- "Pair : VTy → VTy → VTy = λ A B . [fst: A, snd: B]",

  -- "let-insertion"
  "CPair : VTy → VTy → U1 = λ A B. (P : VTy) → (A → B → P) → P",
  -- "dup : {A : U0 Val} → A → CPair A A = λ a P p. let v := a; p a a",

  -- -- -- here      Template Haskell
  -- -- -- <_>        [| _ |]
  -- -- -- ~_         $(_)

  "Eq : {A : U1} → A → A → U1",
  "  = λ {A} x y. (P : A → U1) → P x → P y",
  "refl : {A : U1}{x :A} → Eq {A} x x = λ P px. px",

  -- "id    : {A : U1} → A → A = λ x. x",
  -- "const : {A B : U1} → A → B → A = λ a b. a",
  -- "comp  : {A B C : U1} → (B → C) → (A → B) → A → C = λ f g x. f (g x)",

  -- "rec1 : [fst : Int, snd : Int] := [1000, 2000]",

  -- "rec1 : [Int, Int] := [1000, 2000]",

  -- "foo : [fst: Int, snd: Int] := [100, 200]",
  -- "bar : [snd: Int, fst: Int] := foo"

  -- "p = the (Eq [foo : ^Int] [foo : ^Int]) refl"


  -- "foo : ^(Int → Int) → Int → Int = λ f x. f x",
  -- "fst : {A B : VTy} → [A → Int → B, A] → B = λ x. x.0 100 x.1"

  -- "foo := [10, 20]"


  -- "Sigma  : (A : U1) → (A → U1) → U1 = λ A B. [fst : A, snd : B fst]",
  -- "Sigma2 : (A : U1) → (A → U1) → U1 = λ A B. [fst : A, snd : B fst]",

  --  -- "relative monad"
  -- "SmallState : U0 Val → U0 Val → U0 Comp = λ S A. S → [fst : A, snd : S]",

  -- "relative monad" as well
  "State  : VTy → VTy → U1 = λ S A. S → [fst: A, snd: S]",
  "put    : {S: VTy} → S → State S [] = λ s _. [[], s]",
  "get    : {S: VTy} → State S S = λ s. [s, s]",   -- non-linear!!
  "modify : {S : VTy} → (S → S) → State S [] = λ f s. [[], f s]",
  "pure   : {A S : VTy} → A → State S A = λ a s. [a, s]",
  "bind   : {A B S : VTy} → State S A → (A → State S B) → State S B",
  "         = λ ma f s. let as = ma s; f as.fst as.snd",
  "seq    : {A B S : VTy} → State S A → State S B → State S B = λ ma mb. bind ma (λ _. mb)",
  "evalState : {A S : VTy} → State S A → S → A = λ ma s. (ma s).fst",
  "execState : {A S : VTy} → State S A → S → S = λ ma s. (ma s).snd",

  "f := execState (seq (modify (λ x. x + 10)) (modify (λ x. x + 100)))",
  "g := λ (n:Int). execState {[]}{_} (modify {Int} (λ s. s + 10)) n",

  "h := execState (seq (put 100) (seq (put 100) (pure 5)))",

  "State  : VTy → VTy → U1 = λ S A. S → CPair A S",
  "put    : {S: VTy} → S → State S [] = λ s _ P p. p [] s",
  "get    : {S: VTy} → State S S = λ s P p . p s s",   -- non-linear!!
  "modify : {S : VTy} → (S → S) → State S [] = λ f s P p. p [] (f s)",
  "pure   : {A S : VTy} → A → State S A = λ a s P p. p a s",
  "bind   : {A B S : VTy} → State S A → (A → State S B) → State S B",
  "         = λ ma f s P p. ma s P (λ a s. f a s P p)",
  "seq    : {A B S : VTy} → State S A → State S B → State S B = λ ma mb. bind ma (λ _. mb)",
  "evalState : {A S : VTy} → State S A → S → A = λ ma s. let s := s; ma s _ (λ a s. a)",
  "execState : {A S : VTy} → State S A → S → S = λ ma s. let s := s; ma s _ (λ a s. s)",

  "g : Int → Int := λ n. execState (seq (modify (λ x. x*x*x)) (modify (λ x. x*x*x))) (n + 100)",
  "h := execState (bind get (λ x. seq (modify (λ x. x * x)) get))"
  ]

--------------------------------------------------------------------------------

main :: IO ()
main = pure ()
