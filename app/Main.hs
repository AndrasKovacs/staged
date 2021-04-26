
module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Array.Dynamic.L as D
-- import qualified Data.HashMap.Strict  as M
-- import qualified FlatParse.Stateful as FP
-- import qualified Data.Set as S

import qualified Syntax              as S
-- import qualified Values              as V
import qualified Presyntax           as P
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
      putStrLn (show x ++ " : " ++ showTm1Top a)
      putStrLn ("  := " ++ showTm0Top t)
      nl
      putStrLn "-- STAGED"
      putStrLn (show x ++ " : " ++ showTm1Top' a)
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

-- prettyEx :: B.ByteString -> Error -> String
-- prettyEx b (Error pos e) =

--   let ls       = FP.lines b
--       [(l, c)] = posLineCols b [pos]
--       line     = if l < length ls then ls !! l else ""
--       linum    = show l
--       lpad     = map (const ' ') linum

--       expected (Lit s) = show s
--       expected (Msg s) = s

--       err (Precise exp)     = "expected " ++ expected exp
--       err (Imprecise exps)  = "expected " ++ (imprec $ S.toList $ S.fromList exps)
--       err (IndentMore col)  = "expected token indented to column " ++ show col ++ " or more"
--       err (ExactIndent col) = "expected token indented to column " ++ show col

--       imprec :: [Expected] -> String
--       imprec []     = error "impossible"
--       imprec [s]    = expected s
--       imprec (s:ss) = expected s ++ go ss where
--         go []     = ""
--         go [s]    = " or " ++ expected s
--         go (s:ss) = ", " ++ expected s ++ go ss

--   in show l ++ ":" ++ show c ++ ":\n" ++
--      lpad   ++ "|\n" ++
--      linum  ++ "| " ++ line ++ "\n" ++
--      lpad   ++ "| " ++ replicate c ' ' ++ "^\n" ++
--      "parse error: " ++
--      err e

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
      putStrLn $ showEx (S.localNames ls) e
      exitSuccess
    e -> do
      putStrLn $ showEx [] e
      exitSuccess

  displayState


p1 = unlines [

  "test : Int → Int := λ x. _"

  -- "Alg = [B: U1, true: B, false: B]",
  -- "Bool = (A : Alg) → A.B",
  -- "id : Bool → Bool = λ b A. b [A.B, A.true, A.false]",

  -- "NatAlg = [N : U1, zero : N, suc: N → N]",
  -- "Nat    = (A : NatAlg) → A.N",
  -- "zero   = λ (A : NatAlg). A.zero",
  -- "suc : Nat → Nat = λ n A. A.suc (n A)",
  -- "id  : Nat → Nat = λ n A. n [A.N, A.zero, A.suc]",
  -- "add : Nat → Nat → Nat = λ a b A. a [A.N, b A, A.suc]",
  -- "n5 = suc (suc (suc (suc (suc zero))))",

  -- "id    : {A : U1} → A → A = λ x. x",
  -- "const : {A B : U1} → A → B → A = λ a b. a",
  -- "comp  : {A B C : U1} → (B → C) → (A → B) → A → C = λ f g x. f (g x)",

  -- "id2 : Int → Int := id",
  -- "f : Int → Int = λ x. x + x + 10",
  -- "g := comp id (comp f f)",
  -- "h = comp g id",

  -- "Sg = λ A (B : A → U1). [fst : A, snd: B fst]",

  -- "Pointed = Sg U1 (λ A. A)",

  -- "SmallFunctor : U1 = [F : U0 Val → U1, map : {A B : U0 _} → (A → B) → F A → F B]",
  -- "BigFunctor   : U1 = [F : U1 → U1, map : {A B} → (A → B) → F A → F B]",

  -- "test : Int → Int → Int → Int = λ a b c. _",

  -- "Eq : {A : U1} → A → A → U1",
  -- "  = λ {A} x y. (P : A → U1) → P x → P y",
  -- "refl : {A x} → Eq {A} x x = λ P px. px",

  -- "g := λ x. 10 + x",

  -- "foo : Eq g g = refl",

  -- "g : [fst: Int, snd: Int, thd: Int] := [20, 30, 100]",

  -- "foo : Eq g g = refl"

  -- --

  -- "f1 : Int → Int := λ x. x + 10",



  -- "f2 : Int → Int := λ x. id x"



  -- "f2 : Int → Int := comp f1 f1",              -- stage inference: inserts staging annotations automatically
  --                                              -- "coercive subtyping"

  -- -- "f2 : Int → Int := λ x. comp f1 f1"           -- Int → Int ≤ ^Int → ^Int
  -- --                                               -- whenever (A : U0 cv) then (^A : U1)
  -- --                                               -- <_> : A → ^A
  --                                                  -- ~_  : ^A → A

  -- "myConstant : ^Int = <1000>",           -- ^Int   : type of Int-expressions (at meta level)
  --                                         -- <1000> : quoting of 1000

  -- "myInt := myConstant",
  -- "f2 : Int → Int := comp f1 f1",

  -- "VTy = U0 Val",       -- type synonym for "value" runtime types        (records + inductive types + primitives)
  -- "CTy = U0 Comp",      -- type synonym for "computation" runtime types  (functions)

  -- "id    : {A} → A → A = λ x. x",
  -- "comp  : {A B C} → (B → C) → (A → B) → (A → C) = λ f g x. f (g x)",
  -- "app   : {A B} → (A → B) → A → B = λ f x. f x",
  -- "const : {A B} → A → B → A = λ a b. a",

  -- "Nat : U1 = (N : U1) → (N → N) → N → N",  -- full Church Nat at meta level

  -- "Nat : U1 = (N : VTy) → (N → N) → N → N",

  -- data Nat₀ : VTy = Zero | Suc Nat₀
  -- ^Nat₀ ≃ ((N : VTy) → (^N → ^N) → ^N → ^N)

  -- map : {A B : VTy} → (^A → ^B) → CPS (List A) → CPS (List A)

  -- myListFun : List Int → List Int := map (+100) ∘ filter even ∘ take 100

  -- (f . g) x = f (g x)
  -- (f . g) = \x -> f (g x)

  -- "n0 : Nat = λ N s z. z",
  -- "n1 : Nat = λ N s z. s z",
  -- "add : Nat → Nat → Nat = λ a b N s z. a N s (b N s z)",
  -- "n5 : Nat = λ N s z. s (s (s (s (s z))))",
  -- "mul : Nat → Nat → Nat = λ a b N s. a N (b N s)",

  -- "NatToInt : Nat → Int = λ n. n Int (λ x. x + 1) 0",
  -- "IntToNat : Int → Nat = λ x N s z. _",

  -- "add2 = λ (x : Int). x + 2",

  -- "foo := comp add2 (comp add2 id)",

  -- "hof : (Int → Int) → Int = λ f. f (f 10)",

  -- "staticExp : Nat → Int → Int = λ a b. a Int (λ x. x * b) 1",

  -- "exp5 := comp add2 (staticExp n5)",

  -- -- meta-level
  -- "NatAlg : U1 = [N : U1, zero : N, suc : N → N]",
  -- "zero : (Alg : NatAlg) → Alg.N = λ Alg. Alg.zero",

  -- -- runtime

  -- "Pair1 : U1 → U1 → U1    = λ A B. [fst : A, snd : B]",
  -- "Pair0 : VTy → VTy → VTy = λ A B. [fst : A, snd : B]",

  -- "dup : {A : VTy} → A → Pair0 A A = λ x. let v := x; [x, x]",

  -- -- " enumFromTo x y

  -- "p1 : Pair1 Int Int = [add2 10, add2 20]",

  -- "p2 : Pair0 Int Int := let v := p1.fst; [v, v]"     -- "let insertion"


  -- "Eq : {A : U1} → A → A → U1",                -- Church-encoded propositional equality
  -- "  = λ {A} x y. (P : A → U1) → P x → P y",
  -- "refl : {A x} → Eq {A} x x = λ P px. px"

  -- if I have a) general recursion b) side effects c) exceptions

  -- let x := foo in bar
  -- let x =  foo in bar
  -- let x =  foo in bar

  -- "g := λ x. 100",
  -- "foo : Eq <g 10> <g 20> = refl"

  ]

--------------------------------------------------------------------------------

main :: IO ()
main = pure ()


  -- test "foo = bar"
