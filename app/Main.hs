
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
    TEDef1 a va t vt x _ -> do
      putStrLn (show x ++ " : " ++ showTm1Top a)
      putStrLn ("  = " ++ showTm1Top t)
      nl
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
  "Alg = [B: U1, true: B, false: B]",
  "Bool = (A : Alg) → A.B",
  "id : Bool → Bool = λ b A. b [A.B, A.true, A.false]",

  "NatAlg = [N : U1, zero : N, suc: N → N]",
  "Nat    = (A : NatAlg) → A.N",
  "zero   = λ (A : NatAlg). A.zero",
  "suc : Nat → Nat = λ n A. A.suc (n A)",
  "id  : Nat → Nat = λ n A. n [A.N, A.zero, A.suc]",
  "add : Nat → Nat → Nat = λ a b A. a [A.N, b A, A.suc]",
  "n5 = suc (suc (suc (suc (suc zero))))",

  "id : {i}{A : U0 i} → A → A = λ x. x",

  "comp : {i j k}{A : U0 i}{B : U0 j}{C : U0 k}",
  "     → (B → C) → (A → B) → A → C",
  "  = λ f g x. f (g x)",

  "id2 : Int → Int := id",
  "f : Int → Int = λ x. x + x + 10",
  "g := comp id (comp f f)",
  "h = comp g id",

  "Sg = λ A (B : A → U1). [fst : A, snd: B fst]",

  "Pointed = Sg U1 (λ A. A)",

  "SmallFunctor : U1 = [F : U0 Val → U1, map : {A B : U0 _} → (A → B) → F A → F B]",
  "BigFunctor   : U1 = [F : U1 → U1, map : {A B} → (A → B) → F A → F B]",

  "Eq : {A : U1} → A → A → U1",
  "  = λ {A} x y. (P : A → U1) → P x → P y",
  "refl : {A x} → Eq {A} x x = λ P px. px",

  "g := λ x. 10 + x",

  "foo : Eq g g = refl",

  "g : [fst: Int, snd: Int, thd: Int] := [20, 30, 100]",

  "foo : Eq g g = refl"

  -- "foo : {A : U1} → A → A = λ {A} B x. x"

  -- "foo = 100",
  -- "bar := foo"

  -- "id : Int → Int = λ x. x",
  -- "f : Int → Int := id"

  -- "id2 : Int → Int := id",
  -- "foo := λ x. x + 10",
  -- "bar := λ y. y * y + 400",

  -- Lam1 x Expl (Inserted 0 (Bind1 (Bind1 Empty A (U U1)) B (Var1 0))) (Var1 0)
  -- Pi x Expl (Flex 0 (SApp1 (SApp1 SId (Var1 0) Expl) (Var1 1) Expl))
  --           (Close Nil (App1 (App1 (Meta 0) (Var1 2) Expl) (Var1 1) Expl))

  -- "foo = λ x. 10 + x"

  -- "Sg : (A : MTy) → (A → MTy) → MTy",
  -- "  = λ A B. [fst : A, snd : B fst]",
  -- "Pointed = Sg MTy (λ A. A)",
  -- "foo : (p : Pointed) → p.fst = λ p. p.snd",

  -- "f : {A} → ^(A → A) = λ x. x",
  -- "g : {A B} → ^(A → B → A) = λ x y. x",
  -- "comp : {A B C : MTy} → (B → C) → (A → B) → A → C",
  -- "  = λ f g x. f (g x)",
  -- "idM : {A : MTy} → A → A",
  -- "  = λ x. x",
  -- "idM2 : {A} → A → A",
  -- "  = λ x. idM x"

  -- "Nat  : MTy = (N : MTy) → (N → N) → N → N",
  -- "zero : Nat = λ N s z. z",
  -- "suc  : Nat → Nat = λ a N s z. s (a N s z)",
  -- "add  : Nat → Nat → Nat = λ a b N s z. a N s (b N s z)",
  -- "mul  : Nat → Nat → Nat = λ a b N s z. a N (b N s) z"

  ]

--------------------------------------------------------------------------------

main :: IO ()
main = pure ()


  -- test "foo = bar"
