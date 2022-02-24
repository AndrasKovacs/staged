
module Main where

import Control.Exception
import System.Environment
import System.Exit

import Common
import Cxt
import Errors
import Evaluation
import Metacontext
import Parser
import Pretty
import Elaboration

import qualified Presyntax as P

--------------------------------------------------------------------------------

helpMsg = unlines [
  "usage: 2ltt [--help] COMMAND",
  "",
  "Options:",
  "  --help : display this message",
  "  --show-insertions : displayed inserted implicit lambdas and applications",
  "",
  "All commands read input from stdin",
  "Commands:",
  "elab         : print elaboration output",
  "elab-verbose : print elaboration output, show metavariables and inserted implicits",
  "stage        : print staging output",
  "nf           : print normal form",
  "type         : print normal type of input"
  ]

mainWith :: IO [String] -> IO (P.Tm, String) -> IO ()
mainWith getOpt getRaw = do

  let elab = do
        (t, file) <- getRaw
        inferS (emptyCxt (initialPos file)) t S0
          `catch` \e -> displayError file e >> exitSuccess

  reset
  getOpt >>= \case
    ["--help"] -> putStrLn helpMsg
    ["elab"] -> do
      (t, a) <- elab
      putStrLn "-- metavariables "
      putStrLn "------------------------------------------------------------"
      putStrLn ""
      displayMetas
      putStrLn "-- elaboration output"
      putStrLn "------------------------------------------------------------"
      putStrLn ""
      putStrLn $ showTopTm t
    ["nf"]   -> do
      (t, a) <- elab
      putStrLn $ showTopTm $ nf [] t
      putStrLn "  :"
      putStrLn $ showTopTm $ quote 0 a
    ["type"] -> do
      (t, a) <- elab
      putStrLn $ showTopTm $ quote 0 a
    _ -> putStrLn helpMsg

main :: IO ()
main = mainWith getArgs parseStdin

-- | Run main with inputs as function arguments.
main' :: String -> String -> IO ()
main' mode src = mainWith (pure [mode]) ((,src) <$> parseString src)

ex1 :: IO ()
ex1 = main' "elab" $ unlines [
  -- "let x := U0;",
  -- "let y =  U1;",
  -- "let the = λ (A : U1)(x : A). x;",

  -- "λ (A : U0)",
  -- "  (B : A → U0 → U0).",

  -- "let the = λ (A : U1)(x : A). x;",
  -- "let foo := λ A (x : A). x;",
  -- "let the = λ (A : U1)(x : A). x;",
  -- "let foo := λ A (x : A). x;",
  "let m = _;",

  "U0"
  ]

ex3 :: IO ()
ex3 = main' "elab" $ unlines [

  "λ (List  : U0 → U0)",
  "  (nil   : {A : U0} → List A)",
  "  (cons  : {A : U0} → A → List A → List A)",
  "  (foldr : {A B : U0} → (A → B → B) → B → List A → B).",

  "let map : {A B : U0} → (A → B) → List A → List B",
  "  = λ f as. foldr (λ a bs. cons (f a) bs) nil as;",

  "let mapU0 : List U0 → List U0 := map {U0}{U0} (λ x. x);",

  "U0"

  ]


ex2 :: IO ()
ex2 = main' "elab" $ unlines [

  "-- Examples for unification with ordered metacontext & pruning",
  "--------------------------------------------------------------------------------",

  "-- we use Eq to test unification",
  "let Eq : {A : U1} → A → A → U1",
  "    = λ {A} x y. (P : A → U1) → P x → P y;",

  "let refl : {A : U1}{x : A} → Eq {A} x x",
  "    = λ _ px. px;",

  "-- inline type annotation",
  "let the : (A : U1) → A → A = λ _ x. x;",

  "-- 1. Solvable non-linear spine: (m a a =? λ x y. y) is solvable, because m's type does not",
  "--    depend on the non-linear arguments, and the rhs also does not depend on them.",
  "let m : (A : U1)(B : U1) → U1 → U1 → U1 = _;",
  "let test : U1 = Eq (m U1 U1) (λ x y. y);",

  -- "-- 2. U1nsolvable non-linear spine: m's type depends on non-linear args.",
  -- "-- let m : (A : U1)(B : U1) → A → B → B = _;",
  -- "-- let test = λ a b. the (Eq (m a a) (λ x y. y)) refl;",

  "-- 3. Intersection solution: first & second args pruned from m.",
  "let m : U1 → U1 → U1 → U1 = _;",
  "let test = λ a b c. the (Eq (m a b a) (m c b c)) refl;",

  "-- 4. Examples requiring pruning",
  "let pr1 = λ f x. f x;",
  "let pr2 = λ f x y. f x y;",
  "let pr3 = λ f. f U1;",

  "U0"
  ]

ex5 = main' "elab" $ unlines [
  "λ (Bool  : U0)",
  "  (true  : Bool)",
  "  (false : Bool)",
  "  (case  : {A : U0} → Bool → A → A → A)",
  "  (List  : U0 → U0)",
  "  (nil   : {A : U0} → List A)",
  "  (cons  : {A : U0} → A → List A → List A)",
  "  (foldr : {A B : U0} → (A → B → B) → B → List A → B)",

  "  (Nat₀  : U0)",
  "  (zero₀ : Nat₀)",
  "  (suc₀  : Nat₀ → Nat₀)",
  "  (rec₀  : {A : U0} → A → (A → A) → Nat₀ → A)",
  "  (mul₀  : Nat₀ → Nat₀ → Nat₀)",
  "  (add₀  : Nat₀ → Nat₀ → Nat₀).",

  "let test : List U0 = nil ;",

  "let id : {A : ^U0} → ^[A] → ^[A] = λ x. x ;",

  "let id0 : {A : U0} → A → A := λ x. x ;",

  -- "let kek := id U0;",

  -- "let const : {A B : ^U} → A → B → A = λ x y. x ;",

  -- "let comp : {A B C: ^U} → (B → C) → (A → B) → A → C",
  -- "    = λ f g x. f (g x) ;",

  -- "let ap : {A B: ^U} → (A → B) → A → B",
  -- "   = λ f x. f x ;",

  -- "let foo : Nat₀ → Nat₀ = ap (comp suc₀) (comp suc₀ id) ;",

  -- "let Nat₁ : U1 = (N : U1) → N → (N → N) → N ;",
  -- "let zero₁ : Nat₁ = λ _ z s. z ;",
  -- "let suc₁ : Nat₁ → Nat₁ = λ a _ z s. s (a _ z s) ;",
  -- "let add₁ : Nat₁ → Nat₁ → Nat₁ = λ a b N z s. a N (b N z s) s ;",
  -- "let n₁5 : Nat₁ = λ _ z s. s (s (s (s (s z)))) ;",
  -- "let n₁10 = add₁ n₁5 n₁5 ;",

  -- "let n₀5 = suc₀ (suc₀ (suc₀ (suc₀ (suc₀ zero₀)))) ;",
  -- "let n₀10 = add₀ n₀5 n₀5 ;",

  -- "let List₁ : U1 → U1 = λ A. (L : U1) → (A → L → L) → L → L ;",
  -- "let nil₁ : {A} → List₁ A = λ _ c n. n ;",
  -- "let cons₁ : {A} → A → List₁ A → List₁ A = λ a as L c n. c a (as L c n) ;",

  -- "let Pair : U1 -> U1 -> U1 = λ A B. (P : U1) → (A → B → P) → P ;",
  -- "let pair : {A B} → A → B → Pair A B = λ a b P p. p a b ;",
  -- "let fst : {A B} → Pair A B → A = λ p. p _ (λ a b. a) ;",
  -- "let snd : {A B} → Pair A B → B = λ p. p _ (λ a b. b) ;",

  -- "let ;lCase : {A : ^U} → Bool → A → A → A",
  -- "    = case ;",

  -- "let test : Nat₀ = id n₀10 ;",
  -- "let test : Bool → Nat₀ → Nat₀ = λ b n. ;lCase b (add₀ n n₀10) n ;",

  -- "let map : {A B : ^U} → (^A → ^B) → ^(List A) → ^(List B)",
  -- "    = λ {A}{B} f. foldr (λ a. cons (f a)) nil ;",

  -- "let mapU0 : List (U0) -> List (U0) = map (\\x.x) ;",

  -- "let not : ^Bool → ^Bool = λ b. case b false true ;",

  -- "let mapNot : List Bool → List Bool = map not ;",

  -- "let exp₀ : Nat₀ → Nat₀ → Nat₀ = λ a b. rec₀ (suc₀ zero₀) (mul₀ b) a ;",

  -- "let exp₁ : Nat₁ → Nat₀ → Nat₀",
  -- "    = λ a b. a _ (suc₀ zero₀) (λ n. mul₀ n b) ;",

  -- "let exp5 : Nat₀ → Nat₀ = exp₁ n₁5 ;",

  -- "let lower : Nat₁ → Nat₀ = λ n. n _ zero₀ suc₀ ;",

  -- "let upto : Nat₁ → List₁ Nat₁",
  -- "    = λ n. n _ (λ acc. cons₁ acc nil₁) (λ hyp acc. hyp (suc₁ acc)) zero₁ ;",

  -- "let expSum : List₁ Nat₀ → Nat₀",
  -- "    = λ ns. ns (List₁ Nat₀ → Nat₀)",
  -- "               (λ n hyp xs. <let x = [n] ; hyp (cons₁ x xs)>)",
  -- "               (λ xs. xs _ add₀ zero₀)",
  -- "               nil₁ ;",

  -- "let map₁ : {A B} → (A → B) → List₁ A → List₁ B",
  -- "  = λ f as. as _ (λ a. cons₁ (f  a)) nil₁ ;",

  -- "let test : Nat₀ = expSum (cons₁ n₀5 (cons₁ (add₀ n₀5 n₀10) nil₁)) ;",

  -- "let foo : Nat₀ = lower (add₁ n₁5 (let x : Nat₀ = zero₀ ; n₁5)) ;",

  "U0"
  ]
