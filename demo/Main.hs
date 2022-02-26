
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
import Staging

import qualified Presyntax as P

--------------------------------------------------------------------------------

helpMsg = unlines [
  "usage: 2ltt COMMAND",
  "",
  "All commands read input from stdin. The input must be a single 2LTT",
  "expression at stage 0.",
  "",
  "Commands:",
  "  elab          : print elaboration output",
  "  elab-verbose  : print elaboration output, show metavars and inserted implicits",
  "  stage         : print staging output",
  "  stage-verbose : print staging output, show inserted implicits",
  "  nf            : print normal form",
  "  type          : print normal type of input"
  ]

mainWith :: IO [String] -> IO (P.Tm, String) -> IO ()
mainWith getOpt getRaw = do

  let elab = do
        (!t, !file) <- getRaw
        inferS (emptyCxt (initialPos file)) t S0
          `catch` \e -> displayError file e >> exitSuccess

  reset
  getOpt >>= \case
    ["--help"] -> putStrLn helpMsg
    ["elab"] -> do
      (!t, !a) <- elab
      putStrLn "-- elaboration output"
      putStrLn "------------------------------------------------------------"
      putStrLn ""
      -- putStrLn $ show t
      putStrLn $ showTopTm V0 t
    ["elab-verbose"] -> do
      (!t, !a) <- elab
      putStrLn "-- metavariables "
      putStrLn "------------------------------------------------------------"
      putStrLn ""
      displayMetas
      putStrLn "-- elaboration output"
      putStrLn "------------------------------------------------------------"
      putStrLn ""
      putStrLn $ showTopTm V1 t
    ["nf"]   -> do
      (!t, !a) <- elab
      putStrLn $ showTopTm V1 $ nf [] t
      putStrLn "  :"
      putStrLn $ showTopTm V1 $ quote 0 a
    ["type"] -> do
      (!t, !a) <- elab
      putStrLn $ showTopTm V1 $ quote 0 a
    ["stage"] -> do
      (!t, !a) <- elab
      putStrLn $ showTopTm V0 $ stage $ zonk [] 0 t
    ["stage-verbose"] -> do
      (!t, !a) <- elab
      putStrLn $ showTopTm V1 $ stage $ zonk [] 0 t
    _ -> putStrLn helpMsg

main :: IO ()
main = mainWith getArgs parseStdin

-- | Run main with inputs as function arguments.
main' :: String -> String -> IO ()
main' mode src = mainWith (pure [mode]) ((,src) <$> parseString src)

--------------------------------------------------------------------------------

verbosityTest = main' "elab" $ unlines [
  "let id : {A : U0} → A → A := λ x. x;",
  "id U0"
  ]

natUnifyTest = main' "elab-verbose" $ unlines [
  "let Eq : {A : U1} → A → A → U1",
  "  = λ {A} x y. (P : A → U1) → P x → P y;",

  "let refl : {A : U1}{x : A} → Eq {A} x x",
  "  = λ _ px. px;",

  "let meta : Nat1 → Nat1 = _;",
  "let test : (n : Nat1) → Eq {Nat1} (meta n) (NatElim1 (λ _. Nat1) (λ _ x. x) zero1 n)",
  "  = λ n. refl ;",

  "U0"
  ]

natTest = main' "nf" $ unlines [
  "let foo := zero0;",
  "let bar := λ x. suc0 x;",
  "let x = zero1;",
  "let y = λ x. suc1 x;",

  "let iter0 : {A : U0} → Nat0 → (A → A) → A → A",
  "  := λ {A} n f a. NatElim0 (λ _. A) (λ _. f) a n;",

  "let add0 : Nat0 → Nat0 → Nat0",
  "  := λ a b. iter0 a (λ x. suc0 x) b;",

  "let n5₀ := suc0 (suc0 (suc0 (suc0 (suc0 zero0))));",
  "let n10₀ := add0 n5₀ n5₀;",

  "let iter1 : {A : U1} → Nat1 → (A → A) → A → A",
  "  = λ {A} n f a. NatElim1 (λ _. A) (λ _. f) a n;",

  "let add1 : Nat1 → Nat1 → Nat1",
  "  = λ a b. iter1 a (λ x. suc1 x) b;",

  "let n5₁ = suc1 (suc1 (suc1 (suc1 (suc1 zero1))));",
  "let n10₁ = add1 n5₁ n5₁;",

  "n10₀"
  ]

ex1 :: IO ()
ex1 = main' "elab" $ unlines [
  "let Nat = (N : U1) → (N → N) → N → N;",
  "let n5 : Nat = λ N s z. s (s (s (s (s z))));",
  "n5 U0 (λ A. A → A) U0"
  ]

ex3 :: IO ()
ex3 = main' "elab" $ unlines [

  "λ (List  : U0 → U0)",
  "  (nil   : {A : U0} → List A)",
  "  (cons  : {A : U0} → A → List A → List A)",
  "  (foldr : {A B : U0} → (A → B → B) → B → List A → B).",

  "let map : {A B : U0} → (A → B) → List A → List B",
  "  = λ f as. foldr (λ a bs. cons (f a) bs) nil as;",

  "let mapU0 : List U0 → List U0 := map (λ x. x);",

  "U0"

  ]

inferenceTest = main' "elab-verbose" $ unlines [

  "let Eq : {A : U1} → A → A → U1",
  "    = λ {A} x y. (P : A → U1) → P x → P y;",

  "let refl : {A : U1}{x : A} → Eq {A} x x",
  "    = λ _ px. px;",

  "let Eq0 : {A : U0} → A → A → U0",
  "    := λ {A} x y. (P : A → U0) → P x → P y;",

  "let refl0 : {A : U0}{x : A} → Eq0 {A} x x",
  "    := λ _ px. px;",

  -- pruning <A> from m2
  "let m1 : U1 = _;",
  "let m2 : ^U0 → U1 = _;",
  "let test := λ (A : U0). let foo : Eq {U1} m1 (U1 → m2 <A>) = refl {U1}{m1}; A;",

  -- pruning [A] from m2
  "let m1 : U0 := _;",
  "let m2 : U0 → U0 := _;",
  "let test = λ (A : ^U0). let foo : Eq0 {U0} m1 (U0 → m2 [A]) := refl0 {U0}{m1}; A;",

  -- expand splice in pruning
  "let m1 : U0 := _;",
  "let m2 : ^U0 → ^U0 = _;",
  "let test = λ (A : ^U0). let foo : Eq0 {U0} m1 (U0 → [m2 A]) := refl0 {U0}{m1}; A;",

  -- expand splice in solution spines
  "let m1 : ^U0 → ^U0 = _;",
  "let test = λ (A : ^U0). let foo : Eq0 {U0} [m1 A] A := refl0 {U0}{[m1 A]}; A;",

  -- invert quote in spine
  "let m1 : ^U0 → ^U0 = _;",
  "let test := λ (A : U0). let foo : Eq {^U0} (m1 <A>) <A> = refl {^U0}{m1 <A>} ; A;",

  -- invert splice in spine
  "let m1 : U0 → U0 := _;",
  "let test = λ (A : ^U0). let foo : Eq0 {U0} (m1 [A]) [A] = refl0 {U0}{m1 [A]}; A;",

  "U0"
  ]


ex2 :: IO ()
ex2 = main' "elab-verbose" $ unlines [

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

  "-- 2. Unsolvable non-linear spine: m's type depends on non-linear args.",
  "-- let m : (A : U1)(B : U1) → A → B → B = _;",
  "-- let test = λ a b. the (Eq (m a a) (λ x y. y)) refl;",

  -- "-- 3. Intersection solution: first & third args pruned from m.",
  -- "let m : U1 → U1 → U1 → U1 = _;",
  -- "let test = λ a b c. the (Eq (m a b c) (m c b a)) refl;",

  "-- 4. Examples requiring pruning",
  "let pr1 = λ f x. f x;",
  "let pr2 = λ f x y. f x y;",
  "let pr3 = λ f. f U1;",

  "U0"
  ]

ex5 = main' "elab-verbose" $ unlines [
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

  "let id : {A : U1} → A → A = λ x. x;",

  "let const : {A B : U1} → A → B → A = λ x y. x ;",

  "let comp : {A B C : U1} → (B → C) → (A → B) → A → C",
  "    = λ f g x. f (g x) ;",

  "let ap : {A B : U1} → (A → B) → A → B",
  "   = λ f x. f x ;",

  "let Nat₁ : U1 = (N : U1) → N → (N → N) → N ;",
  "let zero₁ : Nat₁ = λ _ z s. z ;",
  "let suc₁ : Nat₁ → Nat₁ = λ a _ z s. s (a _ z s) ;",
  "let add₁ : Nat₁ → Nat₁ → Nat₁ = λ a b N z s. a N (b N z s) s ;",
  "let n₁5 : Nat₁ = λ _ z s. s (s (s (s (s z)))) ;",
  "let n₁10 = add₁ n₁5 n₁5 ;",

  "let n₀5 := suc₀ (suc₀ (suc₀ (suc₀ (suc₀ zero₀)))) ;",
  "let n₀10 := add₀ n₀5 n₀5 ;",

  "let List₁ : U1 → U1 = λ A. (L : U1) → (A → L → L) → L → L ;",
  "let nil₁ : {A} → List₁ A = λ _ c n. n ;",
  "let cons₁ : {A} → A → List₁ A → List₁ A = λ a as L c n. c a (as L c n) ;",

  "let Pair : U1 -> U1 -> U1 = λ A B. (P : U1) → (A → B → P) → P ;",
  "let pair : {A B} → A → B → Pair A B = λ a b P p. p a b ;",
  "let fst : {A B} → Pair A B → A = λ p. p _ (λ a b. a) ;",
  "let snd : {A B} → Pair A B → B = λ p. p _ (λ a b. b) ;",


  "let map : {A B : U0} → (A → B) → List A → List B",
  "    = λ f. foldr (λ a. cons (f a)) nil;",

  "let mapId : {A : U0} → List A → List A := map id;",

  "let not : ^Bool → ^Bool = λ b. case b false true ;",

  "let mapNot : List Bool → List Bool = map not ;",

  "let exp₀ : Nat₀ → Nat₀ → Nat₀ := λ a b. rec₀ (suc₀ zero₀) (mul₀ b) a ;",

  "let the : (A : U1) → A → A = λ A x. x;",

  "let exp₁ : Nat₁ → ^Nat₀ → ^Nat₀",
  "    = λ a b. a _ (suc₀ zero₀) (λ n. mul₀ n b) ;",

  "let exp5 : Nat₀ → Nat₀ = exp₁ n₁5 ;",

  "let lower : Nat₁ → Nat₀ = λ n. n _ zero₀ suc₀ ;",

  "let upto : Nat₁ → List₁ Nat₁",
  "    = λ n. n _ (λ acc. cons₁ acc nil₁) (λ hyp acc. hyp (suc₁ acc)) zero₁ ;",

  "let expSum : List₁ Nat₀ → Nat₀",
  "    = λ ns. ns (List₁ Nat₀ → Nat₀)",
  "               (λ n hyp xs. let x := n; hyp (cons₁ x xs))",
  "               (λ xs. xs _ add₀ zero₀)",
  "               nil₁ ;",

  "let map₁ : {A B} → (A → B) → List₁ A → List₁ B",
  "  = λ f as. as _ (λ a. cons₁ (f a)) nil₁ ;",

  "let test : Nat₀ = expSum (cons₁ n₀5 (cons₁ (add₀ n₀5 n₀10) nil₁)) ;",

  "U0"
  ]
