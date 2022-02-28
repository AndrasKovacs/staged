{-

-- Compiler: staging, memory layout control, etc  (small + self-hosting + useful soon)

-- Surface language:
   1. CBV + side effects (ML)
   2. CBV + typed effect system:
        - Eff monad
        - Eff monad, row of effects
           f : Int -> Eff [IO, Exc e, MutRef a, MutRef b] Int
        - (extensible / algebraic / resumable effect handlers)


-- Intermediate language:

Assume CBV + printing

   pure language:       admissible substitution (beta-reduce, inline, delta-contraction (CSE))
   effectful language:  doesn't work

   issue: partial evaluation + effects: don't work together
          (effectful things cannot be substituted, (reordered, dropped))

   - effects are in a monad
      - GHC doesn't have a monad in Core, uses token passing to compile IO
         - unsound
         - only linear token-passing models (some) IO
              State hack, runST performance, unsafeInlineIO, ...
              (mandatory late inlining runST, runRW# etc.)


1. Monad in core for everything which is effectful  (M)
   - guarantees: partial eval + NBE still works, conversion checking modulo laws (efficiently, and in a sound way)
   - I don't know anything about the intension of my effects

   Optimization: could be State transformer (locally)

      finite number of mutable locations used in lexical

   put x; get     ~ pure x
   put x; put x;  ~ put x

   throw e; e2 ~ throw e

   limitation of usual State monad: (ST has stronger interface than State)
          - I formalized ST monad in Agda: (requires parametricity to prove heap well-typed)

       refs : List (MutRef Int)
       n    : Int

       do
         write (lookup n refs) 100
         f refs foo bar             -- out-of-line call
                                    -- erase all info in symbolic state

       -- ("symbolic" state,    lookup n refs ↦ 100)

   Exceptions:
      -- something similar

   Call-by-need:
     implementation: CBV + Eff + mutable refs

     interface for call-by-need:

         Lazy  : Type -> Type
         delay : A -> Eff (Lazy A)
                            (delay foo, delay foo)   =/   let x = delax foo in (x, x)
                            "strengthening for delay" : (let x = delay foo in e) ~ e    if   x ∉ FV(e)

         force : Lazy A -> IO A
         result : A -> Lazy A        -- returns an already force value  (useful for compiler opt)

Stuff I looked at:

  - 97: partial eval with side effects     (monadic translation) ("preactions")

   - administrative normal form only for effectful computations
     (monadic program: the same thing!)


   res <- f (g(x, y), h(y, z))
   ...

       to

   res1 <- g(x, y)
   res  <- f (res1, h(y, z))
   ...

   x <- foo
   y <- x

   f

     surface (f : () -> Int)   --->   f : () -> M Int
     let f() = print "foo"
     f();
     f();


  CBV monadic translation:
    - limitation: unknown functions are always monadic

  higher-order function:

    comp : (B -> C) -> (A -> B) -> A -> C
    comp f g x = f (g x)

    comp : (B -> M C) -> (A -> M B) -> A -> M C
    comp f g x = do
      gx <- g x
      f gx

   annoying with curried unknown functions


   foo : (Int -> Int -> Int) -> (Int, Int)
   foo f = (f 0 1, f 0 2)

   foo' : (Int -> Int -> Int) -> (Int, Int)
   foo' f = let f0 = f 0 in (f0 1, f0 2)

   foo : (Int -> M (Int -> M Int)) -> M (Int, Int)
   foo f = do
     f0 <- f 0
     f01 <- f0 1
     f0' <- f 0
     f02 <- f0' 2
     return (f01, f02)

   -- cheating : assume that unknown functions are only M in the return type

   foo : (Int -> Int -> M Int) -> M (Int, Int)
   foo f = do
     f01 <- f 0 1
     f02 <- f 0 2
     return (f01, f12)

   -- not the same program!

   foo : (Int -> Int -> M Int) -> M (Int, Int)
   foo f = do
     let f0 = f 0;
     f01 <- f0 1
     f02 <- f0 2
     return (f01, f12)

  -- CBV --> monadic   (optimize unknown function calls?)
  --                   (just write functions from tuples to result?)

  --  (use n-ary function in surface language, but allow partial application via currying subtyping)

  --  (+) : (Int, Int) -> Int
  --  (+) 10 : Int -> Int           --> elaborated to \y -> (+) (10, y)

   surface: foo : ((Int, Int) -> Int) -> (Int, Int)
   monadic: foo : ((Int, Int) -> M Int) -> M (Int, Int)

            foo f = let f0 = f 0 in (f0 1, f0 1)
      elab: foo f = let f0 = \y. f (0, y) in (f0 1, f0 2)


CBPV (call-by-push-value)

   - point: combine effects + by-value + by-name
      - lots of noise arising from CBV and CBN (by-name!)
      - by-name is practically useless!

   - Extended CBPV : extending CBPV with call-by-need evaluation

   CBV (by default) + effects + some delimited call-by-need



Intermediate lang, opt, PE, side effects, monadic, CBPV, CBPVE, subst
Surface lang: monadic or plain cbv,
monadic thunks
deeply embedded monadic effect for PR

-}


{-# language Strict, LambdaCase, BangPatterns, ViewPatterns, BlockArguments,
             EmptyCase, OverloadedStrings #-}

{-# options_ghc -Wincomplete-patterns #-}

import Data.Maybe
import Data.String

{-
First: monadic term language + effect + NbE + conversion checking

   - normal forms modulo monad laws
   - conversion checking with definitional monad laws

     - potential issue: "quadratic binding"  (Haskell: CPS)

  - conversion checking for monadic expressions?
    - dep. typed language + M : we need conversion checking (unification etc)
    - simply typed language + M + dependently typed static stage language (prove equality of runtime expressions)
       (lots of research questions about proving equality of runtime programs in static stage)

-}


-- "untyped" language
-- typed, just don't have types

type Name = String

data Tm
  = Var Name
  | App Tm Tm
  | Lam Name Tm
  | Let Name Tm Tm
  | Bind Name Tm Tm       -- monadic bind
  | Return Tm             -- monadic return
  | Hello                 -- Hello : M ()
  | Tt                    -- Tt : Unit
  deriving (Eq, Show)

data Val
  = VVar Name
  | VApp Val Val
  | VLam Name (Val -> Val)
  | VBind Name Val (Val -> Val)
  | VReturn Val
  | VHello
  | VTt

type Env = [(Name, Val)]

-- open evaluation of programs, performs pure computation, keeps effects in the same order
--------------------------------------------------------------------------------

vVar :: Env -> Name -> Val
vVar e x = fromJust $ lookup x e

vApp :: Val -> Val -> Val
vApp t u = case t of
  VLam _ t -> t u
  t        -> VApp t u

vBind :: Name -> Val -> (Val -> Val) -> Val
vBind x t u = case t of
  VReturn t    -> u t
  VBind y t t' -> vBind y t (\vy -> vBind x (t' vy) u)
  t            -> VBind x t u

eval :: Env -> Tm -> Val
eval e = \case
  Var x      -> vVar e x
  App t u    -> vApp (eval e t) (eval e u)
  Lam x t    -> VLam x (\vx -> eval ((x, vx):e) t)
  Let x t u  -> case eval e t of t -> eval ((x, t):e) u
  Bind x t u -> vBind x (eval e t) (\vx -> eval ((x, vx):e) u)
  Return t   -> VReturn (eval e t)
  Hello      -> VHello
  Tt         -> VTt

fresh :: [Name] -> Name -> Name
fresh ns "_" = "_"
fresh ns x | elem x ns = fresh ns (x ++ ",")
           | otherwise = x

quote :: [Name] -> Val -> Tm
quote ns = \case
  VVar x                    -> Var x
  VApp t u                  -> App (quote ns t) (quote ns u)
  VLam (fresh ns -> x) t    -> Lam x (quote (x:ns) (t (VVar x)))
  VBind (fresh ns -> x) t u -> case quote (x:ns) (u (VVar x)) of
                                 Return (Var x') | x == x' -> quote ns t
                                 u                         -> Bind x (quote ns t) u
  VReturn t                 -> Return (quote ns t)
  VHello                    -> Hello
  VTt                       -> Tt

nf0 :: Tm -> Tm
nf0 = quote [] . eval []

instance IsString Tm where fromString = Var


-- Execution only for closed values with type IO a
--------------------------------------------------------------------------------

infixr 4 >>>
(>>>) :: Tm -> Tm -> Tm
t >>> u = Bind "_" t u

-- closed value, with (M a) type
exec :: Val -> IO Val
exec = \case
  VReturn t   -> pure t
  VBind x t u -> do {t <- exec t; exec (u t)}
  VHello      -> VTt <$ putStrLn "hello"
  _           -> undefined

run :: Tm -> IO Val
run = exec . eval []

test1 =
  nf0 ((((((Hello >>> Hello) >>> Hello) >>> Hello) >>> ((Hello >>> Hello) >>> Hello)) >>> Hello) >>> Hello)
  ==
  (Hello >>> Hello >>> Hello >>> Hello >>> Hello >>> Hello >>> Hello >>> Hello >>> Hello)
