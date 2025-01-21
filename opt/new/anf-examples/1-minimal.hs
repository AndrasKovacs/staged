
{-
This is the absolute simplest ANF conversion implementation that I know about,
for untyped lambda calculus with sums (given by Inl, Inr and Case).

This illustrates the basic usage of CPS (continuation-passing-style)
implementation. CPS is, strictly speaking, not necessary for ANF conversion, but
as soon as we have any case switching, it is highly convenient.

There are two things in this implementation that make it "toy":

1. The output of the translation is the same type as the input.
   This is clearly not good, because any downstream processing
   wants to rely on guaranteed ANF structure of terms.

2. It does not handle the potential exponential blowup in "case".
   For example, take the following program:

     f = \x0.
       let x1 = case x0 of Inl y -> Inl y;
                           Inr y -> Inr y;
       let x2 = case x1 of Inl y -> Inl y;
                           Inr y -> Inr y;
       x2

   Whenever we ANF-convert a "case", we inline the entire rest
   of the program into both case branches, so we get this:


     f = \x0.
       case x0 of
         Inl y ->
           let x1 = Inl y
           case x1 of
             Inl y ->
               let x2 = Inl y
               x2
             Inr y ->
               let x2 = Inr y
               x2
         Inr y ->
           let x1 = Inr y
           case x2 of
             Inl y ->
               let x2 = Inl y
               x2
             Inr y ->
               let x2 = Inr y
               x2

In the next file we will address both issues.
-}


{-# language Strict, BlockArguments #-}
{-# options_ghc -Wincomplete-patterns #-}

type Lvl = Int -- de Bruijn level

data Tm
  = Var Lvl
  | App Tm Tm
  | Lam Tm
  | Let Tm Tm
  | Inl Tm
  | Inr Tm
  | Case Tm Tm Tm
  deriving Show

{-
Explanation for the arguments of anf.

  - [Lvl] is an "environment" which maps the free variables of the input term to
    variables to be used in the output term. We need this environment because
    ANF conversion inserts lots of extra let-binders, which requires us to
    appropriately adjust the De Bruijn levels in the original term.

    We index the environment from the back to the front, which is why
    "e !! (length e - x - 1)" is used for lookup.

  - The "l" argument with type Lvl keeps track of the next fresh Lvl, i.e.
    the next Lvl that's guaranteed to not occur in the environment.

  - In the (Lvl -> Lvl -> Tm) continuation, the first Lvl is the next fresh Lvl,
    while the second Lvl argument is a variable that's the "result" of previous
    ANF-conversion. Indeed, the point of ANF is that every intermediate
    computation gets a variable as a name.

-}

anf :: [Lvl] -> Tm -> Lvl -> (Lvl -> Lvl -> Tm) -> Tm
anf e t l k = case t of
  -- lookup variable
  Var x      -> k l (e !! (length e - x - 1))

  -- 1 generate code for the two subterms, getting variable labels for the results
  -- 2 let-bind the application result, continue code generation
  -- Note the repeated shadowing for "t", "u" and "l", it's very much intentional,
  -- to prevent usage of values that should not be used anymore.

  App t u    -> anf e u l \u l -> -- we first evaluate the argument, then the function itself
                anf e t l \t l -> -- the order could be switched; the point is that we can control evaluation order
                Let (App (Var t) (Var u)) (k l (l + 1)) -- "l" labels the newly let-bound value
                                                        -- we bump the next fresh var to "l + 1"
                                                        -- because we've just used "l"

  -- we let-bind the ANF-converted lambda, then continue code generation
  -- again in "k l (l + 1)" we pass the label of the new let, and bump the fresh var counter.

  -- In "(anf (l:e) t (l+1) (\t _ -> Var t))" we need some manipulation, because
  -- "t" is under an extra binder. So, we have to map the extra free variable in "t"
  -- to something in the environment. We map it to "l", so we pass "(l:e)". Also,
  -- we bump the fresh var counter. Finally "\t _ -> Var t" means that we don't do
  -- any kind of code generation after processing the lambda body, we simply wrap
  -- the label of the result value in a Var constructor.
  Lam t      -> Let (Lam (anf (l:e) t (l+1) (\t _ -> Var t))) (k l (l + 1))

  -- here we anf-convert "t", get a variable that labels its result,
  -- then re-map the original let-bound variable to that label, in "(t:e)"
  Let t u    -> anf e t l \t l -> anf (t:e) u l k

  -- let-bind a new Inl/Inr and continue
  Inl t      -> anf e t l \t l -> Let (Inl (Var t)) (k l (l + 1))
  Inr t      -> anf e t l \t l -> Let (Inr (Var t)) (k l (l + 1))

  -- get the label for "t", then generate a Case where code generation
  -- continues in both branches with "k". If "k" produces a lot of code,
  -- this results in a size blowup.
  Case t u v -> anf e t l \t l -> Case (Var t) (anf (l:e) u (l + 1) k)
                                               (anf (l:e) v (l + 1) k)

-- ANF-convert a closed Tm
anf0 :: Tm -> Tm
anf0 t = anf [] t 0 (const . Var)
