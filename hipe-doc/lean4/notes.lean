
-- build & run:
-- lean -c notes.c notes.lean; leanc notes.c -O2 -o notes; ./notes

inductive RTm where
  | RVar : String -> RTm
  | RApp : RTm -> RTm -> RTm
  | RLam : String -> RTm -> RTm

open RTm

inductive Tm where
  | Var : UInt64 -> Tm
  | App : Tm -> Tm -> Tm
  | Lam : String -> Tm -> Tm
deriving Repr

open Tm

def elabVar (ns : List String) (x : String) : UInt64 :=
  match ns with
  | []       => 0
  | x' :: ns => if x == x' then 0 else elabVar ns x + 1

def elabTm' (ns : List String) (t : RTm) : Tm :=
  match t with
  | RVar x   => Var (elabVar ns x)
  | RApp t u => App (elabTm' ns t) (elabTm' ns u)
  | RLam x t => Lam x (elabTm' (x :: ns) t)

def elabTm := elabTm' []

inductive Val where
  | VVar : UInt64 -> Val
  | VApp : Val -> Val -> Val
  | VLam : String -> List Val -> Tm -> Val

open Val

def evalVar (e : List Val)(x : UInt64) : Val :=
  match e, x with
  | v :: _, 0 => v
  | _ :: e, x => evalVar e (x - 1)
  | _, _      => VVar 0

unsafe def eval (e : List Val) (t : Tm) : Val :=
  match t with
  | Var x   => evalVar e x
  | App t u => match eval e t with
               | VLam _ e' t => eval (eval e u :: e') t
               | t           => VApp t (eval e u)
  | Lam x t => VLam x e t

unsafe def conv (l : UInt64)(t t' : Val) : Bool :=
  match t, t' with
  | VVar x    , VVar x'      => x == x'
  | VApp t u  , VApp t' u'   => conv l t t' && conv l u u'
  | VLam _ e t, VLam _ e' t' => let v := VVar l;
                                conv (l + 1) (eval (v :: e) t) (eval (v :: e') t')
  | _         , _            => False

unsafe def quote (l : UInt64)(t : Val) : Tm :=
  match t with
  | VVar x => Var (l - x - 1)
  | VApp t u => App (quote l t) (quote l u)
  | VLam x e t => Lam x (quote (l + 1) (eval (VVar l :: e) t))

unsafe def eval0  := eval []
unsafe def conv0  := conv 0
unsafe def quote0 := quote 0
unsafe def nf0    := quote0 ∘ eval0

infixl:80 " $$ " => RApp

@[inline]
def let_ x t u := RLam x u $$ t

def prog :=
  let_ "zero" (RLam "s" $ RLam "z" $ RVar "z") $
  let_ "suc" (RLam "n" $ RLam "s" $ RLam "z" $ RVar "s" $$ (RVar "n" $$ RVar "s" $$ RVar "z")) $
  let_ "n5" (RVar "suc" $$ (RVar "suc" $$ (RVar "suc" $$ (RVar "suc" $$ (RVar "suc" $$ RVar "zero"))))) $
  let_ "add" (RLam "n" $ RLam "m" $ RLam "s" $ RLam "z" $ RVar "n" $$ RVar "s" $$ (RVar "m" $$ RVar "s" $$ RVar "z")) $
  let_ "n10" (RVar "add" $$ RVar "n5" $$ RVar "n5") $
  let_ "n15" (RVar "add" $$ RVar "n5" $$ RVar "n10") $
  let_ "n20" (RVar "add" $$ RVar "n5" $$ RVar "n15") $
  let_ "leaf" (RLam "n" $ RLam "l" $ RVar "l") $
  let_ "node" (RLam "t1" $ RLam "t2" $ RLam "n" $ RLam "l" $ RVar "n" $$ (RVar "t1" $$ RVar "n" $$ RVar "l")
                                                                      $$ (RVar "t2" $$ RVar "n" $$ RVar "l")) $
  let_ "mktree" (RLam "n" $ RVar "n" $$ (RLam "t" $ RVar "node" $$ RVar "t" $$ RVar "t") $$ RVar "leaf") $
  RVar "mktree" $$ RVar "n20"

def prog2 :=
  let_ "zero" (RLam "s" $ RLam "z" $ RVar "z") $
  let_ "suc" (RLam "n" $ RLam "s" $ RLam "z" $ RVar "s" $$ (RVar "n" $$ RVar "s" $$ RVar "z")) $
  let_ "n5" (RVar "suc" $$ (RVar "suc" $$ (RVar "suc" $$ (RVar "suc" $$ (RVar "suc" $$ RVar "zero"))))) $
  let_ "add" (RLam "n" $ RLam "m" $ RLam "s" $ RLam "z" $ RVar "n" $$ RVar "s" $$ (RVar "m" $$ RVar "s" $$ RVar "z")) $
  let_ "n10" (RVar "add" $$ RVar "n5" $$ RVar "n5") $
  let_ "n15" (RVar "add" $$ RVar "n5" $$ RVar "n10") $
  let_ "n20" (RVar "add" $$ RVar "n5" $$ RVar "n15") $
  let_ "leaf" (RLam "n" $ RLam "l" $ RVar "l") $
  let_ "node" (RLam "t1" $ RLam "t2" $ RLam "n" $ RLam "l" $ RVar "n" $$ (RVar "t1" $$ RVar "n" $$ RVar "l")
                                                                      $$ (RVar "t2" $$ RVar "n" $$ RVar "l")) $
  let_ "mktree" (RLam "n" $ RVar "n" $$ (RLam "t" $ RVar "node" $$ RVar "t" $$ RVar "t") $$ RVar "leaf") $
  RVar "mktree" $$ RVar "n20" $$ (RLam "_" $ RLam "_" $ RVar "zero") $$ RVar "zero"

def prog3 :=
  let_ "zero" (RLam "s" $ RLam "z" $ RVar "z") $
  let_ "suc" (RLam "n" $ RLam "s" $ RLam "z" $ RVar "s" $$ (RVar "n" $$ RVar "s" $$ RVar "z")) $
  let_ "n5" (RVar "suc" $$ (RVar "suc" $$ (RVar "suc" $$ (RVar "suc" $$ (RVar "suc" $$ RVar "zero"))))) $
  let_ "add" (RLam "n" $ RLam "m" $ RLam "s" $ RLam "z" $ RVar "n" $$ RVar "s" $$ (RVar "m" $$ RVar "s" $$ RVar "z")) $
  let_ "n10" (RVar "add" $$ RVar "n5" $$ RVar "n5") $
  let_ "n15" (RVar "add" $$ RVar "n5" $$ RVar "n10") $
  let_ "n20" (RVar "add" $$ RVar "n5" $$ RVar "n15") $
  let_ "mktree" (RLam "n" $ RLam "node" $ RLam "l" $
                 RVar "n" $$ (RLam "x" $ RVar "node" $$ RVar "x" $$ RVar "x") $$ RVar "l") $
  RVar "mktree" $$ RVar "n20"

def prog4 :=
  let_ "zero" (RLam "s" $ RLam "z" $ RVar "z") $
  let_ "suc" (RLam "n" $ RLam "s" $ RLam "z" $ RVar "s" $$ (RVar "n" $$ RVar "s" $$ RVar "z")) $
  let_ "n5" (RVar "suc" $$ (RVar "suc" $$ (RVar "suc" $$ (RVar "suc" $$ (RVar "suc" $$ RVar "zero"))))) $
  let_ "add" (RLam "n" $ RLam "m" $ RLam "s" $ RLam "z" $ RVar "n" $$ RVar "s" $$ (RVar "m" $$ RVar "s" $$ RVar "z")) $
  let_ "n10" (RVar "add" $$ RVar "n5" $$ RVar "n5") $
  let_ "n15" (RVar "add" $$ RVar "n5" $$ RVar "n10") $
  let_ "n20" (RVar "add" $$ RVar "n5" $$ RVar "n15") $
  let_ "mktree" (RLam "n" $ RLam "node" $ RLam "l" $
                 RVar "n" $$ (RLam "x" $ RVar "node" $$ RVar "x" $$ RVar "x") $$ RVar "l") $
  RVar "mktree" $$ RVar "n20" $$ (RLam "_" $ RLam "_" $ RVar "zero") $$ RVar "zero"

unsafe def run : RTm → String := reprStr ∘ nf0 ∘ elabTm

@[noinline]
unsafe def force(t : RTm) : UInt64 :=
  match nf0 (elabTm t) with
  | Lam _ _ => 0
  | _       => 1

inductive Tree where
  | Leaf : UInt64 -> Tree
  | Node : Tree -> Tree -> Tree
deriving Repr

open Tree

def maptree (t : Tree) : Tree :=
  match t with
  | Leaf x => Leaf (x + 100)
  | Node l r => Node (maptree l) (maptree r)

unsafe def go (n m : UInt64) : Tree :=
  match n, m with
  | 0, _ => Leaf 0
  | _, 0 => Leaf 0
  | n, m => Node (go (n - 1) (m - 1)) (go (m - 1) (n - 1))

unsafe def mktree n := go n n

def iter : UInt64 := 10

@[noinline]
unsafe def gotimed (times : UInt64)(act : α → IO β)(start : α) : IO Unit :=
  if times == 0 then
     pure ()
  else do
     _ <- act start
     gotimed (times - 1) act start

unsafe def timed (msg : String)(times : UInt64)(act : α → IO β)(start : α) : IO Unit :=
  timeit msg (gotimed  times act start)

unsafe def main := do
  timed "Tree NF" iter (pure ∘ force) prog
  timed "Tree Conv" iter (fun t => (let v := eval0 (elabTm t); pure (conv0 v v))) prog
  timed "Tree force" iter (pure ∘ force) prog2
  timed "Tree NF share" iter (pure ∘ force) prog3
  timed "Tree Conv share" iter (fun t => (let v := eval0 (elabTm t); pure (conv0 v v))) prog3
  timed "Tree force share" iter (pure ∘ force) prog4
  timed "Maptree 1/2" iter (fun (n : UInt64) => pure (maptree (mktree n))) 20
  timed "Maptree 2/3" iter (fun (n : UInt64) => pure (maptree (maptree (mktree n)))) 20
  timed "Maptree 3/4" iter (fun (n : UInt64) => pure (maptree (maptree (maptree (mktree n))))) 20
  timed "Maptree 4/5" iter (fun (n : UInt64) => pure (maptree (maptree (maptree (maptree (mktree n)))))) 20
