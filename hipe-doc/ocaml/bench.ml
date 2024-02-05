

type rtm
  = RVar of string
  | RApp of rtm * rtm
  | RLam of string * rtm

type tm
  = Var of int
  | App of tm * tm
  | Lam of string * tm

let rec show(t : tm) =
  match t with
  | Var(x)    -> "Var(" ^ string_of_int x ^ ")"
  | App(t, u) -> "App(" ^ show t ^ ", " ^ show u ^ ")"
  | Lam(x, t) -> "Lam(" ^ x ^ ", " ^ show t ^ ")"

let undef() = raise (Failure "undefined")

let rec elabVar (ns : string list) (x : string) =
  match ns with
  | x' :: ns -> if x == x' then 0 else elabVar ns x + 1
  | _        -> undef()

let rec elab' (ns : string list) = function
  | RVar(x)    -> Var(elabVar ns x)
  | RApp(t, u) -> App(elab' ns t, elab' ns u)
  | RLam(x, t) -> Lam(x, elab' (x::ns) t)

let elab = elab' []

type value
  = VVar of int
  | VApp of value * value
  | VLam of string * env * tm

and env = value list

let rec vvar (e : env) (x : int) = match (e, x) with
  | v :: _, 0 -> v
  | _ :: e, x -> vvar e (x - 1)
  | _         -> undef()

let rec eval e = function
  | Var(x)    -> vvar e x
  | App(t, u) -> (match eval e t with
                   | VLam(_, e', t) -> eval (eval e u :: e') t
                   | t              -> VApp(t, eval e u))
  | Lam(x, t) -> VLam(x, e, t)

let rec conv l t t' = match (t, t') with
  | VVar(x), VVar(x') -> x == x'
  | VApp(t, u), VApp(t', u') -> conv l t t' && conv l u u'
  | VLam(x, e, t), VLam(x', e', t') ->
     let v = VVar(l) in
     conv (l + 1) (eval (v :: e) t) (eval (v :: e') t')
  | _ -> false

let rec quote (l : int) = function
  | VVar(x)       -> Var(l - x - 1)
  | VApp(t, u)    -> App(quote l t, quote l u)
  | VLam(x, e, t) -> Lam(x, quote (l + 1) (eval (VVar(l)::e) t))

let eval0 = eval []
let conv0 = conv 0
let quote0 = quote 0
let nf0 x = quote0 (eval0 x)

let let'(x, t, u)     = RApp(RLam(x, u), t) [@@inline]
let rapp2(t, u, v)    = RApp(RApp(t, u), v) [@@inline]
let rapp3(t, u, v, w) = RApp(RApp(RApp(t, u), v), w) [@@inline]

let vx = RVar("x")
let vy = RVar("y")
let vz = RVar("z")
let vs = RVar("s")
let vt = RVar("t")
let vn = RVar("n")
let vm = RVar("m")
let vc = RVar("c")
let vl = RVar("l")
let vr = RVar("r")

let prog =
  let'("zero", RLam("s", RLam("z", vz)),
  let'("suc", RLam("n", RLam("s", RLam("z", RApp(vs, rapp2(vn, vs, vz))))),
  let'("five", RApp(RVar("suc"), RApp(RVar("suc")
              , RApp(RVar("suc"), RApp(RVar("suc"), RApp(RVar("suc"), RVar("zero")))))),
  let'("add", RLam("n", RLam("m", RLam("s", RLam("z", rapp2(vn, vs, rapp2(vm, vs, vz)))))),
  let'("ten", rapp2(RVar("add"), RVar("five"), RVar("five")),
  let'("n15", rapp2(RVar("add"), RVar("five"), RVar("ten")),
  let'("n20", rapp2(RVar("add"), RVar("five"), RVar("n15")),
  let'("leaf", RLam("n", RLam("l", RVar("l"))),
  let'("node", RLam("t1", RLam("t2", RLam("n", RLam("l",
              rapp2(vn, rapp2(RVar("t1"), vn, vl), rapp2(RVar("t2"), vn, vl)))))),
  let'("mktree", RLam("n", rapp2(vn, (RLam("t", rapp2(RVar("node"), vt, vt))), RVar("leaf"))),
  RApp(RVar("mktree"), (RVar("n20")))
  ))))))))))

let prog2 =
  let'("zero", RLam("s", RLam("z", vz)),
  let'("suc", RLam("n", RLam("s", RLam("z", RApp(vs, rapp2(vn, vs, vz))))),
  let'("five", RApp(RVar("suc"), RApp(RVar("suc")
              , RApp(RVar("suc"), RApp(RVar("suc"), RApp(RVar("suc"), RVar("zero")))))),
  let'("add", RLam("n", RLam("m", RLam("s", RLam("z", rapp2(vn, vs, rapp2(vm, vs, vz)))))),
  let'("ten", rapp2(RVar("add"), RVar("five"), RVar("five")),
  let'("n15", rapp2(RVar("add"), RVar("five"), RVar("ten")),
  let'("n20", rapp2(RVar("add"), RVar("five"), RVar("n15")),
  let'("leaf", RLam("n", RLam("l", RVar("l"))),
  let'("node", RLam("t1", RLam("t2", RLam("n", RLam("l",
              rapp2(vn, rapp2(RVar("t1"), vn, vl), rapp2(RVar("t2"), vn, vl)))))),
  let'("mktree", RLam("n", rapp2(vn, (RLam("t", rapp2(RVar("node"), vt, vt))), RVar("leaf"))),
  rapp2(RApp(RVar("mktree"), (RVar("n20"))), RLam("_", RLam("_", RVar("leaf"))), RVar("leaf"))
  ))))))))))

let prog3 =
  let'("zero", RLam("s", RLam("z", vz)),
  let'("suc", RLam("n", RLam("s", RLam("z", RApp(vs, rapp2(vn, vs, vz))))),
  let'("five", RApp(RVar("suc"), RApp(RVar("suc")
              , RApp(RVar("suc"), RApp(RVar("suc"), RApp(RVar("suc"), RVar("zero")))))),
  let'("add", RLam("n", RLam("m", RLam("s", RLam("z", rapp2(vn, vs, rapp2(vm, vs, vz)))))),
  let'("ten", rapp2(RVar("add"), RVar("five"), RVar("five")),
  let'("n15", rapp2(RVar("add"), RVar("five"), RVar("ten")),
  let'("n20", rapp2(RVar("add"), RVar("five"), RVar("n15")),
  let'("n25", rapp2(RVar("add"), RVar("five"), RVar("n20")),
  let'("mktree", RLam("n", RLam("node", RLam("l", rapp2(RVar("n")
                     , RLam("x", rapp2(RVar("node"), RVar("x"), RVar("x")))
                     , RVar("l"))))),
  RApp(RVar("mktree"), (RVar("n20")))
  )))))))))

let prog4 =
  let'("zero", RLam("s", RLam("z", vz)),
  let'("suc", RLam("n", RLam("s", RLam("z", RApp(vs, rapp2(vn, vs, vz))))),
  let'("five", RApp(RVar("suc"), RApp(RVar("suc")
              , RApp(RVar("suc"), RApp(RVar("suc"), RApp(RVar("suc"), RVar("zero")))))),
  let'("add", RLam("n", RLam("m", RLam("s", RLam("z", rapp2(vn, vs, rapp2(vm, vs, vz)))))),
  let'("ten", rapp2(RVar("add"), RVar("five"), RVar("five")),
  let'("n15", rapp2(RVar("add"), RVar("five"), RVar("ten")),
  let'("n20", rapp2(RVar("add"), RVar("five"), RVar("n15")),
  let'("n25", rapp2(RVar("add"), RVar("five"), RVar("n20")),
  let'("mktree", RLam("n", RLam("node", RLam("l", rapp2(RVar("n")
                     , RLam("x", rapp2(RVar("node"), RVar("x"), RVar("x")))
                     , RVar("l"))))),
  rapp2(RApp(RVar("mktree"), (RVar("n20"))), RLam("_", RLam("_", RVar("zero"))), RVar("zero"))
    )))))))))

let run x = show (nf0 (elab x))

let force t : int = match nf0 (elab t) with
  | Lam(_) -> 0
  | _      -> 1

type tree
  = Node of tree * tree
  | Leaf of int

let rec maptree = function
  | Leaf(n)    -> Leaf(n + 100)
  | Node(l, r) -> Node(maptree l, maptree r)

let rec mktree (n : int) =
  let rec go n n = match (n, n) with
    | 0, _ -> Leaf(0)
    | _, 0 -> Leaf(0)
    | n, m -> Node(go (n - 1) (m - 1), go (m - 1) (n - 1)) in
  go n n

let timed (msg : string) (times : int)(act : 'a -> 'b) (start : 'a) =
  let t = Sys.time() in
  for i = 1 to times do
    let _ = act start in
    ()
  done;
  let t' = Sys.time() in
  let pad = String.make (20 - String.length msg) ' ' in
  Printf.printf "%s:%s%f\n%!" msg pad ((t' -. t) /. (float_of_int times))
[@@noinline]

let iter : int = 30

let _ = timed "Tree NF" iter force prog
let _ = timed "Tree Conv" iter (fun t -> let v = eval0 (elab t) in conv0 v v) prog
let _ = timed "Tree force" iter force prog2
let _ = timed "Tree NF share" iter force prog3
let _ = timed "Tree Conv share" iter (fun t -> let v = eval0 (elab t) in conv0 v v) prog3
let _ = timed "Tree force share" iter force prog4
let _ = timed "Maptree 1/2" iter (fun n -> let _ = maptree(mktree n) in ()) 20
let _ = timed "Maptree 2/3" iter (fun n -> let _ = maptree(maptree (mktree n)) in ()) 20
let _ = timed "Maptree 3/4" iter (fun n -> let _ = maptree(maptree(maptree (mktree n))) in ()) 20
let _ = timed "Maptree 4/5" iter (fun n -> let _ = maptree(maptree(maptree(maptree(mktree n)))) in ()) 20

(* export OCAMLRUNPARAM="v=0x400,s=4000000" *)
(* /usr/bin/time -v ./bench  *)
