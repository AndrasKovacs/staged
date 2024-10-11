'use strict';

function impossible(){
    throw new Error('Impossible')
}

// ADT for open values

// const _Var    = 0
// const _Let    = 1
// const _Lam    = 2
// const _App    = 3
// const _Erased = 4
// const _Quote  = 5
// const _Splice = 6
// const _Return = 7
// const _Bind   = 8
// const _Seq    = 9
// const _New    = 10
// const _Write  = 11
// const _Read   = 12
// const _Closed = 13

const _Var    = 'Var'
const _Let    = 'Let'
const _Lam    = 'Lam'
const _App    = 'App'
const _Erased = 'Erased'
const _Quote  = 'Quote'
const _Splice = 'Splice'
const _Return = 'Return'
const _Bind   = 'Bind'
const _Seq    = 'Seq'
const _New    = 'New'
const _Write  = 'Write'
const _Read   = 'Read'
const _Closed = 'Closed'

function Var_    (x)       {return {tag: _Var, _1: x}}
function Let_    (x, t, u) {return {tag: _Let, _1: x, _2: t, _3: u}}
function Lam_    (x, t)    {return {tag: _Lam, _1: x, _2: t}}
function App_    (t, u)    {return {tag: _App, _1: t, _2: t}}
const    Erased_ =         {tag: _Erased}
function Quote_  (t)       {return {tag: _Quote, _1: t}}
function Splice_ (t)       {return {tag: _Splice, _1: t}}
function Return_ (t)       {return {tag: _Return, _1: t}}
function Bind_   (x, t, u) {return {tag: _Bind, _1: x, _2: t, _3: u}}
function Seq_    (t, u)    {return {tag: _Seq, _1: t, _2: u}}
function New_    (t)       {return {tag: _New, _1: t}}
function Write_  (t, u)    {return {tag: _Write, _1: t, _2: u}}
function Read_   (t)       {return {tag: _Read, _1: t}}
function Closed_ (t)       {return {tag: _Closed, _1: t}}


// Closure conversion
// --------------------------------------------------------------------------------

const _Closure   = 'Closure'
const _CSP       = 'CSP'
const _LiftedLam = 'LiftedLam'
const _Body      = 'Body'

// data Top
//   = TLet Name Tm Top
//   | TBind Name Tm Top
//   | TSeq Tm Top
//   | TBody Tm
//   | TClosure Name [Name] Name Tm Top -- name, env, arg, body
//   deriving Show

// data Tm
//   = Var Name
//   | CSP Name
//   | Let Name Tm Tm
//   | LiftedLam Name [Name] -- name, env application
//   | Lam Name Tm
//   | App Tm Tm
//   | Erased String
//   | Quote Tm
//   | Splice Tm (Maybe SourcePos)
//   | Return Tm
//   | Bind Name Tm Tm
//   | Seq Tm Tm
//   | New Tm
//   | Write Tm Tm
//   | Read Tm
//   deriving Show

// type TopClosures = Array {name: String, env: Array String, arg: String, body: Tm}
// type CCEnv       = Array {isClosed: Bool, isTop: Bool, name: String}
// type CCConfig    = {env: CCEnv, mode: Int | undefined, topName: String}

// WE SHALL MAKE all the env's and configs mutable top-level!!!!

// CCConfig -> String -> String
function freshenName(c, x) {
    if (c.env.findIndex((e) => e.name === x) === -1) {
	return x
    } else {
	return freshenName(c, x + c.env.length)
    }
}

{
    const env  = [{isClosed: true, isTop: true, name: 'mallac'}]
    const conf = {env: env, mode: undefined, topName: 'foo'}
    console.log(freshenName(conf, 'mallac'))
}

// CCConfig -> Name -> (CCConfig -> Name -> a) -> a
function fresh(c, x, act) {
    const x2 = freshenName(c, x)
    c.env.push({isClosed: false, isTop: false, name: x2 })
    const a = act(c, x2)
    c.env.pop()
    return a
}

// CCConfig ->
function bind(


// -- create fresh name, run action, delete bound name from freevars of the result
// bind :: Name -> (Env => Name -> State S a) -> Env => State S a
// bind x act = fresh x \x -> do
//   a <- act x
//   s <- get
//   freeVars %= S.delete x
//   pure a



// --------------------------------------------------------------------------------

function openApp_(t, u) {
    if (t.tag === _Closed) {
        if (u.tag === _Closed) {
            return Closed_(t._1._1(u._1))
        } else {
            return t._1._2(u)
        }
    } else if (t.tag === _Lam) {
        return t._1(u)
    } else {
        impossible()
    }
}

function openSplice_(t) {
    if (t.tag === _Closed){
        return t._1._1
    } else if (t.tag === _Quote) {
        return t._1
    } else {
	return Splice_(t)
    }
}

function openSplice0_(t) {
    throw new Error('code generation not implemented')
}

function closedExec_(t){
    throw new Error('code generation not implemented')
}

function closedEval_(t){
    throw new Error('code generation not implemented')
}

// --------------------------------------------------------------------------------

const foo = Lam_('x', (x) => {return x});
const bar = Lam_('x', (x) => {return App_(openSplice_(Closed_(foo)), x)});
const main_ = () => {return undefined};
console.log(main_())

/*

to : {A : Set} → (A → Set) → (B : Set) * (B → A)
to f = ((a : A) * f a, fst)

from : {A : Set} → ((B : Set) * (B → A)) → (A → Set)
from (B, f) a = (b : B) * (f b = a)

(A B C : Set) * (C → B) * (B → A)                      ~ (reorder components)
(A : Set) * (B : Set) * (B → A) * (C : Set) * (C → B)  ~ (apply "to")
(A : Set) * (B : Set) * (B → A) * (B → Set)             ~ (associativity of Σ)
(A : Set) * (Bf : (B : Set) * (B → A)) * (fst Bf → Set) ~ (apply "from")
(A : Set) * (B : A → Set) * (fst (to B) → Set)           =
(A : Set) * (B : A → Set) * (((a : A) * B a) → Set)      ~ (distribution of Π and Σ)
(A : Set) * (A → (B : Set) * (B → Set))



*/
