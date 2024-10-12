//@ts-check
'use strict';

/** @typedef {String} Name */

function impossible(){
    throw new Error('Impossible')
}

// Closed and Open values
// ----------------------------------------------------------------------------------------------------

const _Var       = 'Var'
const _Let       = 'Let'
const _Lam       = 'Lam'
const _App       = 'App'
const _Erased    = 'Erased'
const _Quote     = 'Quote'
const _Splice    = 'Splice'
const _Return    = 'Return'
const _Bind      = 'Bind'
const _Seq       = 'Seq'
const _New       = 'New'
const _Write     = 'Write'
const _Read      = 'Read'
const _Closed    = 'Closed'
const _Closure   = 'Closure'
const _CSP       = 'CSP'
const _LiftedLam = 'LiftedLam'
const _Body      = 'Body'


/**
  @typedef {
  {tag: _Var, _1: Name} |
  {tag: _Let, _1: Name, _2: Open, _3: (Open) => Open} |
  {tag: _Lam, _1: Name, _2: (Open) => Open} |
  {tag: _App, _1: Open, _2: Open} |
  {tag: _Erased} |
  {tag: _Quote, _1: Open} |
  {tag: _Splice, _1: Open} |
  {tag: _Return, _1: Open} |
  {tag: _Bind, _1: Name, _2: Open, _3: (Open) => Open} |
  {tag: _Seq, _1: Open, _2: Open} |
  {tag: _New, _1: Open} |
  {tag: _Write, _1: Open, _2: Open} |
  {tag: _Read, _1: Open} |
  {tag: _Closed, _1: Closed}
  } Open

  @typedef {
  undefined          |
  {_1: any, _2: any} |
  Open               |
  {_1 : Closed}
  } Closed
*/

/** @type {(Name) => Open} */
function Var_    (x)       {return {tag: _Var, _1: x}}
/** @type {(x:Name, t:Open, u:(Open) => Open) => Open} */
function Let_    (x, t, u) {return {tag: _Let, _1: x, _2: t, _3: u}}
/** @type {(x:Name, t:(Open) => Open) => Open} */
function Lam_    (x, t)    {return {tag: _Lam, _1: x, _2: t}}
/** @type {(t:Open, u:Open) => Open} */
function App_    (t, u)    {return {tag: _App, _1: t, _2: t}}
/** @type {Open} */
const    Erased_ =  {tag: _Erased}
/** @type {(Open) => Open} */
function Quote_  (t)       {return {tag: _Quote, _1: t}}
/** @type {(Open) => Open} */
function Splice_ (t)       {return {tag: _Splice, _1: t}}
/** @type {(Open) => Open} */
function Return_ (t)       {return {tag: _Return, _1: t}}
/** @type {(x:Name, t:Open, u:(Open) => Open) => Open} */
function Bind_   (x, t, u) {return {tag: _Bind, _1: x, _2: t, _3: u}}
/** @type {(t:Open, u:Open) => Open} */
function Seq_    (t, u)    {return {tag: _Seq, _1: t, _2: u}}
/** @type {(Open) => Open} */
function New_    (t)       {return {tag: _New, _1: t}}
/** @type {(t:Open, u:Open) => Open} */
function Write_  (t, u)    {return {tag: _Write, _1: t, _2: u}}
/** @type {(Open) => Open} */
function Read_   (t)       {return {tag: _Read, _1: t}}
/** @type {(Closed) => Open} */
function Closed_ (t)       {return {tag: _Closed, _1: t}}


// Closure conversion
// ----------------------------------------------------------------------------------------------------

/**
   @typedef {{isClosed: Boolean, isTop: Boolean, name: String}} EnvEntry
   @typedef {Number|undefined} Mode

   @typedef {
   {tag: _Var, _1: Name} |
   {tag: _CSP, _1: Name, _2: Number} |
   {tag: _Let, _1: Name, _2: Tm, _3: Tm} |
   {tag: _Lam, _1: Name, _2: Tm} |
   {tag: _LiftedLam, _1: Name, _2: Array<Name>} |
   {tag: _App, _1: Tm, _2: Tm} |
   {tag: _Erased} |
   {tag: _Quote, _1: Tm} |
   {tag: _Splice, _1: Tm} |
   {tag: _Return, _1: Tm} |
   {tag: _Bind, _1: Name, _2: Tm, _3: Tm} |
   {tag: _Seq, _1: Tm, _2: Tm} |
   {tag: _New, _1: Tm} |
   {tag: _Write, _1: Tm, _2: Tm} |
   {tag: _Read, _1: Tm}
   } Tm

   @typedef {
   {tag: _Let, _1: Name, _2: Tm, _3: Top} |
   {tag: _Bind, _1 : Name, _2: Tm, _3: Top} |
   {tag: _Seq, _1: Tm, _2: Top} |
   {tag: _Body, _1: Tm} |
   {tag: _Closure, _1: Name, _2: Array<Name>, _3: Name, _4: Tm, _5: Top}
   } Top
*/

/** @type {(Name) => Tm} */
function TVar_       (x) {return {tag: _Var, _1: x } }
/** @type {(x:Name, i:Number) => Tm} */
function TCSP_       (x, i) {return {tag: _CSP, _1: x , _2: i} }
/** @type {(x:Name, t:Tm, u:Tm) => Tm} */
function TLet_       (x,t,u) {return {tag: _Let, _1:x , _2:u , _3:u } }
/** @type {(x:Name, t:Tm) => Tm} */
function TLam_       (x,t) {return {tag: _Lam, _1:x , _2: t } }
/** @type {(x:Name, args:Array<Name>) => Tm} */
function TLiftedLam_ (x,args) {return {tag: _LiftedLam, _1: x , _2: args} }
/** @type {(t:Tm, u:Tm) => Tm} */
function TApp_       (t,u) {return {tag: _App, _1: t , _2: u}}
/** @type {Tm} */
const    TErased_    = {tag: _Erased}
/** @type {(Tm) => Tm} */
function TQuote_     (t) {return {tag: _Quote, _1: t } }
/** @type {(Tm) => Tm} */
function TSplice_    (t) {return {tag: _Splice, _1: t } }
/** @type {(Tm) => Tm} */
function TReturn_    (t) {return {tag: _Return, _1: t } }
/** @type {(x:Name, t:Tm, u:Tm) => Tm} */
function TBind_      (x, t, u) {return {tag: _Bind, _1: x , _2: t , _3: u } }
/** @type {(t:Tm, u:Tm) => Tm} */
function TSeq_       (t, u) {return {tag: _Seq, _1: t , _2: u } }
/** @type {(Tm) => Tm} */
function TNew_       (t) {return {tag: _New, _1: t } }
/** @type {(t:Tm, u:Tm) => Tm} */
function TWrite_     (t, u) {return {tag: _Write, _1: t , _2: u } }
/** @type {(Tm) => Tm} */
function TRead_      (t) {return {tag: _Read, _1: t }}

/** @type {(x:Name, t:Tm, u:Top) => Top} */
function TopLet_     (x, t, u) { return {tag: _Let, _1: x , _2: t, _3: u } }
/** @type {(x:Name, t:Tm, u:Top) => Top} */
function TopBind_    (x, t, u) { return {tag: _Bind, _1: x , _2: t, _3: u } }
/** @type {(t:Tm, u:Top) => Top} */
function TopSeq_     (t, u) { return {tag: _Seq, _1: t , _2: u } }
/** @type {(Tm) => Top} */
function TopBody_    (t) { return {tag: _Body, _1: t} }
/** @type {(x:Name, env: Array<Name>, arg:Name, body:Tm, t:Top) => Top} */
function TopClosure_ (x, env, arg, body, t) { return {tag: _Closure, _1: x , _2: env, _3: arg , _4: body , _5: t}}



// ----------------------------------------------------------------------------------------------------

/** @type {Array<EnvEntry>} */
const env = new Array()

/** @type Mode */
let mode = undefined

/** @type {(String) => String} */
function freshenName(x){
    let res = x
    while (env.findIndex((e) => e.name === res) !== -1){
	res = res + env.length
    }
    return res
}

// --------------------------------------------------------------------------------





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

// // CCConfig -> String -> String
// function freshenName(c, x) {
//     if (c.env.findIndex((e) => e.name === x) === -1) {
// 	return x
//     } else {
// 	return freshenName(c, x + c.env.length)
//     }
// }

// {
//     const env  = [{isClosed: true, isTop: true, name: 'mallac'}]
//     const conf = {env: env, mode: undefined, topName: 'foo'}
//     console.log(freshenName(conf, 'mallac'))
// }

// // CCConfig -> Name -> (CCConfig -> Name -> a) -> a
// function fresh(c, x, act) {
//     const x2 = freshenName(c, x)
//     c.env.push({isClosed: false, isTop: false, name: x2 })
//     const a = act(c, x2)
//     c.env.pop()
//     return a
// }


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

// const foo = Lam_('x', (x) => {return x});
// const bar = Lam_('x', (x) => {return App_(openSplice_(Closed_(foo)), x)});
// const main_ = () => {return undefined};
// console.log(main_())
