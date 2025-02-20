//@ts-check
'use strict';

const util_ = require('util');
const reader_ = require("readline-sync"); //npm install readline-sync
const debug_ = (x) => {
  console.log(util_.inspect(x, false, null))
}

/** @typedef {String} Name */

// Closed and Open values
// ----------------------------------------------------------------------------------------------------

const _Var       = 'Var'
const _Let       = 'Let'
const _Lam       = 'Lam'
const _App       = 'App'
const _Quote     = 'Quote'
const _Splice    = 'Splice'
const _Return    = 'Return'
const _Bind      = 'Bind'
const _Seq       = 'Seq'
const _New       = 'New'
const _Write     = 'Write'
const _Read      = 'Read'
const _Closure   = 'Closure'
const _CSP       = 'CSP'
const _LiftedLam = 'LiftedLam'
const _Body      = 'Body'
const _NatElim   = 'NatElim'
const _ReadNat   = 'ReadNat'
const _PrintNat  = 'PrintNat'
const _Log       = 'Log'
const _Rec       = 'Rec'
const _Suc       = 'Suc'
const _Proj      = 'Proj'
const _NatLit    = 'NatLit'

/**
  @typedef {
  {tag: _Var, name: Name} |
  {tag: _Let, _1: Name, _2: Open, _3: (v: Open) => Open} |
  {tag: _Lam, _1: Name, _2: (v: Open) => Open} |
  {tag: _App, _1: Open, _2: Open} |
  {tag: _Quote, _1: Open} |
  {tag: _Splice, _1: Open} |
  {tag: _Return, _1: Open} |
  {tag: _Bind, _1: Name, _2: Open, _3: (v: Open) => Open} |
  {tag: _Seq, _1: Open, _2: Open} |
  {tag: _New, _1: Open} |
  {tag: _Write, _1: Open, _2: Open} |
  {tag: _Read, _1: Open} |
  {tag: _CSP, _1: Closed, _2: Name} |

  {tag: _NatElim, _1 : Open, _2: Open, _3: Open} |
  {tag: _ReadNat} |
  {tag: _PrintNat, _1: Open} |
  {tag: _Log, _1: String} |
  {tag: _Rec, _1: Map<String, Open>} |
  {tag: _Suc, _1: Open} |
  {tag: _Proj, _1: Open, _2: Name}
  } Open

  @typedef {
    undefined |
    Object |
    Number |
    Open |
    {_1 : Closed}
    } Closed
*/

/** @type {(x:Name) => Open} */
function Var_    (x)       {return {tag: _Var, name: x}}
/** @type {(x:Name, t:Open, u:(v: Open) => Open) => Open} */
function Let_    (x, t, u) {return {tag: _Let, _1: x, _2: t, _3: u}}
/** @type {(x:Name, t:(v: Open) => Open) => Open} */
function Lam_    (x, t)    {return {tag: _Lam, _1: x, _2: t}}
/** @type {(t:Open, u:Open) => Open} */
function App_    (t, u)    {return {tag: _App, _1: t, _2: u}}
/** @type {(t: Open) => Open} */
function Quote_  (t)       {return {tag: _Quote, _1: t}}
/** @type {(t: Open) => Open} */
function Splice_ (t)       {return {tag: _Splice, _1: t}}
/** @type {(t: Open) => Open} */
function Return_ (t)       {return {tag: _Return, _1: t}}
/** @type {(x:Name, t:Open, u:(v: Open) => Open) => Open} */
function Bind_   (x, t, u) {return {tag: _Bind, _1: x, _2: t, _3: u}}
/** @type {(t:Open, u:Open) => Open} */
function Seq_    (t, u)    {return {tag: _Seq, _1: t, _2: u}}
/** @type {(t: Open) => Open} */
function New_    (t)       {return {tag: _New, _1: t}}
/** @type {(t:Open, u:Open) => Open} */
function Write_  (t, u)    {return {tag: _Write, _1: t, _2: u}}
/** @type {(t: Open) => Open} */
function Read_   (t)       {return {tag: _Read, _1: t}}
/** @type {(t: Closed, x:Name) => Open} */
function CSP_ (t,x)        {return {tag: _CSP, _1: t, _2:x}}
/** @type {(s: Open, z: Open, n: Open) => Open} */
function NatElim_(s, z, n) {return {tag: _NatElim, _1: s, _2: z, _3: n}}
/** @type {() => Open} */
function ReadNat_() {return {tag: _ReadNat}}
/** @type {(t: Open) => Open} */
function PrintNat_(t) {return {tag: _PrintNat, _1: t}}
/** @type {(s:String) => Open} */
function Log_(s) {return {tag: _Log, _1: s}}
/** @type {(ts: Map<String, Open>) => Open} */
function Rec_(ts) {return {tag: _Rec, _1: ts}}
/** @type {(n:Open) => Open} */
function Suc_(n) {return {tag: _Suc, _1: n}}
/** @type {(t:Open, x: Name) => Open} */
function Proj_(t, x) {return {tag: _Proj, _1: t, _2: x}}

/** @type {Open} */
const CSP_undefined_ = CSP_(undefined, 'undefined')


// Non-shadowing first-order term representation of code
// ----------------------------------------------------------------------------------------------------

/**
   @typedef {
   {tag: _Var, _1: Name} |
   {tag: _CSP, _1: Number, _2: Name} |
   {tag: _Let, _1: Name, _2: Tm, _3: Tm} |
   {tag: _Lam, _1: Name, _2: Tm} |
   {tag: _LiftedLam, _1: Name, _2: Name, _3: Array<Name>} |
   {tag: _App, _1: Tm, _2: Tm} |
   {tag: _Quote, _1: Tm} |
   {tag: _Splice, _1: Tm} |
   {tag: _Return, _1: Tm} |
   {tag: _Bind, _1: Name, _2: Tm, _3: Tm} |
   {tag: _Seq, _1: Tm, _2: Tm} |
   {tag: _New, _1: Tm} |
   {tag: _Write, _1: Tm, _2: Tm} |
   {tag: _Read, _1: Tm} |
   {tag: _Log, _1: String} |
   {tag: _ReadNat} |
   {tag: _PrintNat, _1: Tm} |
   {tag: _Rec, _1: Map<Name, Tm>} |
   {tag: _Proj, _1: Tm, _2: Name} |
   {tag: _Suc, _1: Tm} |
   {tag: _NatElim, _1: Tm, _2 : Tm, _3 : Tm} |
   {tag: _NatLit, _1: Number}
   } Tm

  // Top-level terms are only used in closure conversion in closed codegen
   @typedef {
   {tag: _Let, _1: Name, _2: Tm, _3: Top} |
   {tag: _Bind, _1 : Name, _2: Tm, _3: Top} |
   {tag: _Seq, _1: Tm, _2: Top} |
   {tag: _Body, _1: Tm} |
   {tag: _Closure, _1: Name, _2: Array<Name>, _3: Name, _4: Tm, _5: Top}
   } Top
*/

/** @type {(x: Name) => Tm} */
function TVar_       (x) {return {tag: _Var, _1: x } }
/** @type {(x:Number, y:Name) => Tm} */
function TCSP_       (i, x) {return {tag: _CSP, _1: i, _2:x} }
/** @type {(x:Name, t:Tm, u:Tm) => Tm} */
function TLet_       (x,t,u) {return {tag: _Let, _1:x , _2:t , _3:u } }
/** @type {(x:Name, t:Tm) => Tm} */
function TLam_       (x,t) {return {tag: _Lam, _1:x , _2: t } }
/** @type {(f:Name, x:Name, args:Array<Name>) => Tm} */
function TLiftedLam_ (f, x, args) {return {tag: _LiftedLam, _1: f , _2:x, _3: args} }
/** @type {(t:Tm, u:Tm) => Tm} */
function TApp_       (t,u) {return {tag: _App, _1: t , _2: u}}
/** @type {(t: Tm) => Tm} */
function TQuote_     (t) {return {tag: _Quote, _1: t } }
/** @type {(t: Tm) => Tm} */
function TSplice_    (t) {return {tag: _Splice, _1: t } }
/** @type {(t: Tm) => Tm} */
function TReturn_    (t) {return {tag: _Return, _1: t } }
/** @type {(x:Name, t:Tm, u:Tm) => Tm} */
function TBind_      (x, t, u) {return {tag: _Bind, _1: x , _2: t , _3: u } }
/** @type {(t:Tm, u:Tm) => Tm} */
function TSeq_       (t, u) {return {tag: _Seq, _1: t , _2: u } }
/** @type {(t: Tm) => Tm} */
function TNew_       (t) {return {tag: _New, _1: t } }
/** @type {(t:Tm, u:Tm) => Tm} */
function TWrite_     (t, u) {return {tag: _Write, _1: t , _2: u } }
/** @type {(t: Tm) => Tm} */
function TRead_      (t) {return {tag: _Read, _1: t }}
/** @type {(t: Number) => Tm} */
function TNatLit_      (t) {return {tag: _NatLit, _1: t }}
/** @type {() => Tm} */
function TReadNat_() {return {tag: _ReadNat}}
/** @type {(t:Tm) => Tm} */
function TPrintNat_(t) {return {tag: _PrintNat, _1: t}}
/** @type {(ts:Map<Name, Tm>) => Tm} */
function TRec_(ts) {return {tag: _Rec, _1: ts}}
/** @type {(t:Tm, x: Name) => Tm} */
function TProj_(t, x) {return {tag: _Proj, _1: t, _2:x}}
/** @type {(t:Tm) => Tm} */
function TSuc_(t) {return {tag: _Suc, _1 : t}}
/** @type {(s:Tm, z:Tm, n:Tm) => Tm} */
function TNatElim_(s,z,n) {return {tag: _NatElim, _1 : s, _2: z, _3 : n}}
/** @type {(x:String) => Tm} */
function TLog_(x) {return {tag: _Log, _1 : x}}


/** @type {(x:Name, t:Tm, u:Top) => Top} */
function TopLet_     (x, t, u) { return {tag: _Let, _1: x , _2: t, _3: u } }
/** @type {(x:Name, t:Tm, u:Top) => Top} */
function TopBind_    (x, t, u) { return {tag: _Bind, _1: x , _2: t, _3: u } }
/** @type {(t:Tm, u:Top) => Top} */
function TopSeq_     (t, u) { return {tag: _Seq, _1: t , _2: u } }
/** @type {(t: Tm) => Top} */
function TopBody_    (t) { return {tag: _Body, _1: t} }
/** @type {(x:Name, env: Array<Name>, arg:Name, body:Tm, t:Top) => Top} */
function TopClosure_ (x, env, arg, body, t) { return {tag: _Closure, _1: x , _2: env, _3: arg , _4: body , _5: t}}

// SHARED TOP STATE
// ----------------------------------------------------------------------------------------------------

/** @type{Set<Name>} */
const boundVarSet_ = new Set();

/** @type {(x: String) => String} */
function freshenName_(x){
  let res = x
  while (boundVarSet_.has(res)){
    res = res + boundVarSet_.size
  }
  return res
}

//----------------------------------------------------------------------------------------------------

/** @type{(ls : undefined|Array<String>, code:String) => void} */
function displayCode_(loc, code){
  if (loc) {
    console.log('CODE GENERATED AT:')
    for (const l of loc){
      console.log(l)
    }
  } else {
    console.log('CODE GENERATED:')
  }
  console.log('CODE:')
  console.log(code)
  console.log('')
}

// CODE GENERATION CALLED FROM CLOSED EVALUATION
// ----------------------------------------------------------------------------------------------------

/** @type{(x: String, y: Array<Closed>, loc: undefined|Array<String>) => Closed} */
function evalCodeGenClosed_(src_, csp_, loc_) {
  displayCode_(loc_, src_);
  const res_ = eval(src_)();
  return res_;
}

/** @type {(t:Open, loc: undefined|Array<String>) => Closed} */
function codegenClosed_(t, loc) {

  // CLOSURE CONVERSION
  // ----------------------------------------------------------------------------------------------------
  /** @type {(t:Open) => {_1 : Top, _2 : Array<Closed>}} */
  function closureConvert(top){

    /** @typedef {{name : Name, env: Array<Name>, arg: Name, body: Tm}} Closure */

    /** @type {undefined|Number} */
    let stage = undefined

    /** @type{(s : Number, act: () => Tm) => Tm} */
    const inStage = (s, act) => {
      const backup = stage
      stage = s
      const res = act()
      stage = backup
      return res
    }

    /** @type {Set<Name>} */
    let freeVars = new Set()

    /** @type {Set<Name>} */
    let topVars = new Set()

    /** @type {Array<Closure>} */
    let closures = new Array()

    /** @type {Array<Closed>} */
    let cspArray = new Array()

    /** @type {Name} */
    let currentTopName = ''

    /** @type{(t:Open) => Top} */
    function goTop(top){

      /** @type {(cs: Array<Closure>, t: Top) => Top} */
      function addClosures(cs, t){
        let res = t
        for (const cl of cs.reverse()){
          res = TopClosure_(cl.name, cl.env, cl.arg, cl.body, res)
        }
        return res
      }

      /** @type {(name: Name) => void} */
      function resetAtTop(name){
        currentTopName = name
        freeVars.clear()
        closures = new Array()
      }

      switch (top.tag) {
        case _Let : {
          const x = freshenName_(top._1)
          const t = top._2
          const u = top._3
          resetAtTop(x)
          const t2 = go(t)
          const newClosures = closures
          boundVarSet_.add(x)
          topVars.add(x)
          const u2 = goTop(u(Var_(x)))
          return addClosures(newClosures, TopLet_(x, t2, u2))
        }
        case _Bind : {
          const x = freshenName_(top._1)
          const t = top._2
          const u = top._3
          resetAtTop(x)
          const t2 = go(t)
          const newClosures = closures
          boundVarSet_.add(x)
          topVars.add(x)
          const u2 = goTop(u(Var_(x)))
          return addClosures(newClosures, TopBind_(x, t2, u2))
        }

        case _Seq : {
          const t = top._1
          const u = top._2
          resetAtTop('$cl')
          const t2 = go(t)
          const newClosures = closures
          const u2 = goTop(u)
          return addClosures(newClosures, TopSeq_(t2, u2))
        }

        default: {
          resetAtTop('$cl')
          const t2 = go(top)
          return addClosures(closures, TopBody_(t2))
        }
      } // switch
    } // goTop

    /** @type {(x: Name, act: (x: Name) => Tm) => Tm} */
    function fresh(x, act){
      const x2 = freshenName_(x)
      boundVarSet_.add(x2)
      const res = act(x2)
      boundVarSet_.delete(x)
      return res
    }

    /** @type {(x : Name, act : (x: Name) => Tm) => Tm} */
    const bind = (x, act) =>
      fresh(x, (x) => {
        const a = act(x)
        freeVars.delete(x)
        return a
    })

    /** @type {(t: Open) => Tm} */
    function go(top){
      switch (top.tag){
        case _Var : {
          if (!topVars.has(top.name)) {
            freeVars.add(top.name)
          }
          return TVar_(top.name)
        }
        case _Let : {
          const x = top._1
          const t = top._2
          const u = top._3
          const t2 = go(t)
          return bind(x, (x) => TLet_(x, t2, go(u(Var_(x)))))
        }
        case _Lam : {
          const x = top._1
          const t = top._2
          if (stage === undefined) {
            return fresh(x, (x) => {
              let oldFreeVars = freeVars
              freeVars = new Set()
              const t2 = go(t(Var_(x)))
              freeVars.delete(x)
              const capture = Array.from(freeVars.values())
              const clName  = currentTopName + closures.length + '_'
              closures.push({name: clName, env: capture, arg: x, body: t2})
              freeVars.forEach((x) => oldFreeVars.add(x))
              freeVars = oldFreeVars
              return TLiftedLam_(clName, x, capture)
            })
          } else {
            return bind(x, (x) => TLam_(x, go(t(Var_(x)))))
          }
        }
        case _App : return TApp_(go(top._1), go(top._2))
        case _Quote : {
          if (stage === undefined) {
            return inStage(1, () => TQuote_(go(top._1)))
          } else {
            return inStage(stage + 1, () => TQuote_(go(top._1)))
          }
        }
        case _Splice : {
          if (stage && stage > 0){
            return inStage(stage - 1, () => TSplice_(go(top._1)))
          } else {
            return TSplice_(go(top._1))
          }
        }
        case _Bind : {
          const t2 = go(top._2)
          return bind(top._1, (x) => TBind_(x, t2, go(top._3(Var_(x)))))
        }
        case _Return : return TReturn_(go(top._1))
        case _Seq    : return TSeq_(go(top._1), go(top._2))
        case _New    : return TNew_(go(top._1))
        case _Write  : return TWrite_(go(top._1), go(top._2))
        case _Read   : return TRead_(go(top._1))

        case _CSP : {
          if (typeof top._1 === 'number'){  // inline closed numerals into source code
            return TNatLit_(top._1)
          } else {
            const id = cspArray.length
            cspArray.push(top._1)
            return TCSP_(id, top._2)
          }
        }
        case _Suc     : return TSuc_(go(top._1))
        case _NatElim : return TNatElim_(go(top._1), go(top._2), go(top._3))

        case _Rec : {
          const res = new Map()
          top._1.forEach((t, x) => {res.set(x, go(t))})
          return TRec_(res)
        }
        case _Proj     : return TProj_(go(top._1), top._2)
        case _PrintNat : return TPrintNat_(go(top._1))
        case _ReadNat  : return TReadNat_()
        case _Log      : return TLog_(top._1)
      } // switch
    } // go

    const res = goTop(top)
    return {_1: res, _2: cspArray}
  } // closureConvert

  // CODE EMISSION
  // ----------------------------------------------------------------------------------------------------

  /** @type{(t:Top) => String} */
  function emitCode(t){

    // code builder is simply an array of strings
    /** @type {Array<String>} */
    const builder = new Array()

    /** @type {() => String} */
    const build = () => {return builder.join('')}

    /** @type {Number} */
    let indentation = 0

    /** @type {Boolean} */
    let isTail = true

    // only used in open emission
    /** @type{Number} */
    let stage = 0

    /** @type{Set<Name>} */
    const closedVars = new Set()

    /** @type {(s:String) => void} */
    const put = (s) => {builder.push(s)}

    /** @type {(s:String) => () => void} */
    const str = (s) => () => put(s)

    /** @type {(s:String) => () => void} */
    const strLit = (s) => () => put("`" + s + "`")

    /** @type {() => void} */
    const newl = () => {put('\n' + ' '.repeat(indentation))}

    /** @type{(s : Number, act: () => void) => (() => void)} */
    const inStage = (s, act) => () => {
      const backup = stage
      stage = s
      const res = act()
      stage = backup
      return res
    }

    /** @type{ (act: () => void) => (() => void) } */
    const tail = (act) => () => {
      const backup = isTail
      isTail = true
      const res = act()
      isTail = backup
      return res
    }

    /** @type{ (act: () => void) => (() => void) } */
    const nonTail = (act) => () => {
      const backup = isTail
      isTail = false
      const res = act()
      isTail = backup
      return res
    }

    /** @type{ (act: () => void) => (() => void) } */
    const indent = (act) => () => {
      const backup = indentation
      indentation += 2
      const res = act()
      indentation = backup
      return res
    }

    /** @type{ () => void } */
    function semi(){put(';')}

    /** @type{ (act: () => void) => (() => void) } */
    const par = (act) => () => {put('('); act(); put(')')}

    /** @type{ (x: Name, closed: Boolean, act: () => void) => () => void} */
    const bind = (x, closed, act) => () => {
      if (closed) {closedVars.add(x)};
      const res = act();
      if (closed) {closedVars.delete(x)}
      return res;
    }

    /** @type{ (x: Name, closed: Boolean, t: () => void, u: () => void) => (() => void) } */
    const jLet = (x, closed, t, u) => () => {
      if (isTail){
        put('const ' + x + ' = '); indent(nonTail(t))(); semi(); newl(); tail(bind(x, closed, u))()
      } else {
        put('((' + x + ') => ');
        par(nonTail(bind(x, closed, u)))(); put(')('); nonTail(t)(); put(')')
      }
    }

    /** @type{ (t: () => void, u: () => void) => (() => void) } */
    const jSeq = (t, u) => () => {
      if (isTail){
        indent(nonTail(t))(); semi(); newl(); tail(u)()
      } else {
        put('((_) => '); par(nonTail(u))(); put(')('); nonTail(t)(); put(')')
      }
    }

    /** @type{(xs: Array<() => void>) => (() => void)} */
    const jTuple = (xs) => () => {
      if (xs.length === 0){
        put('()')
      } else {
        put('('); xs[0](); xs.slice(1, xs.length).forEach((act) => {put(', '); act()}); put(')')
      }
    }

    /** @type((t : () => void) => () => void)} */
    const jReturn = (t) => () => {
      if (isTail) { put('return '); nonTail(t)()
      } else      { t() }
    }

    /** @type{(xs : Array<Name>, closed: Boolean, t: () => void) => () => void} */
    const jLam = (xs, closed, t) => () => {
      jReturn(() => {
        jTuple(xs.map(str))();
        put(' => {');
        tail(() => {
          if (closed) {xs.forEach((x) => closedVars.add(x))}
          t();
          if (closed) {xs.forEach((x) => closedVars.delete(x))}
        })();
        put('}')
      })()
    }

    /** @type{(xs: Array<Name>, closed:Boolean, t: () => void) => () => void} */
    const jLamExp = (xs, closed, t) => () => {
      jReturn(() => {
        jTuple(xs.map(str))();
        put(' => ')
        nonTail(() => {
          if (closed) {xs.forEach((x) => closedVars.add(x))};
          t();
          if (closed) {xs.forEach((x) => closedVars.delete(x))};
          })()
      })()
    }

    /** @type{(t: () => void, u: () => void) => () => void} */
    const cApp = (t, u) => () => {
      jReturn(() => {par(t)(); put('._1'); par(u)()})()
    }

    /** @type{(ts : Map<Name, Tm>) => () => void} */
    const cRec = (ts) => () => {
      const arr = Array.from(ts);
      /** @type{(i:Number) => void} */
      function go(i) {
        if (i == arr.length){
          return;
        } else if (arr.length !== 0 && i == arr.length - 1){
          put(arr[i][0]);
          put(': ');
          return nonTail(() => ceval(arr[i][1]))();
        } else {
          put(arr[i][0]);
          put(': ');
          nonTail(() => ceval(arr[i][1]))();
          put(', ');
          return go(i+1);
        }
      }
      go(0)
    }

    /** @type{(t : () => void, args: Array<() => void>) => () => void} */
    const jApp = (t, args) => () => {
      jReturn(() => {t(); jTuple(args)() })()
    }

    /** @type{(t : () => void) => () => void} */
    const cRun = (t) => () => {
      jReturn(() => {t(); put('()') })()
    }

    /** @type{(env: Array<Name>, x: Name, closed:Boolean, t : () => void) => () => void} */
    const jClosure = (env, x, closed, t) => () => {
      if (env.length === 0){
        jLam([x], closed, t)()
      } else {
        jLamExp(env, closed, jLam([x], closed, t))()
      }
    }

    /** @type{(t: () => void, args: Array<() => void>) => () => void} */
    const jAppClosure = (t, args) => () => {
      if (args.length === 0){
        t()
      } else {
        t(); jTuple(args)()
      }
    }

    /** @type{(ts : Map<Name, Tm>) => () => void} */
    const oRec = (ts) => () => {
      const arr = Array.from(ts);
      /** @type{(i:Number) => void} */
      function go(i) {
        if (i == arr.length){
          return;
        } else if (arr.length !== 0 && i == arr.length - 1){
          put('['); strLit(arr[i][0])(); put(', '); nonTail(() => oeval(arr[i][1]))(); put(']');
        } else {
          put('['); strLit(arr[i][0])(); put(', '); nonTail(() => oeval(arr[i][1]))(); put('], ');
          return go(i+1);
        }
      }
      go(0)
    }

    /** @type{(x:Name) => Name} */
    const closeVar = (x) => x + 'c'
    /** @type{(x:Name) => Name} */
    const openVar  = (x) => x + 'o'

    //----------------------------------------------------------------------------------------------------

    /** @type {(t:Tm) => void} */
    function exec(top){
      switch (top.tag){
        case _Var       : return cRun(() => put(top._1))()
        case _Let       : return jLet(top._1, true, () => ceval(top._2), () => exec(top._3))()
        case _Lam       : throw new Error('impossible')
        case _LiftedLam : throw new Error('impossible')
        case _App       : return cRun(cApp(() => ceval(top._1), () => ceval(top._2)))()
        case _Quote     : throw new Error('impossible')
        case _Splice    : return jApp(str('codegen'), [() => ceval(top._1)])()
        case _Return    : return jReturn(() => ceval(top._1))()
        case _Bind      : return jLet(top._1, true, () => exec(top._2), () => exec(top._3))()
        case _Seq       : return jSeq(() => exec(top._1), () => exec(top._2))()
        case _New       : return jReturn(() => {put('{_1 : '); ceval(top._1); put('}')})()
        case _Write     : return nonTail(() => {ceval(top._1); put('._1 = '); ceval(top._2); jReturn(str("{}"))() })()
        case _Read      : return jReturn(() => {ceval(top._1); put('._1')})()
        case _CSP       : return jReturn(() => {put('csp_[' + top._1 + ']()/*'); strLit(top._2); put('*/')})()
        case _Log       : return jApp(str('log_'), [])()
        case _ReadNat   : return jApp(str('readNat_'), [])()
        case _PrintNat  : return jApp(str('printNat_'), [() => ceval(top._1)])()
        case _Rec       : throw new Error('impossible')
        case _Proj      : return cRun(() => {ceval(top._1); put('.'); put(top._2)})()
        case _Suc       : throw new Error('impossible')
        case _NatElim   : return cRun(jApp(str('cNatElim_'),[() => ceval(top._1), () => ceval(top._2), () => ceval(top._3)]))()
        case _NatLit    : throw new Error('impossible')
      }
    }

    /** @type {(t:Tm) => void} */
    function ceval(top){
      switch (top.tag){
        case _Var       : return jReturn(str(top._1))()
        case _Let       : return jLet(top._1, true, () => ceval(top._2), () => ceval(top._3))()
        case _Lam       : throw new Error('impossible')
        case _LiftedLam : return jReturn(() => {
                            put('{_1 : ');
                            jAppClosure(str(closeVar(top._1)), top._3.map(str))();
                            put(', _2 : ');
                            jAppClosure(str(openVar(top._1)), top._3.map(oevalVar))();
                            put('}')
                            })()
        case _App       : return cApp(() => ceval(top._1), () => ceval(top._2))()
        case _Quote     : return inStage(1, () => {return oeval(top._1)})()
        case _Splice    : return jApp(str('codegenClosed_'), [() => ceval(top._1)])()
        case _Return    : return jLam([], true, () => exec(top))()
        case _Bind      : return jLam([], true, () => exec(top))()
        case _Seq       : return jLam([], true, () => exec(top))()
        case _New       : return jLam([], true, () => exec(top))()
        case _Write     : return jLam([], true, () => exec(top))()
        case _Read      : return jLam([], true, () => exec(top))()
        case _CSP       : return jReturn(() => {put('csp_[' + top._1 + ']/*'); put(top._2); put('*/')})()
        case _Log       : return jLam([], true, () => exec(top))()
        case _ReadNat   : return jLam([], true, () => exec(top))()
        case _PrintNat  : return jLam([], true, () => exec(top))()
        case _Rec       : return jReturn(() => {put('{'); cRec(top._1)(); put('}')})()
        case _Proj      : return jReturn(() => {ceval(top._1); put('.'); put(top._2) })()
        case _Suc       : return jApp(str('cSuc_'), [() => ceval(top._1)])()
        case _NatElim   : return jApp(str('cNatElim_'), [() => ceval(top._1), () => ceval(top._2), () => ceval(top._3)])()
        case _NatLit    : return jReturn(str(top._1.toString()))()
      }
    }

    /** @type {(x:Name) => () => void} */
    const oevalVar = (x) => () => {
      if (closedVars.has(x)){
        return jApp(str('CSP_'), [str(x), strLit(x)])();
      } else {
        return jReturn(str(x))();
      }
    }

    /** @type {(t:Tm) => void} */
    function oeval(top){
      switch (top.tag){
        case _Var : return oevalVar(top._1)()
        case _CSP : return jApp(str('CSP_'), [str('csp_[' + top._1 + ']'), strLit(top._2)])()

        case _Let : {
          if (stage === 0){
            return jLet(top._1, false, () => oeval(top._2), () => oeval(top._3))()
          } else {
            return jApp(str('Let_'), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oeval(top._3))])()
          }
        }
        case _Lam : {
          return jApp(str('Lam_'), [strLit(top._1), jLam([top._1], false, () => oeval(top._2))])()
        }
        case _LiftedLam :
          return jApp(str('Lam_'), [strLit(top._2), jAppClosure(str(openVar(top._1)), top._3.map(str))])()
        case _App : {
          if (stage === 0){
            return jApp(str('app_'), [() => oeval(top._1), () => oeval(top._2)])()
          } else {
            return jApp(str('App_'), [() => oeval(top._1), () => oeval(top._2)])()
          }
        }
        case _Quote : return jApp(str('quote_'), [inStage(stage + 1, () => oeval(top._1))])()
        case _Splice : {
          if (stage === 0){
            return jApp(str('codegenOpen_'), [() => oeval(top._1)])()
          } else {
            return inStage(stage - 1, jApp(str('splice_'), [() => oeval(top._1)]))()
          }
        }

        case _Return    : return jApp(str('Return_'), [() => oeval(top._1)])()
        case _Bind      : return jApp(str('Bind_'), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oeval(top._3))])()
        case _Seq       : return jApp(str('Seq_'), [() => oeval(top._1), () => oeval(top._2)])()
        case _New       : return jApp(str('New_'), [() => oeval(top._1)])()
        case _Write     : return jApp(str('Write_'), [() => oeval(top._1), () => oeval(top._2)])()
        case _Read      : return jApp(str('Read_'), [() => oeval(top._1)])()
        case _Log       : return jApp(str('Log_'), [strLit(top._1)])()
        case _ReadNat   : return jApp(str('ReadNat_'), [])()
        case _PrintNat  : return jApp(str('PrintNat_'), [() => oeval(top._1)])()
        case _Rec       : return jApp(str('Rec_'), [ () => {put('new Map(['); oRec(top._1)(); put('])')}])()

        case _Proj : {
          if (stage === 0){
            return jApp(str('proj_'), [() => oeval(top._1), strLit(top._2)])()
          } else {
            return jApp(str('Proj_'), [() => oeval(top._1), strLit(top._2)])()
          }
        }
        case _Suc : {
          if (stage === 0){
            return jApp(str('suc_'), [() => oeval(top._1)])()
          } else {
            return jApp(str('Suc_'), [() => oeval(top._1)])()
          }
        }
        case _NatElim : {
          if (stage === 0){
            return jApp(str('natElim_'), [() => oeval(top._1), () => oeval(top._2), () => oeval(top._3)])()
          } else {
            return jApp(str('NatElim_'), [() => oeval(top._1), () => oeval(top._2), () => oeval(top._3)])()
          }
        }
        case _NatLit : {
          return jApp(str('CSP_'), [str(top._1.toString()), strLit('')])()
        }
      }
    }

    /** @type {(t:Top) => void} */
    function execTop(top){
      tail(() => {
        switch (top.tag){
          case _Let  : return jLet(top._1, true, () => ceval(top._2), () => {execTop(top._3)})()
          case _Bind : return jLet(top._1, true, () => exec(top._2), () => {execTop(top._3)})()
          case _Seq  : return jSeq((() => exec(top._1)), (() => {execTop(top._2)}))()

          case _Closure : {
            const x    = top._1
            const env  = top._2
            const arg  = top._3
            const body = top._4
            const t    = top._5
            return jLet(closeVar(x), true, jClosure(env, arg, true, () => ceval(body)),
                   jLet(openVar(x), true, jClosure(env, arg, false, inStage(0, () => {return oeval(body)})), () =>
                   execTop(t)))()
          }
          case _Body : {
            return exec(top._1)
          }
        }
      })()
    }

    /** @type {(t:Top) => void} */
    function cevalTop(top){
      switch (top.tag){
        case _Let : {
          return jLet(top._1, true, () => ceval(top._2), () => {newl(); cevalTop(top._3)})()
        }
        case _Bind : {
          return jLam([], true, () => execTop(top))()
        }
        case _Seq : {
          return jLam([], true, () => execTop(top))()
        }
        case _Closure : {
          const x    = top._1
          const env  = top._2
          const arg  = top._3
          const body = top._4
          const t    = top._5
          return jLet(closeVar(x), true, jClosure(env, arg, true, () => ceval(body)),
                jLet(openVar(x), true, jClosure(env, arg, false, inStage(0, () => {return oeval(body)})), () =>
                cevalTop(t)))()
        }
        case _Body:
          return ceval(top._1)
      }
    } // cevalTop

    /** @type {(t:Top) => void} */
    function oevalTop(top){
      switch (top.tag){
        case _Let: {
          if (stage === 0){
            return jLet(top._1, false, () => oeval(top._2), () => {newl(); oevalTop(top._3)})()
          } else {
            return jApp(str('Let_'), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oevalTop(top._3))])()
          }
        }
        case _Bind:
          return jApp(str('Bind_'), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oevalTop(top._3))])()
        case _Seq:
          return jApp(str('Seq_'), [() => oeval(top._1), () => oevalTop(top._2)])()
        case _Closure:
          throw new Error('impossible')
        case _Body:
          return oeval(top._1)
      }
    } // oevalTop

    put('() => {\n');
    cevalTop(t);
    put(';\n}');
    return build();

  } // emitCode

  const {_1: t2, _2: cspArray} = closureConvert(t);
  const source = emitCode(t2);
  return evalCodeGenClosed_(source, cspArray, loc)
} // codegenClosed


// CODE GENERATION CALLED FROM OPEN EVALUATION
// ----------------------------------------------------------------------------------------------------

/** @type{(x: String, y: Array<Closed>, loc: undefined|Array<String>) => Open} */
function evalCodeGenOpen_(src_, csp_, loc_) {
  displayCode_(loc_, src_);
  const res_ = eval(src_)();
  return res_;
}

/** @type {(t:Open, loc: undefined|Array<String>) => Open} */
function codegenOpen_(t, loc){

  // CODE QUOTING
  // ----------------------------------------------------------------------------------------------------
  /** @type{(t : Open) => {_1: Tm, _2: Array<Closed>}} */
  function quote(top){

    /** @type {Array<Closed>} */
    const cspArray = new Array();

    /** @type{(x: Name, act : (x:Name) => Tm) => Tm} */
    function bind(x, act){
      const x2 = freshenName_(x);
      boundVarSet_.add(x2);
      const res = act(x2);
      boundVarSet_.delete(x);
      return res;
    }

    /** @type{(t : Open) => Tm} */
    function go(top){
      switch (top.tag){
        case _Var : return TVar_(top.name)
        case _Let : {
          const x = top._1
          const t = top._2
          const u = top._3
          const t2 = go(t)
          return bind(x, (x) => TLet_(x, t2, go(u(Var_(x)))))
        }
        case _Lam    : return bind(top._1, (x) => TLam_(x, go(top._2(Var_(x)))))
        case _App    : return TApp_(go(top._1), go(top._2))
        case _Quote  : return TQuote_(go(top._1))
        case _Splice : return TSplice_(go(top._1))
        case _Bind : {
          const t2 = go(top._2)
          return bind(top._1, (x) => TBind_(x, t2, go(top._3(Var_(x)))))
        }
        case _Return : return TReturn_(go(top._1))
        case _Seq    : return TSeq_(go(top._1), go(top._2))
        case _New    : return TNew_(go(top._1))
        case _Write  : return TWrite_(go(top._1), go(top._2))
        case _Read   : return TRead_(go(top._1))

        case _CSP : {
          if (typeof top._1 === 'number'){  // inline closed numerals into source code
            return TNatLit_(top._1)
          } else {
            const id = cspArray.length
            cspArray.push(top._1)
            return TCSP_(id, top._2)
          }
        }
        case _Suc     : return TSuc_(go(top._1))
        case _NatElim : return TNatElim_(go(top._1), go(top._2), go(top._3))

        case _Rec : {
          const res = new Map()
          top._1.forEach((t, x) => {res.set(x, go(t))})
          return TRec_(res)
        }
        case _Proj     : return TProj_(go(top._1), top._2)
        case _PrintNat : return TPrintNat_(go(top._1))
        case _ReadNat  : return TReadNat_()
        case _Log      : return TLog_(top._1)
      } // switch
    } // go

    return {_1: go(top), _2: cspArray}
  } // quote

  // CODE EMISSION
  //----------------------------------------------------------------------------------------------------

  /** @type{(t:Tm) => String} */
  function emitCode(t){

    // code builder is simply an array of strings
    /** @type {Array<String>} */
    const builder = new Array()

    /** @type {() => String} */
    const build = () => {return builder.join('')}

    /** @type {Set<Name>} */
    const localVars = new Set()

    /** @type {Number} */
    let indentation = 0

    /** @type {Boolean} */
    let isTail = true

    /** @type{Number} */
    let stage = 0

    /** @type {(s:String) => void} */
    const put = (s) => {builder.push(s)}

    /** @type {(s:String) => () => void} */
    const str = (s) => () => put(s)

    /** @type {(s:String) => () => void} */
    const strLit = (s) => () => put("`" + s + "`")

    /** @type {() => void} */
    const newl = () => {put('\n' + ' '.repeat(indentation))}

    /** @type{(s : Number, act: () => void) => (() => void)} */
    const inStage = (s, act) => () => {
      const backup = stage
      stage = s
      const res = act()
      stage = backup
      return res
    }

    /** @type{ (act: () => void) => (() => void) } */
    const tail = (act) => () => {
      const backup = isTail
      isTail = true
      const res = act()
      isTail = backup
      return res
    }

    /** @type{ (act: () => void) => (() => void) } */
    const nonTail = (act) => () => {
      const backup = isTail
      isTail = false
      const res = act()
      isTail = backup
      return res
    }

    /** @type{ (act: () => void) => (() => void) } */
    const indent = (act) => () => {
      const backup = indentation
      indentation += 2
      const res = act()
      indentation = backup
      return res
    }

    /** @type{ () => void } */
    function semi(){put(';')}

    /** @type{ (act: () => void) => (() => void) } */
    const par = (act) => () => {put('('); act(); put(')')}

    /** @type{ (x: Name, act: () => void) => () => void} */
    const bind = (x, act) => () => {
      if (closed) {localVars.add(x)};
      const res = act();
      if (closed) {localVars.delete(x)}
      return res;
    }

    /** @type{ (x: Name, closed: Boolean, t: () => void, u: () => void) => (() => void) } */
    const jLet = (x, closed, t, u) => () => {
      if (isTail){
        put('const ' + x + ' = '); indent(nonTail(t))(); semi(); newl(); tail(bind(x, u))()
      } else {
        put('((' + x + ') => ');
        par(nonTail(bind(x, u)))(); put(')('); nonTail(t)(); put(')')
      }
    }

    /** @type{ (t: () => void, u: () => void) => (() => void) } */
    const jSeq = (t, u) => () => {
      if (isTail){
        indent(nonTail(t))(); semi(); newl(); tail(u)()
      } else {
        put('((_) => '); par(nonTail(u))(); put(')('); nonTail(t)(); put(')')
      }
    }

    /** @type{(xs: Array<() => void>) => (() => void)} */
    const jTuple = (xs) => () => {
      if (xs.length === 0){
        put('()')
      } else {
        put('('); xs[0](); xs.slice(1, xs.length).forEach((act) => {put(', '); act()}); put(')')
      }
    }

    /** @type((t : () => void) => () => void)} */
    const jReturn = (t) => () => {
      if (isTail) { put('return '); nonTail(t)()
      } else      { t() }
    }

    /** @type{(xs : Array<Name>, closed: Boolean, t: () => void) => () => void} */
    const jLam = (xs, closed, t) => () => {
      jReturn(() => {
        jTuple(xs.map(str))();
        put(' => {');
        xs.forEach((x) => localVars.add(x))
        tail(t)();
        xs.forEach((x) => localVars.delete(x))
        put('}')
      })()
    }

    /** @type{(xs: Array<Name>, closed:Boolean, t: () => void) => () => void} */
    const jLamExp = (xs, closed, t) => () => {
      jReturn(() => {
        jTuple(xs.map(str))();
        put(' => ')
        nonTail(t)();
      })()
    }

    /** @type{(t: () => void, u: () => void) => () => void} */
    const cApp = (t, u) => () => {
      jReturn(() => {par(t)(); put('._1'); par(u)()})()
    }

    /** @type{(t : () => void, args: Array<() => void>) => () => void} */
    const jApp = (t, args) => () => {
      jReturn(() => {t(); jTuple(args)() })()
    }

    /** @type{(t : () => void) => () => void} */
    const cRun = (t) => () => {
      jReturn(() => {t(); put('()') })()
    }

    /** @type{(env: Array<Name>, x: Name, closed:Boolean, t : () => void) => () => void} */
    const jClosure = (env, x, closed, t) => () => {
      if (env.length === 0){
        jLam([x], closed, t)()
      } else {
        jLamExp(env, closed, jLam([x], closed, t))()
      }
    }

    /** @type{(t: () => void, args: Array<() => void>) => () => void} */
    const jAppClosure = (t, args) => () => {
      if (args.length === 0){
        t()
      } else {
        t(); jTuple(args)()
      }
    }

    /** @type{(ts : Map<Name, Tm>) => () => void} */
    const oRec = (ts) => () => {
      const arr = Array.from(ts);
      /** @type{(i:Number) => void} */
      function go(i) {
        if (i == arr.length){
          return;
        } else if (arr.length !== 0 && i == arr.length - 1){
          put('['); put(arr[i][0]); put(', '); nonTail(() => oeval(arr[i][1]))(); put(']');
        } else {
          put('['); put(arr[i][0]); put(', '); nonTail(() => oeval(arr[i][1]))(); put('], ');
          return go(i+1);
        }
      }
      go(0)
    }

    /** @type{(x:Name) => Name} */
    const openVar  = (x) => x + 'o'

    //----------------------------------------------------------------------------------------------------

    // Local variables are those bound within the currently generate code
    // Non-local vars are the bound vars floating around, bound upstream.

    /** @type {(x:Name) => () => void} */
    const oevalVar = (x) => () => {
      if (localVars.has(x)) {
        return jReturn(str(x))();
      } else {
        return jApp(str('Var_'), [strLit(x)])();
      }
    }

    /** @type {(t:Tm) => void} */
    function oeval(top){
      switch (top.tag){
        case _Var : return oevalVar(top._1)();
        case _CSP : return jApp(str('CSP_'), [str('csp_[' + top._1 + ']'), strLit(top._2)])()

        case _Let : {
          if (stage === 0){
            return jLet(top._1, false, () => oeval(top._2), () => oeval(top._3))()
          } else {
            return jApp(str('Let_'), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oeval(top._3))])()
          }
        }
        case _Lam : {
          return jApp(str('Lam_'), [strLit(top._1), jLam([top._1], false, () => oeval(top._2))])()
        }
        case _LiftedLam :
          throw new Error('impossible')
        case _App : {
          if (stage === 0){
            return jApp(str('app_'), [() => oeval(top._1), () => oeval(top._2)])()
          } else {
            return jApp(str('App_'), [() => oeval(top._1), () => oeval(top._2)])()
          }
        }
        case _Quote : return jApp(str('quote_'), [inStage(stage + 1, () => oeval(top._1))])()
        case _Splice : {
          if (stage === 0){
            return jApp(str('codegenOpen_'), [() => oeval(top._1)])()
          } else {
            return inStage(stage - 1, jApp(str('splice_'), [() => oeval(top._1)]))()
          }
        }

        case _Return    : return jApp(str('Return_'), [() => oeval(top._1)])()
        case _Bind      : return jApp(str('Bind_'), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oeval(top._3))])()
        case _Seq       : return jApp(str('Seq_'), [() => oeval(top._1), () => oeval(top._2)])()
        case _New       : return jApp(str('New_'), [() => oeval(top._1)])()
        case _Write     : return jApp(str('Write_'), [() => oeval(top._1), () => oeval(top._2)])()
        case _Read      : return jApp(str('Read_'), [() => oeval(top._1)])()
        case _Log       : return jApp(str('Log_'), [strLit(top._1)])()
        case _ReadNat   : return jApp(str('ReadNat_'), [])()
        case _PrintNat  : return jApp(str('PrintNat_'), [() => oeval(top._1)])()
        case _Rec       : return jApp(str('Rec_'), [ () => {put('new Map(['); oRec(top._1)(); put('])')}])()

        case _Proj : {
          if (stage === 0){
            return jApp(str('proj_'), [() => oeval(top._1), strLit(top._2)])()
          } else {
            return jApp(str('Proj_'), [() => oeval(top._1), strLit(top._2)])()
          }
        }
        case _Suc : {
          if (stage === 0){
            return jApp(str('suc_'), [() => oeval(top._1)])()
          } else {
            return jApp(str('Suc_'), [() => oeval(top._1)])()
          }
        }
        case _NatElim : {
          if (stage === 0){
            return jApp(str('natElim_'), [() => oeval(top._1), () => oeval(top._2), () => oeval(top._3)])()
          } else {
            return jApp(str('NatElim_'), [() => oeval(top._1), () => oeval(top._2), () => oeval(top._3)])()
          }
        }
        case _NatLit : {
          const s = top._1.toString();
          return jApp(str('CSP_'), [str(s), strLit(s)])()
        }
      } // switch
    } // oeval

    put('() => {\n');
    oeval(t);
    put(';\n}');
    return build();

  } // emitCode

  switch (t.tag){
    case _CSP : {
      const res = codegenClosed_(t._1, loc);
      return CSP_(res, '');
    }
    case _Quote : {
      const {_1: t2, _2: cspArray} = quote(t._1);
      const source = emitCode(t2);
      return evalCodeGenOpen_(source, cspArray, loc);
    }
    default : {
      return Splice_(t)
    }
  }
} // codegenOpen_


// CLOSED COMPUTATIONS
//----------------------------------------------------------------------------------------------------

/** @type{(t:Closed) => Closed} */
function cSuc_(t){
  if (typeof t === 'number'){
    return t + 1
  } else {
    throw new Error('impossible')
  }
}

/** @type{(s : Closed, z: Closed, n:Closed) => Closed} */
function cNatElim_(s, z, n) {
  if (n === 0) {
    return z;
  } else if (n > 0) {
    const m = n - 1;
    return s._1(m)._1(cNatElim_(s, z, m))
  } else {
    throw new Error('impossible')
  }
}

// OPEN COMPUTATIONS
//----------------------------------------------------------------------------------------------------

/** @type {(t:Open, u:Open) => Open} */
function app_(t, u) {
    if (t.tag === _CSP) {
        // t must be a closed closure
        const v1 = /** @type{{_1: (v:Closed) => Closed, _2: (v:Open) => Open}} */ (t._1)
        if (u.tag === _CSP) {
            return CSP_(v1._1(u._1), '')
        } else {
            return v1._2(u)
        }
    } else if (t.tag === _Lam) {
        return t._2(u)
    } else {
        return App_(t, u)
    }
}

// Splice in stage 1+, no code generation
/** @type {(t:Open) => Open} */
function splice_(t) {
  if (t.tag == _CSP){
    // t must be a quoted open value
    return /** @type{Open} */ (t._1)
  } else if (t.tag === _Quote) {
    return t._1
  } else {
    return Splice_(t)
  }
}

// Quote in open evaluation
/** @type{(t:Open) => Open} */
function quote_(t){
  if (t.tag === _Splice){
    return t._1
  } else {
    return Quote_(t)
  }
}

/** @type{(t:Open, x:Name) => Open} */
function proj_(t, x){
  if (t.tag === _CSP){
    // must be a record
    return CSP_((t._1)[x], '');
  } else if (t.tag === _Rec) {
    return /** @type{Open} */ (t._1.get(x))
  } else {
    return Proj_(t, x);
  }
}

/** @type{(s : Open, z:Open, n:Open) => Open} */
function natElim_(s, z, n){
  /** @type{(n : Number) => Open} */
  function go(n){
    if (n === 0){
      return z
    } else if (n > 0) {
      const m = n - 1
      return app_(app_(s, CSP_(m, '')), go(m))
    } else {
      throw new Error('impossible')
    }
  }
  if (n.tag === _CSP){
    return go(n._1)
  } else if (n.tag == _Suc){
    return app_(app_(s, n._1), natElim_(s, z, n._1))
  } else {
    return NatElim_(s, z, n);
  }
}

/** @type{(t:Open) => Open} */
function suc_(t){
  if (t.tag === _CSP) {
    return CSP_(t._1 + 1, '')
  } else {
    return Suc_(t)
  }
}

// IO
//----------------------------------------------------------------------------------------------------

function log_(s){
  console.log(s)
  return {}
}

function readNat_(){
  const str = reader_.prompt()
  const num = parseFloat(str)
  const n   = Math.round(num)
  if (!(num === n)){
    throw new Error('Non-integral number')
  } else if (num < 0) {
    throw new Error('negative number')
  } else {
    return n
  }
}

/** @type{(t:Closed) => {}} */
function printNat_(n){
  console.log(n)
  return {}
}


// BEGIN CODE
// ----------------------------------------------------------------------------------------------------


const $iter0_c = ($f, $n4) => ($a5) => {return (($f)._1($n4))._1($a5)};
const $iter0_o = ($f, $n4) => ($a5) => {return app_(app_($f, $n4), $a5)};
const $iter1_c = ($f) => ($n4) => {return { _1 : $iter0_c($f, $n4), _2 : $iter0_o(CSP_($f, `$f`), CSP_($n4, `$n4`))}};
const $iter1_o = ($f) => ($n4) => {return Lam_(`$a5`, $iter0_o($f, $n4))};
const $iter2_c = ($f, $n) => ($a) => {return cNatElim_({ _1 : $iter1_c($f), _2 : $iter1_o(CSP_($f, `$f`))}, $a, $n)};
const $iter2_o = ($f, $n) => ($a) => {return natElim_(Lam_(`$n4`, $iter1_o($f)), $a, $n)};
const $iter3_c = ($n) => ($f) => {return { _1 : $iter2_c($f, $n), _2 : $iter2_o(CSP_($f, `$f`), CSP_($n, `$n`))}};
const $iter3_o = ($n) => ($f) => {return Lam_(`$a`, $iter2_o($f, $n))};
const $iter4_c = ($n) => {return { _1 : $iter3_c($n), _2 : $iter3_o(CSP_($n, `$n`))}};
const $iter4_o = ($n) => {return Lam_(`$f`, $iter3_o($n))};
const $iter5_c = ($A) => {return { _1 : $iter4_c, _2 : $iter4_o}};
const $iter5_o = ($A) => {return Lam_(`$n`, $iter4_o)};
const $iter = { _1 : $iter5_c, _2 : $iter5_o};

const $add0_c = ($n4) => {return cSuc_($n4)};
const $add0_o = ($n4) => {return suc_($n4)};
const $add1_c = ($_) => {return { _1 : $add0_c, _2 : $add0_o}};
const $add1_o = ($_) => {return Lam_(`$n4`, $add0_o)};
const $add2_c = ($n) => ($m) => {return (((($iter)._1(undefined))._1($n))._1({ _1 : $add1_c, _2 : $add1_o}))._1($m)};
const $add2_o = ($n) => ($m) => {return app_(app_(app_(app_(CSP_($iter, `$iter`), CSP_undefined_), $n), Lam_(`$_`, $add1_o)), $m)};
const $add3_c = ($n) => {return { _1 : $add2_c($n), _2 : $add2_o(CSP_($n, `$n`))}};
const $add3_o = ($n) => {return Lam_(`$m`, $add2_o($n))};
const $add = { _1 : $add3_c, _2 : $add3_o};

const $mul0_c = ($m) => ($y) => {return (($add)._1($y))._1($m)};
const $mul0_o = ($m) => ($y) => {return app_(app_(CSP_($add, `$add`), $y), $m)};
const $mul1_c = ($m) => ($_) => {return { _1 : $mul0_c($m), _2 : $mul0_o(CSP_($m, `$m`))}};
const $mul1_o = ($m) => ($_) => {return Lam_(`$y`, $mul0_o($m))};
const $mul2_c = ($n) => ($m) => {return (((($iter)._1(undefined))._1($n))._1({ _1 : $mul1_c($m), _2 : $mul1_o(CSP_($m, `$m`))}))._1(0)};
const $mul2_o = ($n) => ($m) => {return app_(app_(app_(app_(CSP_($iter, `$iter`), CSP_undefined_), $n), Lam_(`$_`, $mul1_o($m))), CSP_(0, `0`))};
const $mul3_c = ($n) => {return { _1 : $mul2_c($n), _2 : $mul2_o(CSP_($n, `$n`))}};
const $mul3_o = ($n) => {return Lam_(`$m`, $mul2_o($n))};
const $mul = { _1 : $mul3_c, _2 : $mul3_o};

const $Pair0_c = ($B) => {return undefined};
const $Pair0_o = ($B) => {return CSP_undefined_};
const $Pair1_c = ($A) => {return { _1 : $Pair0_c, _2 : $Pair0_o}};
const $Pair1_o = ($A) => {return Lam_(`$B`, $Pair0_o)};
const $Pair = { _1 : $Pair1_c, _2 : $Pair1_o};

const $id0_c = ($x) => {return $x};
const $id0_o = ($x) => {return $x};
const $id1_c = ($A) => {return { _1 : $id0_c, _2 : $id0_o}};
const $id1_o = ($A) => {return Lam_(`$x`, $id0_o)};
const $id = { _1 : $id1_c, _2 : $id1_o};

const $const0_c = ($x) => ($y) => {return $x};
const $const0_o = ($x) => ($y) => {return $x};
const $const1_c = ($x) => {return { _1 : $const0_c($x), _2 : $const0_o(CSP_($x, `$x`))}};
const $const1_o = ($x) => {return Lam_(`$y`, $const0_o($x))};
const $const2_c = ($B) => {return { _1 : $const1_c, _2 : $const1_o}};
const $const2_o = ($B) => {return Lam_(`$x`, $const1_o)};
const $const3_c = ($A) => {return { _1 : $const2_c, _2 : $const2_o}};
const $const3_o = ($A) => {return Lam_(`$B`, $const2_o)};
const $const = { _1 : $const3_c, _2 : $const3_o};

const $Monad0_c = ($M) => {return undefined};
const $Monad0_o = ($M) => {return CSP_undefined_};
const $Monad = { _1 : $Monad0_c, _2 : $Monad0_o};

const $Identity0_c = ($A) => {return $A};
const $Identity0_o = ($A) => {return $A};
const $Identity = { _1 : $Identity0_c, _2 : $Identity0_o};

const $MIdentity0_c = ($a) => {return $a};
const $MIdentity0_o = ($a) => {return $a};
const $MIdentity1_c = ($A) => {return { _1 : $MIdentity0_c, _2 : $MIdentity0_o}};
const $MIdentity1_o = ($A) => {return Lam_(`$a`, $MIdentity0_o)};
const $MIdentity2_c = ($a) => ($f) => {return ($f)._1($a)};
const $MIdentity2_o = ($a) => ($f) => {return app_($f, $a)};
const $MIdentity3_c = ($a) => {return { _1 : $MIdentity2_c($a), _2 : $MIdentity2_o(CSP_($a, `$a`))}};
const $MIdentity3_o = ($a) => {return Lam_(`$f`, $MIdentity2_o($a))};
const $MIdentity4_c = ($B) => {return { _1 : $MIdentity3_c, _2 : $MIdentity3_o}};
const $MIdentity4_o = ($B) => {return Lam_(`$a`, $MIdentity3_o)};
const $MIdentity5_c = ($A) => {return { _1 : $MIdentity4_c, _2 : $MIdentity4_o}};
const $MIdentity5_o = ($A) => {return Lam_(`$B`, $MIdentity4_o)};
const $MIdentity = {ret: { _1 : $MIdentity1_c, _2 : $MIdentity1_o}, bind: { _1 : $MIdentity5_c, _2 : $MIdentity5_o}};

const $Gen0_c = ($A) => {return undefined};
const $Gen0_o = ($A) => {return CSP_undefined_};
const $Gen = { _1 : $Gen0_c, _2 : $Gen0_o};

const $MonadGen0_c = ($M) => {return undefined};
const $MonadGen0_o = ($M) => {return CSP_undefined_};
const $MonadGen = { _1 : $MonadGen0_c, _2 : $MonadGen0_o};

const $runGen0_c = ($x) => {return $x};
const $runGen0_o = ($x) => {return $x};
const $runGen1_c = ($A) => ($ga) => {return (($ga)._1($A))._1({ _1 : $runGen0_c, _2 : $runGen0_o})};
const $runGen1_o = ($A) => ($ga) => {return app_(app_($ga, $A), Lam_(`$x`, $runGen0_o))};
const $runGen2_c = ($A) => {return { _1 : $runGen1_c($A), _2 : $runGen1_o(CSP_($A, `$A`))}};
const $runGen2_o = ($A) => {return Lam_(`$ga`, $runGen1_o($A))};
const $runGen = { _1 : $runGen2_c, _2 : $runGen2_o};

const $MGen0_c = ($a) => ($k) => {return ($k)._1($a)};
const $MGen0_o = ($a) => ($k) => {return app_($k, $a)};
const $MGen1_c = ($a) => ($R) => {return { _1 : $MGen0_c($a), _2 : $MGen0_o(CSP_($a, `$a`))}};
const $MGen1_o = ($a) => ($R) => {return Lam_(`$k`, $MGen0_o($a))};
const $MGen2_c = ($a) => {return { _1 : $MGen1_c($a), _2 : $MGen1_o(CSP_($a, `$a`))}};
const $MGen2_o = ($a) => {return Lam_(`$R`, $MGen1_o($a))};
const $MGen3_c = ($A) => {return { _1 : $MGen2_c, _2 : $MGen2_o}};
const $MGen3_o = ($A) => {return Lam_(`$a`, $MGen2_o)};
const $MGen4_c = ($R, $f, $k) => ($a) => {return ((($f)._1($a))._1($R))._1($k)};
const $MGen4_o = ($R, $f, $k) => ($a) => {return app_(app_(app_($f, $a), $R), $k)};
const $MGen5_c = ($R, $f, $ga) => ($k) => {return (($ga)._1($R))._1({ _1 : $MGen4_c($R, $f, $k), _2 : $MGen4_o(CSP_($R, `$R`), CSP_($f, `$f`), CSP_($k, `$k`))})};
const $MGen5_o = ($R, $f, $ga) => ($k) => {return app_(app_($ga, $R), Lam_(`$a`, $MGen4_o($R, $f, $k)))};
const $MGen6_c = ($f, $ga) => ($R) => {return { _1 : $MGen5_c($R, $f, $ga), _2 : $MGen5_o(CSP_($R, `$R`), CSP_($f, `$f`), CSP_($ga, `$ga`))}};
const $MGen6_o = ($f, $ga) => ($R) => {return Lam_(`$k`, $MGen5_o($R, $f, $ga))};
const $MGen7_c = ($ga) => ($f) => {return { _1 : $MGen6_c($f, $ga), _2 : $MGen6_o(CSP_($f, `$f`), CSP_($ga, `$ga`))}};
const $MGen7_o = ($ga) => ($f) => {return Lam_(`$R`, $MGen6_o($f, $ga))};
const $MGen8_c = ($ga) => {return { _1 : $MGen7_c($ga), _2 : $MGen7_o(CSP_($ga, `$ga`))}};
const $MGen8_o = ($ga) => {return Lam_(`$f`, $MGen7_o($ga))};
const $MGen9_c = ($B) => {return { _1 : $MGen8_c, _2 : $MGen8_o}};
const $MGen9_o = ($B) => {return Lam_(`$ga`, $MGen8_o)};
const $MGen10_c = ($A) => {return { _1 : $MGen9_c, _2 : $MGen9_o}};
const $MGen10_o = ($A) => {return Lam_(`$B`, $MGen9_o)};
const $MGen = {ret: { _1 : $MGen3_c, _2 : $MGen3_o}, bind: { _1 : $MGen10_c, _2 : $MGen10_o}};

const $MGenGen0_c = ($a) => ($k) => {return Let_(`$x`, splice_(CSP_($a, `$a`)), ($x) => {return splice_(app_(CSP_($k, `$k`), quote_($x)))})};
const $MGenGen0_o = ($a) => ($k) => {return quote_(Let_(`$x`, splice_($a), ($x) => {return splice_(app_($k, quote_($x)))}))};
const $MGenGen1_c = ($a) => ($R) => {return { _1 : $MGenGen0_c($a), _2 : $MGenGen0_o(CSP_($a, `$a`))}};
const $MGenGen1_o = ($a) => ($R) => {return Lam_(`$k`, $MGenGen0_o($a))};
const $MGenGen2_c = ($a) => {return { _1 : $MGenGen1_c($a), _2 : $MGenGen1_o(CSP_($a, `$a`))}};
const $MGenGen2_o = ($a) => {return Lam_(`$R`, $MGenGen1_o($a))};
const $MGenGen3_c = ($A) => {return { _1 : $MGenGen2_c, _2 : $MGenGen2_o}};
const $MGenGen3_o = ($A) => {return Lam_(`$a`, $MGenGen2_o)};
const $MGenGen4_c = ($ma) => {return $ma};
const $MGenGen4_o = ($ma) => {return $ma};
const $MGenGen5_c = ($A) => {return { _1 : $MGenGen4_c, _2 : $MGenGen4_o}};
const $MGenGen5_o = ($A) => {return Lam_(`$ma`, $MGenGen4_o)};
const $MGenGen = {gen: { _1 : $MGenGen3_c, _2 : $MGenGen3_o}, liftGen: { _1 : $MGenGen5_c, _2 : $MGenGen5_o}};

const $StateT0_c = ($A) => {return undefined};
const $StateT0_o = ($A) => {return CSP_undefined_};
const $StateT1_c = ($M) => {return { _1 : $StateT0_c, _2 : $StateT0_o}};
const $StateT1_o = ($M) => {return Lam_(`$A`, $StateT0_o)};
const $StateT2_c = ($S) => {return { _1 : $StateT1_c, _2 : $StateT1_o}};
const $StateT2_o = ($S) => {return Lam_(`$M`, $StateT1_o)};
const $StateT = { _1 : $StateT2_c, _2 : $StateT2_o};

const $MStateT0_c = ($MM, $a) => ($s) => {return (($MM.ret)._1(undefined))._1({fst: $a, snd: $s})};
const $MStateT0_o = ($MM, $a) => ($s) => {return app_(app_(proj_($MM, `ret`), CSP_undefined_), Rec_(new Map([[`fst`, $a], [`snd`, $s]])))};
const $MStateT1_c = ($MM) => ($a) => {return { _1 : $MStateT0_c($MM, $a), _2 : $MStateT0_o(CSP_($MM, `$MM`), CSP_($a, `$a`))}};
const $MStateT1_o = ($MM) => ($a) => {return Lam_(`$s`, $MStateT0_o($MM, $a))};
const $MStateT2_c = ($MM) => ($A) => {return { _1 : $MStateT1_c($MM), _2 : $MStateT1_o(CSP_($MM, `$MM`))}};
const $MStateT2_o = ($MM) => ($A) => {return Lam_(`$a`, $MStateT1_o($MM))};
const $MStateT3_c = ($f) => ($as) => {return (($f)._1($as.fst))._1($as.snd)};
const $MStateT3_o = ($f) => ($as) => {return app_(app_($f, proj_($as, `fst`)), proj_($as, `snd`))};
const $MStateT4_c = ($MM, $f, $ma) => ($s) => {return (((($MM.bind)._1(undefined))._1(undefined))._1(($ma)._1($s)))._1({ _1 : $MStateT3_c($f), _2 : $MStateT3_o(CSP_($f, `$f`))})};
const $MStateT4_o = ($MM, $f, $ma) => ($s) => {return app_(app_(app_(app_(proj_($MM, `bind`), CSP_undefined_), CSP_undefined_), app_($ma, $s)), Lam_(`$as`, $MStateT3_o($f)))};
const $MStateT5_c = ($MM, $ma) => ($f) => {return { _1 : $MStateT4_c($MM, $f, $ma), _2 : $MStateT4_o(CSP_($MM, `$MM`), CSP_($f, `$f`), CSP_($ma, `$ma`))}};
const $MStateT5_o = ($MM, $ma) => ($f) => {return Lam_(`$s`, $MStateT4_o($MM, $f, $ma))};
const $MStateT6_c = ($MM) => ($ma) => {return { _1 : $MStateT5_c($MM, $ma), _2 : $MStateT5_o(CSP_($MM, `$MM`), CSP_($ma, `$ma`))}};
const $MStateT6_o = ($MM) => ($ma) => {return Lam_(`$f`, $MStateT5_o($MM, $ma))};
const $MStateT7_c = ($MM) => ($B) => {return { _1 : $MStateT6_c($MM), _2 : $MStateT6_o(CSP_($MM, `$MM`))}};
const $MStateT7_o = ($MM) => ($B) => {return Lam_(`$ma`, $MStateT6_o($MM))};
const $MStateT8_c = ($MM) => ($A) => {return { _1 : $MStateT7_c($MM), _2 : $MStateT7_o(CSP_($MM, `$MM`))}};
const $MStateT8_o = ($MM) => ($A) => {return Lam_(`$B`, $MStateT7_o($MM))};
const $MStateT9_c = ($MM) => {return {ret: { _1 : $MStateT2_c($MM), _2 : $MStateT2_o(CSP_($MM, `$MM`))}, bind: { _1 : $MStateT8_c($MM), _2 : $MStateT8_o(CSP_($MM, `$MM`))}}};
const $MStateT9_o = ($MM) => {return Rec_(new Map([[`ret`, Lam_(`$A`, $MStateT2_o($MM))], [`bind`, Lam_(`$A`, $MStateT8_o($MM))]]))};
const $MStateT10_c = ($M) => {return { _1 : $MStateT9_c, _2 : $MStateT9_o}};
const $MStateT10_o = ($M) => {return Lam_(`$MM`, $MStateT9_o)};
const $MStateT11_c = ($S) => {return { _1 : $MStateT10_c, _2 : $MStateT10_o}};
const $MStateT11_o = ($S) => {return Lam_(`$M`, $MStateT10_o)};
const $MStateT = { _1 : $MStateT11_c, _2 : $MStateT11_o};

const $MonadState0_c = ($M) => {return undefined};
const $MonadState0_o = ($M) => {return CSP_undefined_};
const $MonadState1_c = ($S) => {return { _1 : $MonadState0_c, _2 : $MonadState0_o}};
const $MonadState1_o = ($S) => {return Lam_(`$M`, $MonadState0_o)};
const $MonadState = { _1 : $MonadState1_c, _2 : $MonadState1_o};

const $MStateStateT0_c = ($MM) => ($s) => {return (($MM.ret)._1(undefined))._1({fst: $s, snd: $s})};
const $MStateStateT0_o = ($MM) => ($s) => {return app_(app_(proj_($MM, `ret`), CSP_undefined_), Rec_(new Map([[`fst`, $s], [`snd`, $s]])))};
const $MStateStateT1_c = ($MM, $s) => ($_) => {return (($MM.ret)._1(undefined))._1({fst: {}, snd: $s})};
const $MStateStateT1_o = ($MM, $s) => ($_) => {return app_(app_(proj_($MM, `ret`), CSP_undefined_), Rec_(new Map([[`fst`, Rec_(new Map([]))], [`snd`, $s]])))};
const $MStateStateT2_c = ($MM) => ($s) => {return { _1 : $MStateStateT1_c($MM, $s), _2 : $MStateStateT1_o(CSP_($MM, `$MM`), CSP_($s, `$s`))}};
const $MStateStateT2_o = ($MM) => ($s) => {return Lam_(`$_`, $MStateStateT1_o($MM, $s))};
const $MStateStateT3_c = ($MM) => {return {get: { _1 : $MStateStateT0_c($MM), _2 : $MStateStateT0_o(CSP_($MM, `$MM`))}, put: { _1 : $MStateStateT2_c($MM), _2 : $MStateStateT2_o(CSP_($MM, `$MM`))}}};
const $MStateStateT3_o = ($MM) => {return Rec_(new Map([[`get`, Lam_(`$s`, $MStateStateT0_o($MM))], [`put`, Lam_(`$s`, $MStateStateT2_o($MM))]]))};
const $MStateStateT4_c = ($M) => {return { _1 : $MStateStateT3_c, _2 : $MStateStateT3_o}};
const $MStateStateT4_o = ($M) => {return Lam_(`$MM`, $MStateStateT3_o)};
const $MStateStateT5_c = ($S) => {return { _1 : $MStateStateT4_c, _2 : $MStateStateT4_o}};
const $MStateStateT5_o = ($S) => {return Lam_(`$M`, $MStateStateT4_o)};
const $MStateStateT = { _1 : $MStateStateT5_c, _2 : $MStateStateT5_o};

const $MGenStateT0_c = ($MM, $s) => ($a25) => {return (($MM.ret)._1(undefined))._1({fst: $a25, snd: $s})};
const $MGenStateT0_o = ($MM, $s) => ($a25) => {return app_(app_(proj_($MM, `ret`), CSP_undefined_), Rec_(new Map([[`fst`, $a25], [`snd`, $s]])))};
const $MGenStateT1_c = ($A, $MGM, $MM, $a) => ($s) => {return (((($MM.bind)._1(undefined))._1(undefined))._1((($MGM.gen)._1($A))._1($a)))._1({ _1 : $MGenStateT0_c($MM, $s), _2 : $MGenStateT0_o(CSP_($MM, `$MM`), CSP_($s, `$s`))})};
const $MGenStateT1_o = ($A, $MGM, $MM, $a) => ($s) => {return app_(app_(app_(app_(proj_($MM, `bind`), CSP_undefined_), CSP_undefined_), app_(app_(proj_($MGM, `gen`), $A), $a)), Lam_(`$a25`, $MGenStateT0_o($MM, $s)))};
const $MGenStateT2_c = ($A, $MGM, $MM) => ($a) => {return { _1 : $MGenStateT1_c($A, $MGM, $MM, $a), _2 : $MGenStateT1_o(CSP_($A, `$A`), CSP_($MGM, `$MGM`), CSP_($MM, `$MM`), CSP_($a, `$a`))}};
const $MGenStateT2_o = ($A, $MGM, $MM) => ($a) => {return Lam_(`$s`, $MGenStateT1_o($A, $MGM, $MM, $a))};
const $MGenStateT3_c = ($MGM, $MM) => ($A) => {return { _1 : $MGenStateT2_c($A, $MGM, $MM), _2 : $MGenStateT2_o(CSP_($A, `$A`), CSP_($MGM, `$MGM`), CSP_($MM, `$MM`))}};
const $MGenStateT3_o = ($MGM, $MM) => ($A) => {return Lam_(`$a`, $MGenStateT2_o($A, $MGM, $MM))};
const $MGenStateT4_c = ($MM, $s) => ($a) => {return (($MM.ret)._1(undefined))._1({fst: $a, snd: $s})};
const $MGenStateT4_o = ($MM, $s) => ($a) => {return app_(app_(proj_($MM, `ret`), CSP_undefined_), Rec_(new Map([[`fst`, $a], [`snd`, $s]])))};
const $MGenStateT5_c = ($A, $MGM, $MM, $ga) => ($s) => {return (((($MM.bind)._1($A))._1(undefined))._1((($MGM.liftGen)._1($A))._1($ga)))._1({ _1 : $MGenStateT4_c($MM, $s), _2 : $MGenStateT4_o(CSP_($MM, `$MM`), CSP_($s, `$s`))})};
const $MGenStateT5_o = ($A, $MGM, $MM, $ga) => ($s) => {return app_(app_(app_(app_(proj_($MM, `bind`), $A), CSP_undefined_), app_(app_(proj_($MGM, `liftGen`), $A), $ga)), Lam_(`$a`, $MGenStateT4_o($MM, $s)))};
const $MGenStateT6_c = ($A, $MGM, $MM) => ($ga) => {return { _1 : $MGenStateT5_c($A, $MGM, $MM, $ga), _2 : $MGenStateT5_o(CSP_($A, `$A`), CSP_($MGM, `$MGM`), CSP_($MM, `$MM`), CSP_($ga, `$ga`))}};
const $MGenStateT6_o = ($A, $MGM, $MM) => ($ga) => {return Lam_(`$s`, $MGenStateT5_o($A, $MGM, $MM, $ga))};
const $MGenStateT7_c = ($MGM, $MM) => ($A) => {return { _1 : $MGenStateT6_c($A, $MGM, $MM), _2 : $MGenStateT6_o(CSP_($A, `$A`), CSP_($MGM, `$MGM`), CSP_($MM, `$MM`))}};
const $MGenStateT7_o = ($MGM, $MM) => ($A) => {return Lam_(`$ga`, $MGenStateT6_o($A, $MGM, $MM))};
const $MGenStateT8_c = ($MM) => ($MGM) => {return {gen: { _1 : $MGenStateT3_c($MGM, $MM), _2 : $MGenStateT3_o(CSP_($MGM, `$MGM`), CSP_($MM, `$MM`))}, liftGen: { _1 : $MGenStateT7_c($MGM, $MM), _2 : $MGenStateT7_o(CSP_($MGM, `$MGM`), CSP_($MM, `$MM`))}}};
const $MGenStateT8_o = ($MM) => ($MGM) => {return Rec_(new Map([[`gen`, Lam_(`$A`, $MGenStateT3_o($MGM, $MM))], [`liftGen`, Lam_(`$A`, $MGenStateT7_o($MGM, $MM))]]))};
const $MGenStateT9_c = ($MM) => {return { _1 : $MGenStateT8_c($MM), _2 : $MGenStateT8_o(CSP_($MM, `$MM`))}};
const $MGenStateT9_o = ($MM) => {return Lam_(`$MGM`, $MGenStateT8_o($MM))};
const $MGenStateT10_c = ($M) => {return { _1 : $MGenStateT9_c, _2 : $MGenStateT9_o}};
const $MGenStateT10_o = ($M) => {return Lam_(`$MM`, $MGenStateT9_o)};
const $MGenStateT11_c = ($S) => {return { _1 : $MGenStateT10_c, _2 : $MGenStateT10_o}};
const $MGenStateT11_o = ($S) => {return Lam_(`$M`, $MGenStateT10_o)};
const $MGenStateT = { _1 : $MGenStateT11_c, _2 : $MGenStateT11_o};

const $testMonads0_c = ($f, $put) => ($s) => {return ($put)._1(($f)._1($s))};
const $testMonads0_o = ($f, $put) => ($s) => {return app_($put, app_($f, $s))};
const $testMonads1_c = ($MM, $MS, $S) => ($f) => {const $tmp24 = $MM;
    const $ret = $tmp24.ret;
    const $bind = $tmp24.bind;
    const $tmp26 = $MS;
    const $get = $tmp26.get;
    const $put = $tmp26.put;
    return (((($bind)._1($S))._1(undefined))._1($get))._1({ _1 : $testMonads0_c($f, $put), _2 : $testMonads0_o(CSP_($f, `$f`), CSP_($put, `$put`))})};
const $testMonads1_o = ($MM, $MS, $S) => ($f) => {const $tmp24 = $MM;
    const $ret = proj_($tmp24, `ret`);
    const $bind = proj_($tmp24, `bind`);
    const $tmp26 = $MS;
    const $get = proj_($tmp26, `get`);
    const $put = proj_($tmp26, `put`);
    return app_(app_(app_(app_($bind, $S), CSP_undefined_), $get), Lam_(`$s`, $testMonads0_o($f, $put)))};
const $testMonads2_c = ($MM, $S) => ($MS) => {return { _1 : $testMonads1_c($MM, $MS, $S), _2 : $testMonads1_o(CSP_($MM, `$MM`), CSP_($MS, `$MS`), CSP_($S, `$S`))}};
const $testMonads2_o = ($MM, $S) => ($MS) => {return Lam_(`$f`, $testMonads1_o($MM, $MS, $S))};
const $testMonads3_c = ($S) => ($MM) => {return { _1 : $testMonads2_c($MM, $S), _2 : $testMonads2_o(CSP_($MM, `$MM`), CSP_($S, `$S`))}};
const $testMonads3_o = ($S) => ($MM) => {return Lam_(`$MS`, $testMonads2_o($MM, $S))};
const $testMonads4_c = ($S) => {return { _1 : $testMonads3_c($S), _2 : $testMonads3_o(CSP_($S, `$S`))}};
const $testMonads4_o = ($S) => {return Lam_(`$MM`, $testMonads3_o($S))};
const $testMonads5_c = ($M) => {return { _1 : $testMonads4_c, _2 : $testMonads4_o}};
const $testMonads5_o = ($M) => {return Lam_(`$S`, $testMonads4_o)};
const $testMonads6_c = ($x0) => {return undefined};
const $testMonads6_o = ($x0) => {return CSP_undefined_};
const $testMonads7_c = ($x0) => {return undefined};
const $testMonads7_o = ($x0) => {return CSP_undefined_};
const $testMonads8_c = ($x0) => {return undefined};
const $testMonads8_o = ($x0) => {return CSP_undefined_};
const $testMonads9_c = ($ret) => ($_) => {return (($ret)._1(undefined))._1(Rec_(new Map([])))};
const $testMonads9_o = ($ret) => ($_) => {return app_(app_($ret, CSP_undefined_), quote_(Rec_(new Map([]))))};
const $testMonads10_c = ($bind, $put, $ret) => ($n35) => {return (((($bind)._1(undefined))._1(undefined))._1(($put)._1($n35)))._1({ _1 : $testMonads9_c($ret), _2 : $testMonads9_o(CSP_($ret, `$ret`))})};
const $testMonads10_o = ($bind, $put, $ret) => ($n35) => {return app_(app_(app_(app_($bind, CSP_undefined_), CSP_undefined_), app_($put, $n35)), Lam_(`$_`, $testMonads9_o($ret)))};
const $testMonads11_c = ($bind, $gen, $put, $ret) => ($n34) => {return (((($bind)._1(undefined))._1(undefined))._1((($gen)._1(undefined))._1(App_(App_(CSP_($mul, `$mul`), splice_(CSP_($n34, `$n34`))), CSP_(100, `100`)))))._1({ _1 : $testMonads10_c($bind, $put, $ret), _2 : $testMonads10_o(CSP_($bind, `$bind`), CSP_($put, `$put`), CSP_($ret, `$ret`))})};
const $testMonads11_o = ($bind, $gen, $put, $ret) => ($n34) => {return app_(app_(app_(app_($bind, CSP_undefined_), CSP_undefined_), app_(app_($gen, CSP_undefined_), quote_(App_(App_(CSP_($mul, `$mul`), splice_($n34)), CSP_(100, `100`))))), Lam_(`$n35`, $testMonads10_o($bind, $put, $ret)))};
const $testMonads12_c = ($bind, $gen, $put, $ret) => ($n) => {return (((($bind)._1(undefined))._1(undefined))._1((($gen)._1(undefined))._1(App_(App_(CSP_($add, `$add`), splice_(CSP_($n, `$n`))), CSP_(100, `100`)))))._1({ _1 : $testMonads11_c($bind, $gen, $put, $ret), _2 : $testMonads11_o(CSP_($bind, `$bind`), CSP_($gen, `$gen`), CSP_($put, `$put`), CSP_($ret, `$ret`))})};
const $testMonads12_o = ($bind, $gen, $put, $ret) => ($n) => {return app_(app_(app_(app_($bind, CSP_undefined_), CSP_undefined_), app_(app_($gen, CSP_undefined_), quote_(App_(App_(CSP_($add, `$add`), splice_($n)), CSP_(100, `100`))))), Lam_(`$n34`, $testMonads11_o($bind, $gen, $put, $ret)))};
const $testMonads13_c = ($MM, $MS) => ($MG) => {const $tmp27 = $MM;
    const $ret = $tmp27.ret;
    const $bind = $tmp27.bind;
    const $tmp29 = $MS;
    const $get = $tmp29.get;
    const $put = $tmp29.put;
    const $tmp31 = $MG;
    const $gen = $tmp31.gen;
    const $liftGen = $tmp31.liftGen;
    return (((($bind)._1(undefined))._1(undefined))._1($get))._1({ _1 : $testMonads12_c($bind, $gen, $put, $ret), _2 : $testMonads12_o(CSP_($bind, `$bind`), CSP_($gen, `$gen`), CSP_($put, `$put`), CSP_($ret, `$ret`))})};
const $testMonads13_o = ($MM, $MS) => ($MG) => {const $tmp27 = $MM;
    const $ret = proj_($tmp27, `ret`);
    const $bind = proj_($tmp27, `bind`);
    const $tmp29 = $MS;
    const $get = proj_($tmp29, `get`);
    const $put = proj_($tmp29, `put`);
    const $tmp31 = $MG;
    const $gen = proj_($tmp31, `gen`);
    const $liftGen = proj_($tmp31, `liftGen`);
    return app_(app_(app_(app_($bind, CSP_undefined_), CSP_undefined_), $get), Lam_(`$n`, $testMonads12_o($bind, $gen, $put, $ret)))};
const $testMonads14_c = ($MM) => ($MS) => {return { _1 : $testMonads13_c($MM, $MS), _2 : $testMonads13_o(CSP_($MM, `$MM`), CSP_($MS, `$MS`))}};
const $testMonads14_o = ($MM) => ($MS) => {return Lam_(`$MG`, $testMonads13_o($MM, $MS))};
const $testMonads15_c = ($MM) => {return { _1 : $testMonads14_c($MM), _2 : $testMonads14_o(CSP_($MM, `$MM`))}};
const $testMonads15_o = ($MM) => {return Lam_(`$MS`, $testMonads14_o($MM))};
const $testMonads16_c = ($M) => {return { _1 : $testMonads15_c, _2 : $testMonads15_o}};
const $testMonads16_o = ($M) => {return Lam_(`$MM`, $testMonads15_o)};
const $testMonads17_c = ($m) => {return Lam_(`$s`, ($s) => {return splice_(app_(app_(app_(CSP_($m, `$m`), quote_($s)), CSP_undefined_), Lam_(`$as`, ($as) => {return quote_(Rec_(new Map([[`fst`, splice_(proj_($as, `fst`))], [`snd`, splice_(proj_($as, `snd`))]])))})))})};
const $testMonads17_o = ($m) => {return quote_(Lam_(`$s`, ($s) => {return splice_(app_(app_(app_($m, quote_($s)), CSP_undefined_), Lam_(`$as`, ($as) => {return quote_(Rec_(new Map([[`fst`, splice_(proj_($as, `fst`))], [`snd`, splice_(proj_($as, `snd`))]])))})))}))};
const $testMonads18_c = ($ret) => ($_) => {return (($ret)._1(undefined))._1(Rec_(new Map([])))};
const $testMonads18_o = ($ret) => ($_) => {return app_(app_($ret, CSP_undefined_), quote_(Rec_(new Map([]))))};
const $testMonads19_c = ($bind, $put, $ret) => ($n33) => {return (((($bind)._1(undefined))._1(undefined))._1(($put)._1($n33)))._1({ _1 : $testMonads18_c($ret), _2 : $testMonads18_o(CSP_($ret, `$ret`))})};
const $testMonads19_o = ($bind, $put, $ret) => ($n33) => {return app_(app_(app_(app_($bind, CSP_undefined_), CSP_undefined_), app_($put, $n33)), Lam_(`$_`, $testMonads18_o($ret)))};
const $testMonads20_c = ($bind, $gen, $put, $ret) => ($n32) => {return (((($bind)._1(undefined))._1(undefined))._1((($gen)._1(undefined))._1(App_(App_(CSP_($mul, `$mul`), splice_(CSP_($n32, `$n32`))), CSP_(10, `10`)))))._1({ _1 : $testMonads19_c($bind, $put, $ret), _2 : $testMonads19_o(CSP_($bind, `$bind`), CSP_($put, `$put`), CSP_($ret, `$ret`))})};
const $testMonads20_o = ($bind, $gen, $put, $ret) => ($n32) => {return app_(app_(app_(app_($bind, CSP_undefined_), CSP_undefined_), app_(app_($gen, CSP_undefined_), quote_(App_(App_(CSP_($mul, `$mul`), splice_($n32)), CSP_(10, `10`))))), Lam_(`$n33`, $testMonads19_o($bind, $put, $ret)))};
const $testMonads21_c = ($bind, $gen, $put, $ret) => ($n) => {return (((($bind)._1(undefined))._1(undefined))._1((($gen)._1(undefined))._1(App_(App_(CSP_($add, `$add`), splice_(CSP_($n, `$n`))), CSP_(10, `10`)))))._1({ _1 : $testMonads20_c($bind, $gen, $put, $ret), _2 : $testMonads20_o(CSP_($bind, `$bind`), CSP_($gen, `$gen`), CSP_($put, `$put`), CSP_($ret, `$ret`))})};
const $testMonads21_o = ($bind, $gen, $put, $ret) => ($n) => {return app_(app_(app_(app_($bind, CSP_undefined_), CSP_undefined_), app_(app_($gen, CSP_undefined_), quote_(App_(App_(CSP_($add, `$add`), splice_($n)), CSP_(10, `10`))))), Lam_(`$n32`, $testMonads20_o($bind, $gen, $put, $ret)))};
const $testMonads = (($modify) => ((($inst1) => ((($inst2) => ((($inst3) => ((($testPoly) => ((($down) => ((($testMonoObj) => (($testMonoObj)._1(10).snd))(codegenClosed_(($down)._1((($tmp25) => ((($ret) => ((($bind) => ((($tmp27) => ((($get) => ((($put) => ((($tmp29) => ((($gen) => ((($liftGen) => ((((($bind)._1(undefined))._1(undefined))._1($get))._1({ _1 : $testMonads21_c($bind, $gen, $put, $ret), _2 : $testMonads21_o(CSP_($bind, `$bind`), CSP_($gen, `$gen`), CSP_($put, `$put`), CSP_($ret, `$ret`))})))($tmp29.liftGen)))($tmp29.gen)))($inst3)))($tmp27.put)))($tmp27.get)))($inst2)))($tmp25.bind)))($tmp25.ret)))($inst1)), [`(stdin):97:45:`, `   |`, `   | `, `97 |   let testMonoObj : StateT ℕ Identity Σ() = ~(down $`, `   |                                             ^`]))))({ _1 : $testMonads17_c, _2 : $testMonads17_o})))({ _1 : $testMonads16_c, _2 : $testMonads16_o})))((((($MGenStateT)._1(undefined))._1({ _1 : $testMonads8_c, _2 : $testMonads8_o}))._1($MGen))._1($MGenGen))))(((($MStateStateT)._1(undefined))._1({ _1 : $testMonads7_c, _2 : $testMonads7_o}))._1($MGen))))(((($MStateT)._1(undefined))._1({ _1 : $testMonads6_c, _2 : $testMonads6_o}))._1($MGen))))({ _1 : $testMonads5_c, _2 : $testMonads5_o});

const $Vec0_c = ($A) => ($B) => {return (($Pair)._1($A))._1($B)};
const $Vec0_o = ($A) => ($B) => {return app_(app_(CSP_($Pair, `$Pair`), $A), $B)};
const $Vec1_c = ($A) => ($_) => {return { _1 : $Vec0_c($A), _2 : $Vec0_o(CSP_($A, `$A`))}};
const $Vec1_o = ($A) => ($_) => {return Lam_(`$B`, $Vec0_o($A))};
const $Vec2_c = ($n) => ($A) => {return (((($iter)._1(undefined))._1($n))._1({ _1 : $Vec1_c($A), _2 : $Vec1_o(CSP_($A, `$A`))}))._1(undefined)};
const $Vec2_o = ($n) => ($A) => {return app_(app_(app_(app_(CSP_($iter, `$iter`), CSP_undefined_), $n), Lam_(`$_`, $Vec1_o($A))), CSP_undefined_)};
const $Vec3_c = ($n) => {return { _1 : $Vec2_c($n), _2 : $Vec2_o(CSP_($n, `$n`))}};
const $Vec3_o = ($n) => {return Lam_(`$A`, $Vec2_o($n))};
const $Vec = { _1 : $Vec3_c, _2 : $Vec3_o};

const $nil0_c = ($A) => {return {}};
const $nil0_o = ($A) => {return Rec_(new Map([]))};
const $nil = { _1 : $nil0_c, _2 : $nil0_o};

const $cons0_c = ($a) => ($as) => {return {fst: $a, snd: $as}};
const $cons0_o = ($a) => ($as) => {return Rec_(new Map([[`fst`, $a], [`snd`, $as]]))};
const $cons1_c = ($a) => {return { _1 : $cons0_c($a), _2 : $cons0_o(CSP_($a, `$a`))}};
const $cons1_o = ($a) => {return Lam_(`$as`, $cons0_o($a))};
const $cons2_c = ($n) => {return { _1 : $cons1_c, _2 : $cons1_o}};
const $cons2_o = ($n) => {return Lam_(`$a`, $cons1_o)};
const $cons3_c = ($A) => {return { _1 : $cons2_c, _2 : $cons2_o}};
const $cons3_o = ($A) => {return Lam_(`$n`, $cons2_o)};
const $cons = { _1 : $cons3_c, _2 : $cons3_o};

const $map0_c = ($f, $rec) => ($acc) => {return {fst: ($f)._1($acc.fst), snd: ($rec)._1($acc.snd)}};
const $map0_o = ($f, $rec) => ($acc) => {return Rec_(new Map([[`fst`, app_($f, proj_($acc, `fst`))], [`snd`, app_($rec, proj_($acc, `snd`))]]))};
const $map1_c = ($f) => ($rec) => {return { _1 : $map0_c($f, $rec), _2 : $map0_o(CSP_($f, `$f`), CSP_($rec, `$rec`))}};
const $map1_o = ($f) => ($rec) => {return Lam_(`$acc`, $map0_o($f, $rec))};
const $map2_c = ($f) => ($n28) => {return { _1 : $map1_c($f), _2 : $map1_o(CSP_($f, `$f`))}};
const $map2_o = ($f) => ($n28) => {return Lam_(`$rec`, $map1_o($f))};
const $map3_c = ($_) => {return {}};
const $map3_o = ($_) => {return Rec_(new Map([]))};
const $map4_c = ($f, $n) => ($as) => {return (cNatElim_({ _1 : $map2_c($f), _2 : $map2_o(CSP_($f, `$f`))}, { _1 : $map3_c, _2 : $map3_o}, $n))._1($as)};
const $map4_o = ($f, $n) => ($as) => {return app_(natElim_(Lam_(`$n28`, $map2_o($f)), Lam_(`$_`, $map3_o), $n), $as)};
const $map5_c = ($n) => ($f) => {return { _1 : $map4_c($f, $n), _2 : $map4_o(CSP_($f, `$f`), CSP_($n, `$n`))}};
const $map5_o = ($n) => ($f) => {return Lam_(`$as`, $map4_o($f, $n))};
const $map6_c = ($n) => {return { _1 : $map5_c($n), _2 : $map5_o(CSP_($n, `$n`))}};
const $map6_o = ($n) => {return Lam_(`$f`, $map5_o($n))};
const $map7_c = ($B) => {return { _1 : $map6_c, _2 : $map6_o}};
const $map7_o = ($B) => {return Lam_(`$n`, $map6_o)};
const $map8_c = ($A) => {return { _1 : $map7_c, _2 : $map7_o}};
const $map8_o = ($A) => {return Lam_(`$B`, $map7_o)};
const $map = { _1 : $map8_c, _2 : $map8_o};

const $foldl0_c = ($B32) => ($x) => {return ($B32)._1(cSuc_($x))};
const $foldl0_o = ($B32) => ($x) => {return app_($B32, suc_($x))};
const $foldl1_c = ($f33) => ($n36) => {return ($f33)._1(cSuc_($n36))};
const $foldl1_o = ($f33) => ($n36) => {return app_($f33, suc_($n36))};
const $foldl2_c = ($B32, $f33, $rec, $z34) => ($as35) => {return (((($rec)._1({ _1 : $foldl0_c($B32), _2 : $foldl0_o(CSP_($B32, `$B32`))}))._1({ _1 : $foldl1_c($f33), _2 : $foldl1_o(CSP_($f33, `$f33`))}))._1(((($f33)._1(0))._1($z34))._1($as35.fst)))._1($as35.snd)};
const $foldl2_o = ($B32, $f33, $rec, $z34) => ($as35) => {return app_(app_(app_(app_($rec, Lam_(`$x`, $foldl0_o($B32))), Lam_(`$n36`, $foldl1_o($f33))), app_(app_(app_($f33, CSP_(0, `0`)), $z34), proj_($as35, `fst`))), proj_($as35, `snd`))};
const $foldl3_c = ($B32, $f33, $rec) => ($z34) => {return { _1 : $foldl2_c($B32, $f33, $rec, $z34), _2 : $foldl2_o(CSP_($B32, `$B32`), CSP_($f33, `$f33`), CSP_($rec, `$rec`), CSP_($z34, `$z34`))}};
const $foldl3_o = ($B32, $f33, $rec) => ($z34) => {return Lam_(`$as35`, $foldl2_o($B32, $f33, $rec, $z34))};
const $foldl4_c = ($B32, $rec) => ($f33) => {return { _1 : $foldl3_c($B32, $f33, $rec), _2 : $foldl3_o(CSP_($B32, `$B32`), CSP_($f33, `$f33`), CSP_($rec, `$rec`))}};
const $foldl4_o = ($B32, $rec) => ($f33) => {return Lam_(`$z34`, $foldl3_o($B32, $f33, $rec))};
const $foldl5_c = ($rec) => ($B32) => {return { _1 : $foldl4_c($B32, $rec), _2 : $foldl4_o(CSP_($B32, `$B32`), CSP_($rec, `$rec`))}};
const $foldl5_o = ($rec) => ($B32) => {return Lam_(`$f33`, $foldl4_o($B32, $rec))};
const $foldl6_c = ($rec) => {return { _1 : $foldl5_c($rec), _2 : $foldl5_o(CSP_($rec, `$rec`))}};
const $foldl6_o = ($rec) => {return Lam_(`$B32`, $foldl5_o($rec))};
const $foldl7_c = ($n30) => {return { _1 : $foldl6_c, _2 : $foldl6_o}};
const $foldl7_o = ($n30) => {return Lam_(`$rec`, $foldl6_o)};
const $foldl8_c = ($z32) => ($as33) => {return $z32};
const $foldl8_o = ($z32) => ($as33) => {return $z32};
const $foldl9_c = ($z32) => {return { _1 : $foldl8_c($z32), _2 : $foldl8_o(CSP_($z32, `$z32`))}};
const $foldl9_o = ($z32) => {return Lam_(`$as33`, $foldl8_o($z32))};
const $foldl10_c = ($f31) => {return { _1 : $foldl9_c, _2 : $foldl9_o}};
const $foldl10_o = ($f31) => {return Lam_(`$z32`, $foldl9_o)};
const $foldl11_c = ($B30) => {return { _1 : $foldl10_c, _2 : $foldl10_o}};
const $foldl11_o = ($B30) => {return Lam_(`$f31`, $foldl10_o)};
const $foldl12_c = ($f) => ($n30) => {return ($f)._1($n30)};
const $foldl12_o = ($f) => ($n30) => {return app_($f, $n30)};
const $foldl13_c = ($B, $f, $n, $z) => ($as) => {return ((((cNatElim_({ _1 : $foldl7_c, _2 : $foldl7_o}, { _1 : $foldl11_c, _2 : $foldl11_o}, $n))._1($B))._1({ _1 : $foldl12_c($f), _2 : $foldl12_o(CSP_($f, `$f`))}))._1($z))._1($as)};
const $foldl13_o = ($B, $f, $n, $z) => ($as) => {return app_(app_(app_(app_(natElim_(Lam_(`$n30`, $foldl7_o), Lam_(`$B30`, $foldl11_o), $n), $B), Lam_(`$n30`, $foldl12_o($f))), $z), $as)};
const $foldl14_c = ($B, $f, $z) => ($n) => {return { _1 : $foldl13_c($B, $f, $n, $z), _2 : $foldl13_o(CSP_($B, `$B`), CSP_($f, `$f`), CSP_($n, `$n`), CSP_($z, `$z`))}};
const $foldl14_o = ($B, $f, $z) => ($n) => {return Lam_(`$as`, $foldl13_o($B, $f, $n, $z))};
const $foldl15_c = ($B, $f) => ($z) => {return { _1 : $foldl14_c($B, $f, $z), _2 : $foldl14_o(CSP_($B, `$B`), CSP_($f, `$f`), CSP_($z, `$z`))}};
const $foldl15_o = ($B, $f) => ($z) => {return Lam_(`$n`, $foldl14_o($B, $f, $z))};
const $foldl16_c = ($B) => ($f) => {return { _1 : $foldl15_c($B, $f), _2 : $foldl15_o(CSP_($B, `$B`), CSP_($f, `$f`))}};
const $foldl16_o = ($B) => ($f) => {return Lam_(`$z`, $foldl15_o($B, $f))};
const $foldl17_c = ($B) => {return { _1 : $foldl16_c($B), _2 : $foldl16_o(CSP_($B, `$B`))}};
const $foldl17_o = ($B) => {return Lam_(`$f`, $foldl16_o($B))};
const $foldl18_c = ($A) => {return { _1 : $foldl17_c, _2 : $foldl17_o}};
const $foldl18_o = ($A) => {return Lam_(`$B`, $foldl17_o)};
const $foldl = { _1 : $foldl18_c, _2 : $foldl18_o};

const $foldr0_c = ($f, $n, $z) => ($as) => {return App_(NatElim_(Lam_(`$n31`, ($n31) => {return Lam_(`$rec`, ($rec) => {return Lam_(`$as33`, ($as33) => {return splice_(app_(app_(app_(app_(CSP_($f, `$f`), $n31), quote_($as33)), quote_(Proj_($as33, `fst`))), quote_(App_($rec, Proj_($as33, `snd`)))))})})}), Lam_(`$_`, ($_) => {return splice_(CSP_($z, `$z`))}), CSP_($n, `$n`)), splice_(CSP_($as, `$as`)))};
const $foldr0_o = ($f, $n, $z) => ($as) => {return quote_(App_(NatElim_(Lam_(`$n31`, ($n31) => {return Lam_(`$rec`, ($rec) => {return Lam_(`$as33`, ($as33) => {return splice_(app_(app_(app_(app_($f, $n31), quote_($as33)), quote_(Proj_($as33, `fst`))), quote_(App_($rec, Proj_($as33, `snd`)))))})})}), Lam_(`$_`, ($_) => {return splice_($z)}), $n), splice_($as)))};
const $foldr1_c = ($f, $z) => ($n) => {return { _1 : $foldr0_c($f, $n, $z), _2 : $foldr0_o(CSP_($f, `$f`), CSP_($n, `$n`), CSP_($z, `$z`))}};
const $foldr1_o = ($f, $z) => ($n) => {return Lam_(`$as`, $foldr0_o($f, $n, $z))};
const $foldr2_c = ($f) => ($z) => {return { _1 : $foldr1_c($f, $z), _2 : $foldr1_o(CSP_($f, `$f`), CSP_($z, `$z`))}};
const $foldr2_o = ($f) => ($z) => {return Lam_(`$n`, $foldr1_o($f, $z))};
const $foldr3_c = ($f) => {return { _1 : $foldr2_c($f), _2 : $foldr2_o(CSP_($f, `$f`))}};
const $foldr3_o = ($f) => {return Lam_(`$z`, $foldr2_o($f))};
const $foldr4_c = ($B) => {return { _1 : $foldr3_c, _2 : $foldr3_o}};
const $foldr4_o = ($B) => {return Lam_(`$f`, $foldr3_o)};
const $foldr5_c = ($A) => {return { _1 : $foldr4_c, _2 : $foldr4_o}};
const $foldr5_o = ($A) => {return Lam_(`$B`, $foldr4_o)};
const $foldr = { _1 : $foldr5_c, _2 : $foldr5_o};

const $up0_c = ($a38) => {return undefined};
const $up0_o = ($a38) => {return CSP_undefined_};
const $up1_c = ($n37) => {return { _1 : $up0_c, _2 : $up0_o}};
const $up1_o = ($n37) => {return Lam_(`$a38`, $up0_o)};
const $up2_c = ($a39) => {return undefined};
const $up2_o = ($a39) => {return CSP_undefined_};
const $up3_c = ($n38) => {return { _1 : $up2_c, _2 : $up2_o}};
const $up3_o = ($n38) => {return Lam_(`$a39`, $up2_o)};
const $up4_c = ($a, $ret) => ($as38) => {return (($ret)._1(undefined))._1({fst: $a, snd: $as38})};
const $up4_o = ($a, $ret) => ($as38) => {return app_(app_($ret, CSP_undefined_), Rec_(new Map([[`fst`, $a], [`snd`, $as38]])))};
const $up5_c = ($a, $bind, $n33, $rec, $ret) => ($as37) => {return (((($bind)._1(cNatElim_({ _1 : $up3_c, _2 : $up3_o}, undefined, $n33)))._1(undefined))._1(($rec)._1($as37)))._1({ _1 : $up4_c($a, $ret), _2 : $up4_o(CSP_($a, `$a`), CSP_($ret, `$ret`))})};
const $up5_o = ($a, $bind, $n33, $rec, $ret) => ($as37) => {return app_(app_(app_(app_($bind, natElim_(Lam_(`$n38`, $up3_o), CSP_undefined_, $n33)), CSP_undefined_), app_($rec, $as37)), Lam_(`$as38`, $up4_o($a, $ret)))};
const $up6_c = ($as35, $bind, $gen, $n33, $rec, $ret) => ($a) => {return (((($bind)._1(undefined))._1(undefined))._1((($gen)._1(cNatElim_({ _1 : $up1_c, _2 : $up1_o}, undefined, $n33)))._1(Proj_(splice_(CSP_($as35, `$as35`)), `snd`))))._1({ _1 : $up5_c($a, $bind, $n33, $rec, $ret), _2 : $up5_o(CSP_($a, `$a`), CSP_($bind, `$bind`), CSP_($n33, `$n33`), CSP_($rec, `$rec`), CSP_($ret, `$ret`))})};
const $up6_o = ($as35, $bind, $gen, $n33, $rec, $ret) => ($a) => {return app_(app_(app_(app_($bind, CSP_undefined_), CSP_undefined_), app_(app_($gen, natElim_(Lam_(`$n37`, $up1_o), CSP_undefined_, $n33)), quote_(Proj_(splice_($as35), `snd`)))), Lam_(`$as37`, $up5_o($a, $bind, $n33, $rec, $ret)))};
const $up7_c = ($A, $bind, $gen, $n33, $rec, $ret) => ($as35) => {return (((($bind)._1(undefined))._1(undefined))._1((($gen)._1($A))._1(Proj_(splice_(CSP_($as35, `$as35`)), `fst`))))._1({ _1 : $up6_c($as35, $bind, $gen, $n33, $rec, $ret), _2 : $up6_o(CSP_($as35, `$as35`), CSP_($bind, `$bind`), CSP_($gen, `$gen`), CSP_($n33, `$n33`), CSP_($rec, `$rec`), CSP_($ret, `$ret`))})};
const $up7_o = ($A, $bind, $gen, $n33, $rec, $ret) => ($as35) => {return app_(app_(app_(app_($bind, CSP_undefined_), CSP_undefined_), app_(app_($gen, $A), quote_(Proj_(splice_($as35), `fst`)))), Lam_(`$a`, $up6_o($as35, $bind, $gen, $n33, $rec, $ret)))};
const $up8_c = ($A, $bind, $gen, $n33, $ret) => ($rec) => {return { _1 : $up7_c($A, $bind, $gen, $n33, $rec, $ret), _2 : $up7_o(CSP_($A, `$A`), CSP_($bind, `$bind`), CSP_($gen, `$gen`), CSP_($n33, `$n33`), CSP_($rec, `$rec`), CSP_($ret, `$ret`))}};
const $up8_o = ($A, $bind, $gen, $n33, $ret) => ($rec) => {return Lam_(`$as35`, $up7_o($A, $bind, $gen, $n33, $rec, $ret))};
const $up9_c = ($A, $bind, $gen, $ret) => ($n33) => {return { _1 : $up8_c($A, $bind, $gen, $n33, $ret), _2 : $up8_o(CSP_($A, `$A`), CSP_($bind, `$bind`), CSP_($gen, `$gen`), CSP_($n33, `$n33`), CSP_($ret, `$ret`))}};
const $up9_o = ($A, $bind, $gen, $ret) => ($n33) => {return Lam_(`$rec`, $up8_o($A, $bind, $gen, $n33, $ret))};
const $up10_c = ($ret) => ($as33) => {return (($ret)._1(undefined))._1({})};
const $up10_o = ($ret) => ($as33) => {return app_(app_($ret, CSP_undefined_), Rec_(new Map([])))};
const $up11_c = ($A, $n) => ($as) => {const $tmp29 = $MGen;
    const $ret = $tmp29.ret;
    const $bind = $tmp29.bind;
    const $tmp31 = $MGenGen;
    const $gen = $tmp31.gen;
    const $liftGen = $tmp31.liftGen;
    return (cNatElim_({ _1 : $up9_c($A, $bind, $gen, $ret), _2 : $up9_o(CSP_($A, `$A`), CSP_($bind, `$bind`), CSP_($gen, `$gen`), CSP_($ret, `$ret`))}, { _1 : $up10_c($ret), _2 : $up10_o(CSP_($ret, `$ret`))}, $n))._1($as)};
const $up11_o = ($A, $n) => ($as) => {const $tmp29 = CSP_($MGen, `$MGen`);
    const $ret = proj_($tmp29, `ret`);
    const $bind = proj_($tmp29, `bind`);
    const $tmp31 = CSP_($MGenGen, `$MGenGen`);
    const $gen = proj_($tmp31, `gen`);
    const $liftGen = proj_($tmp31, `liftGen`);
    return app_(natElim_(Lam_(`$n33`, $up9_o($A, $bind, $gen, $ret)), Lam_(`$as33`, $up10_o($ret)), $n), $as)};
const $up12_c = ($A) => ($n) => {return { _1 : $up11_c($A, $n), _2 : $up11_o(CSP_($A, `$A`), CSP_($n, `$n`))}};
const $up12_o = ($A) => ($n) => {return Lam_(`$as`, $up11_o($A, $n))};
const $up13_c = ($A) => {return { _1 : $up12_c($A), _2 : $up12_o(CSP_($A, `$A`))}};
const $up13_o = ($A) => {return Lam_(`$n`, $up12_o($A))};
const $up = { _1 : $up13_c, _2 : $up13_o};

const $down = codegenClosed_(Lam_(`$n`, ($n) => {return Lam_(`$A`, ($A) => {return Lam_(`$as`, ($as) => {return splice_(app_(app_(app_(app_(app_(app_(CSP_($foldr, `$foldr`), CSP_undefined_), Lam_(`$n30`, ($n30) => {return CSP_undefined_})), Lam_(`$n30`, ($n30) => {return Lam_(`$_`, ($_) => {return Lam_(`$a`, ($a) => {return Lam_(`$res`, ($res) => {return quote_(quote_(Rec_(new Map([[`fst`, splice_(splice_($a))], [`snd`, splice_(splice_($res))]]))))})})})})), quote_(quote_(Rec_(new Map([]))))), $n), quote_($as)))})})}), [`(stdin):157:3:`, `    |`, `    | down : (n : ℕ) → {A} → Vec n (□ A) → □ (Vec n A) =`, `157 |   ~<λ n {A} as. ~(foldr (λ n. □ (Vec n A))`, `    |   ^`]);

const $append0_c = ($hyp) => ($xs35) => {return {fst: $xs35.fst, snd: ($hyp)._1($xs35.snd)}};
const $append0_o = ($hyp) => ($xs35) => {return Rec_(new Map([[`fst`, proj_($xs35, `fst`)], [`snd`, app_($hyp, proj_($xs35, `snd`))]]))};
const $append1_c = ($hyp) => {return { _1 : $append0_c($hyp), _2 : $append0_o(CSP_($hyp, `$hyp`))}};
const $append1_o = ($hyp) => {return Lam_(`$xs35`, $append0_o($hyp))};
const $append2_c = ($n33) => {return { _1 : $append1_c, _2 : $append1_o}};
const $append2_o = ($n33) => {return Lam_(`$hyp`, $append1_o)};
const $append3_c = ($ys) => ($_) => {return $ys};
const $append3_o = ($ys) => ($_) => {return $ys};
const $append4_c = ($n, $xs) => ($ys) => {return (cNatElim_({ _1 : $append2_c, _2 : $append2_o}, { _1 : $append3_c($ys), _2 : $append3_o(CSP_($ys, `$ys`))}, $n))._1($xs)};
const $append4_o = ($n, $xs) => ($ys) => {return app_(natElim_(Lam_(`$n33`, $append2_o), Lam_(`$_`, $append3_o($ys)), $n), $xs)};
const $append5_c = ($n) => ($xs) => {return { _1 : $append4_c($n, $xs), _2 : $append4_o(CSP_($n, `$n`), CSP_($xs, `$xs`))}};
const $append5_o = ($n) => ($xs) => {return Lam_(`$ys`, $append4_o($n, $xs))};
const $append6_c = ($n) => ($A) => {return { _1 : $append5_c($n), _2 : $append5_o(CSP_($n, `$n`))}};
const $append6_o = ($n) => ($A) => {return Lam_(`$xs`, $append5_o($n))};
const $append7_c = ($n) => ($m) => {return { _1 : $append6_c($n), _2 : $append6_o(CSP_($n, `$n`))}};
const $append7_o = ($n) => ($m) => {return Lam_(`$A`, $append6_o($n))};
const $append8_c = ($n) => {return { _1 : $append7_c($n), _2 : $append7_o(CSP_($n, `$n`))}};
const $append8_o = ($n) => {return Lam_(`$m`, $append7_o($n))};
const $append = { _1 : $append8_c, _2 : $append8_o};

const $replicate0_c = ($a) => ($as) => {return {fst: $a, snd: $as}};
const $replicate0_o = ($a) => ($as) => {return Rec_(new Map([[`fst`, $a], [`snd`, $as]]))};
const $replicate1_c = ($a) => ($n32) => {return { _1 : $replicate0_c($a), _2 : $replicate0_o(CSP_($a, `$a`))}};
const $replicate1_o = ($a) => ($n32) => {return Lam_(`$as`, $replicate0_o($a))};
const $replicate2_c = ($n) => ($a) => {return cNatElim_({ _1 : $replicate1_c($a), _2 : $replicate1_o(CSP_($a, `$a`))}, {}, $n)};
const $replicate2_o = ($n) => ($a) => {return natElim_(Lam_(`$n32`, $replicate1_o($a)), Rec_(new Map([])), $n)};
const $replicate3_c = ($n) => {return { _1 : $replicate2_c($n), _2 : $replicate2_o(CSP_($n, `$n`))}};
const $replicate3_o = ($n) => {return Lam_(`$a`, $replicate2_o($n))};
const $replicate4_c = ($A) => {return { _1 : $replicate3_c, _2 : $replicate3_o}};
const $replicate4_o = ($A) => {return Lam_(`$n`, $replicate3_o)};
const $replicate = { _1 : $replicate4_c, _2 : $replicate4_o};

const $unrolledMap0_c = ($A, $B, $n) => ($f) => {return Lam_(`$as`, ($as) => {return splice_((($tmp35) => ((($ret) => ((($bind) => (app_(app_(CSP_($runGen, `$runGen`), natElim_(Lam_(`$n37`, ($n37) => {return Lam_(`$a`, ($a) => {return CSP_undefined_})}), CSP_undefined_, CSP_($n, `$n`))), app_(app_(app_(app_($bind, natElim_(Lam_(`$n37`, ($n37) => {return Lam_(`$a`, ($a) => {return CSP_undefined_})}), CSP_undefined_, CSP_($n, `$n`))), CSP_undefined_), app_(app_(app_(CSP_($up, `$up`), CSP_($A, `$A`)), CSP_($n, `$n`)), quote_($as))), Lam_(`$as37`, ($as37) => {return app_(app_($ret, CSP_undefined_), app_(app_(app_(CSP_($down, `$down`), CSP_($n, `$n`)), CSP_($B, `$B`)), app_(app_(app_(app_(app_(CSP_($map, `$map`), CSP_undefined_), CSP_undefined_), CSP_($n, `$n`)), CSP_($f, `$f`)), $as37)))})))))(proj_($tmp35, `bind`))))(proj_($tmp35, `ret`))))(CSP_($MGen, `$MGen`)))})};
const $unrolledMap0_o = ($A, $B, $n) => ($f) => {return quote_(Lam_(`$as`, ($as) => {return splice_((($tmp35) => ((($ret) => ((($bind) => (app_(app_(CSP_($runGen, `$runGen`), natElim_(Lam_(`$n37`, ($n37) => {return Lam_(`$a`, ($a) => {return CSP_undefined_})}), CSP_undefined_, $n)), app_(app_(app_(app_($bind, natElim_(Lam_(`$n37`, ($n37) => {return Lam_(`$a`, ($a) => {return CSP_undefined_})}), CSP_undefined_, $n)), CSP_undefined_), app_(app_(app_(CSP_($up, `$up`), $A), $n), quote_($as))), Lam_(`$as37`, ($as37) => {return app_(app_($ret, CSP_undefined_), app_(app_(app_(CSP_($down, `$down`), $n), $B), app_(app_(app_(app_(app_(CSP_($map, `$map`), CSP_undefined_), CSP_undefined_), $n), $f), $as37)))})))))(proj_($tmp35, `bind`))))(proj_($tmp35, `ret`))))(CSP_($MGen, `$MGen`)))}))};
const $unrolledMap1_c = ($A, $n) => ($B) => {return { _1 : $unrolledMap0_c($A, $B, $n), _2 : $unrolledMap0_o(CSP_($A, `$A`), CSP_($B, `$B`), CSP_($n, `$n`))}};
const $unrolledMap1_o = ($A, $n) => ($B) => {return Lam_(`$f`, $unrolledMap0_o($A, $B, $n))};
const $unrolledMap2_c = ($n) => ($A) => {return { _1 : $unrolledMap1_c($A, $n), _2 : $unrolledMap1_o(CSP_($A, `$A`), CSP_($n, `$n`))}};
const $unrolledMap2_o = ($n) => ($A) => {return Lam_(`$B`, $unrolledMap1_o($A, $n))};
const $unrolledMap3_c = ($n) => {return { _1 : $unrolledMap2_c($n), _2 : $unrolledMap2_o(CSP_($n, `$n`))}};
const $unrolledMap3_o = ($n) => {return Lam_(`$A`, $unrolledMap2_o($n))};
const $unrolledMap = { _1 : $unrolledMap3_c, _2 : $unrolledMap3_o};

const $replicateEff0_c = ($act) => ($rec) => {return () => {const $a = $act();
    const $as = $rec();
    return {fst: $a, snd: $as}}};
const $replicateEff0_o = ($act) => ($rec) => {return Bind_(`$a`, $act, ($a) => {return Bind_(`$as`, $rec, ($as) => {return Return_(Rec_(new Map([[`fst`, $a], [`snd`, $as]])))})})};
const $replicateEff1_c = ($act) => ($n34) => {return { _1 : $replicateEff0_c($act), _2 : $replicateEff0_o(CSP_($act, `$act`))}};
const $replicateEff1_o = ($act) => ($n34) => {return Lam_(`$rec`, $replicateEff0_o($act))};
const $replicateEff2_c = ($n) => ($act) => {return cNatElim_({ _1 : $replicateEff1_c($act), _2 : $replicateEff1_o(CSP_($act, `$act`))}, () => {return {}}, $n)};
const $replicateEff2_o = ($n) => ($act) => {return natElim_(Lam_(`$n34`, $replicateEff1_o($act)), Return_(Rec_(new Map([]))), $n)};
const $replicateEff3_c = ($n) => {return { _1 : $replicateEff2_c($n), _2 : $replicateEff2_o(CSP_($n, `$n`))}};
const $replicateEff3_o = ($n) => {return Lam_(`$act`, $replicateEff2_o($n))};
const $replicateEff4_c = ($A) => {return { _1 : $replicateEff3_c, _2 : $replicateEff3_o}};
const $replicateEff4_o = ($A) => {return Lam_(`$n`, $replicateEff3_o)};
const $replicateEff = { _1 : $replicateEff4_c, _2 : $replicateEff4_o};

const $test0_c = ($x) => {return App_(App_(CSP_($add, `$add`), splice_(CSP_($x, `$x`))), CSP_(10, `10`))};
const $test0_o = ($x) => {return quote_(App_(App_(CSP_($add, `$add`), splice_($x)), CSP_(10, `10`)))};
const $test = () => {const $n = readNat_();
    const $ref = {_1 : 0};
    const $f = codegenClosed_((((($unrolledMap)._1($n))._1(undefined))._1(undefined))._1({ _1 : $test0_c, _2 : $test0_o}), [`(stdin):188:11:`, `    |`, `    |   do ref ← new 0;`, `188 |   let f = ~(unrolledMap n (λ x. <add ~x 10>));`, `    |           ^`]);
    const $ns = ((($replicateEff)._1(undefined))._1($n))._1(() => {const $n35 = readNat_();
        $ref._1 = $n35;
        return $n35})();
    const $m = $ref._1;
    printNat_($m);
    return {fst: $n, snd: ($f)._1($ns)}};

const $res = $test();

const main_ = () => {return $res};
console.log('RESULT:');console.log(util_.inspect(main_(), false, null))
