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
