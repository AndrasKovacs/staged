//@ts-check
'use strict';

/** @typedef {String} Name */

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
  {tag: _Var, name: Name, isTop: Boolean, isClosed: Boolean} |
  {tag: _Let, _1: Name, _2: Open, _3: (v: Open) => Open} |
  {tag: _Lam, _1: Name, _2: (v: Open) => Open} |
  {tag: _App, _1: Open, _2: Open} |
  {tag: _Erased} |
  {tag: _Quote, _1: Open} |
  {tag: _Splice, _1: Open} |
  {tag: _Return, _1: Open} |
  {tag: _Bind, _1: Name, _2: Open, _3: (v: Open) => Open} |
  {tag: _Seq, _1: Open, _2: Open} |
  {tag: _New, _1: Open} |
  {tag: _Write, _1: Open, _2: Open} |
  {tag: _Read, _1: Open} |
  {tag: _CSP, _1: Closed}
  } Open

  @typedef {
  undefined          |
  {_1: (v:Closed) => Closed, _2: (v:Open) => Open} |
  Open               |
  {_1 : Closed}
  } Closed
*/

/** @type {(x:Name, isTop:Boolean, isClosed:Boolean) => Open} */
function Var_    (x, isTop, isClosed)       {return {tag: _Var, name: x, isTop: isTop, isClosed: isClosed}}
/** @type {(x:Name, t:Open, u:(v: Open) => Open) => Open} */
function Let_    (x, t, u) {return {tag: _Let, _1: x, _2: t, _3: u}}
/** @type {(x:Name, t:(v: Open) => Open) => Open} */
function Lam_    (x, t)    {return {tag: _Lam, _1: x, _2: t}}
/** @type {(t:Open, u:Open) => Open} */
function App_    (t, u)    {return {tag: _App, _1: t, _2: t}}
/** @type {Open} */
const    Erased_ =  {tag: _Erased}
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
/** @type {(t: Closed) => Open} */
function CSP_ (t)          {return {tag: _CSP, _1: t}}



// Closure conversion
// ----------------------------------------------------------------------------------------------------

/**
   @typedef {
   {tag: _Var, _1: Name} |
   {tag: _CSP, _1: Number} |
   {tag: _Closed, _1: Name} |
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

/** @type {(x: Name) => Tm} */
function TVar_       (x) {return {tag: _Var, _1: x } }
/** @type {(x:Number) => Tm} */
function TCSP_       (i) {return {tag: _CSP, _1: i} }
/** @type {(x:Name) => Tm} */
function TClosed_    (x) {return {tag: _Closed, _1: x} }
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


// closure-conversion state
// ----------------------------------------------------------------------------------------------------

/**
   @typedef {{name : Name, env: Array<Name>, arg: Name, body: Tm}} Closure
*/

/** @type {undefined|Number} */
let mode_ = undefined

/** @type {Set<Name>} */
const cxtNames_ = new Set()

/** @type {Set<Name>} */
let freeVars_ = new Set()

/** @type {Number} */
let nextClosureId_ = 0

/** @type {Array<Closure>} */
let closures_ = new Array()

/** @type {Array<Closed>} */
let cspArray_ = new Array()

/** @type {Name} */
let currentTopName_ = ''

function resetCConvState_(){
    mode_ = undefined
    cxtNames_.clear()
    freeVars_.clear()
    nextClosureId_ = 0
    closures_ = new Array()
    cspArray_ = new Array()
}


//----------------------------------------------------------------------------------------------------

/** @type {(x: String) => String} */
function freshenName_(x){
  let res = x
  while (cxtNames_.has(res)){
    res = res + cxtNames_.size
  }
  return res
}

/**
   @type {(x: Name, act: (x: Name) => Tm) => Tm}
*/
function fresh_(x, act){
    const x2 = freshenName_(x)
    cxtNames_.add(x2)
    const res = act(x2)
    cxtNames_.delete(x)
    return res
}

/**
   @type {(x : Name, act : (x: Name) => Tm) => Tm}
*/
const bind_ = (x, act) =>
  fresh_(x, (x) => {
    const a = act(x)
    freeVars_.delete(x)
    return a
    })

/** @type {(t: Open) => Tm} */
function cconv_(top){
   switch (top.tag){

     case _Var : {
       if (!top.isTop) {
         freeVars_.add(top.name)
       }
       if (mode_ === undefined) {
         return TVar_(top.name)
       } else {
         if (top.isClosed) {
           return TClosed_(top.name)
         } else {
           return TVar_(top.name)
         }
       }
     }

     case _Let : {
       const x = top._1
       const t = top._2
       const u = top._3
       const t2 = cconv_(t)
       return bind_(x, (x) =>
         TLet_(x, t2, cconv_(u(Var_(x, false, mode_ === undefined)))))
     }

     case _Lam : {
       const x = top._1
       const t = top._2
       if (mode_ === undefined) {
         return fresh_(x, (x) => {
           let old_freeVars = freeVars_
           freeVars_ = new Set()
           const t2 = cconv_(t(Var_(x, false, true)))
           freeVars_.delete(x)
           const capture = Array.from(freeVars_.values())
           const clName = currentTopName_ + nextClosureId_
           nextClosureId_ += 1
           closures_.push({name: clName, env: capture, arg: x, body: t2})
           freeVars_.forEach((x) => old_freeVars.add(x))
           freeVars_ = old_freeVars
           return TLiftedLam_(x, capture)
         })
       } else {
         return bind_(x, (x) => TLam_(x, cconv_(t(Var_(x, false, false)))))
       }
     }

     case _App : {
       return TApp_(cconv_(top._1), cconv_(top._2))
     }

     case _Erased : {
       return TErased_
     }

     case _Quote : {
       if (mode_ === undefined){
         mode_ = 1
       } else {
         mode_ += 1
       }
       return TQuote_(cconv_(top._1))
     }

     case _Splice : {
       if (mode_ && mode_ > 0) {
         mode_ -= 1
       }
       return TSplice_(cconv_(top._1))
     }

     case _Return : {
       return TReturn_(cconv_(top._1))
     }

     case _Bind : {
       const t2 = cconv_(top._2)
       return bind_(top._1, (x) =>
         TBind_(x, t2, cconv_(top._3(Var_(x, false, mode_ === undefined)))))
     }

     case _Seq : {
       return TSeq_(cconv_(top._1), cconv_(top._2))
     }

     case _New : {
       return TNew_(cconv_(top._1))
     }

     case _Write : {
       return TWrite_(cconv_(top._1), cconv_(top._2))
     }

     case _Read : {
       return TRead_(cconv_(top._1))
     }

     case _CSP : {
       const id = cspArray_.length
       cspArray_.push(top._1)
       return TCSP_(id)
     }
   }
}

/** @type {(t:Open) => Top} */
function cconvTop_(top){

  /** @type {(cs: Array<Closure>, t: Top) => Top} */
  function addClosures(cs, t){
    let res = t
    for (const cl of cs){
      res = TopClosure_(cl.name, cl.env, cl.arg, cl.body, res)
    }
    return res
  }

  /** @type {(name: Name) => void} */
  function reset(name){
    currentTopName_ = name
    freeVars_.clear()
    nextClosureId_ = 0
    closures_ = new Array()
  }

  switch (top.tag) {
    case _Let : {
      const x = freshenName_(top._1)
      const t = top._2
      const u = top._3

      reset(x)
      const t2 = cconv_(t)
      const new_closures = closures_
      cxtNames_.add(x)
      const u2 = cconvTop_(u(Var_(x, false, mode_ === undefined)))
      return addClosures(new_closures, TopLet_(x, t2, u2))
    }
    case _Bind : {
      const x = freshenName_(top._1)
      const t = top._2
      const u = top._3

      reset(x)
      const t2 = cconv_(t)
      const new_closures = closures_
      cxtNames_.add(x)
      const u2 = cconvTop_(u(Var_(x, false, mode_ === undefined)))
      return addClosures(new_closures, TopBind_(x, t2, u2))
    }

    case _Seq : {
      const t = top._1
      const u = top._2

      reset('cl')
      const t2 = cconv_(t)
      const new_closures = closures_
      const u2 = cconvTop_(u)
      return addClosures(new_closures, TopSeq_(t2, u2))
    }

    default: {
      reset('cl')
      const t2 = cconv_(top)
      return addClosures(closures_, TopBody_(t2))
    }
  }
}

/** @type {(t:Open, m: undefined|Number) => Top} */
function runCConv_(t, m){
  resetCConvState_()
  mode_ = m
  return cconvTop_(t)
}

// open eliminators
//----------------------------------------------------------------------------------------------------

/** @type {(t:Open, u:Open) => Open} */
function app_(t, u) {
    if (t.tag === _CSP) {
        // t must be a closed closure
        const v1 = /** @type{{_1: (v:Closed) => Closed, _2: (v:Open) => Open}} */ (t._1)
        if (u.tag === _CSP) {
            return CSP_(v1._1(u._1))
        } else {
            return v1._2(u)
        }
    } else if (t.tag === _Lam) {
        return t._2(u)
    } else {
        throw new Error('Impossible')
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

// Code generation
//----------------------------------------------------------------------------------------------------

// code builder is simply an array of strings
/** @type {Array<String>} */
const builder_ = new Array()

/** @type {Number} */
let indentation_ = 0

/** @type {Boolean} */
let isTail_ = true

/** @type {(s:String) => void} */
const put_ = (s) => {builder_.push(s)}

/** @type {(s:String) => () => void} */
const str_ = (s) => () => put_(s)

/** @type {(s:String) => () => void} */
const strLit_ = (s) => () => put_("'" + s + "'")

/** @type {() => void} */
const newl_ = () => {put_(' '.repeat(indentation_))}

/** @type{Number} */
let stage_ = 0

/** @type{(s : Number, act: () => void) => (() => void)} */
const inStage_ = (s, act) => () => {
  const backup = stage_
  stage_ = s
  const res = act()
  stage_ = backup
  return res
}

/** @type{ (act: () => void) => (() => void) } */
const tail_ = (act) => () => {
  const backup = isTail_
  isTail_ = true
  const res = act()
  isTail_ = backup
  return res
}

/** @type{ (act: () => void) => (() => void) } */
const nonTail_ = (act) => () => {
  const backup = isTail_
  isTail_ = false
  const res = act()
  isTail_ = backup
  return res
}

/** @type{ (act: () => void) => (() => void) } */
const indent_ = (act) => () => {
  const backup = indentation_
  indentation_ += 2
  const res = act()
  indentation_ = backup
  return res
}

/** @type{ () => void } */
function semi_(){
  put_(';')
}

/** @type{ (act: () => void) => (() => void) } */
const par_ = (act) => () => {
  put_('(')
  act()
  put_(')')
}

/** @type{ (x: Name, t: () => void, u: () => void) => (() => void) } */
const jLet_ = (x, t, u) => () => {
  if (isTail_){
    put_('const ' + x + ' = ')
    indent_(nonTail_(t))()
    semi_()
    newl_()
    tail_(u)()
  } else {
    put_('((' + x + ') => ')
    nonTail_(u)()
    put_(')(')
    nonTail_(t)()
    put_(')')
  }
}

/** @type{(xs: Array<() => void>) => (() => void)} */
const jTuple_ = (xs) => () => {
  if (xs.length === 0){
    return
  } else {
    xs[0]()
    xs.slice(1, xs.length).forEach((act) => {put_(', '); act()})
  }
}

/** @type((t : () => void) => () => void)} */
const jReturn_ = (t) => () => {
  if (isTail_) {
    put_('return ')
    nonTail_(t)()
  } else {
    t()
  }
}

/** @type{(xs : Array<Name>, t: () => void) => () => void} */
const jLam_ = (xs, t) => () => {
  jReturn_(() => {
    jTuple_(xs.map(str_))();
    put_(' =>  {')
    tail_(t)()
    put_('}')
  })()
}

/** @type{(xs: Array<Name>, t: () => void) => () => void} */
const jLamExp_ = (xs, t) => () => {
  jReturn_(() => {
    jTuple_(xs.map(str_))();
    put_(' => ')
    nonTail_(t)()
  })()
}

/** @type{(t: () => void, u: () => void) => () => void} */
const cApp_ = (t, u) => () => {
  jReturn_(() => {
    par_(t)()
    put_('._1')
    par_(u)()
  })()
}

/** @type{(t : () => void, args: Array<() => void>) => () => void} */
const jApp_ = (t, args) => () => {
  jReturn_(() => {
    t()
    jTuple_(args)()
  })()
}

/** @type{(t : () => void) => () => void} */
const cRun_ = (t) => () => {
  jReturn_(() => {
    t()
    put_('()')
  })()
}

/** @type{(env: Array<Name>, x: Name, t : () => void) => () => void} */
const jClosure_ = (env, x, t) => () => {
  if (env.length ===0){
    jLam_([x], t)()
  } else {
    jLamExp_(env, jLam_([x], t))()
  }
}

/** @type{(t: () => void, args: Array<() => void>) => () => void} */
const jAppClosure_ = (t, args) => () => {
  if (args.length === 0){
    t()
  } else {
    t()
    jTuple_(args)()
  }
}

/** @type{(x:Name) => Name} */
const closeVar_ = (x) => x + 'c'
/** @type{(x:Name) => Name} */
const openVar_  = (x) => x + 'o'

//----------------------------------------------------------------------------------------------------

/** @type {(t:Tm) => void} */
function exec_(top){
  switch (top.tag){
    case _Var       : return cRun_(() => put_(top._1))()
    case _Let       : return jLet_(top._1, () => ceval_(top._2), () => exec_(top._3))()
    case _Lam       : throw new Error('impossible')
    case _LiftedLam : throw new Error('impossible')
    case _App       : return cRun_(cApp_(() => ceval_(top._1), () => ceval_(top._2)))()
    case _Erased    : throw new Error('impossible')
    case _Quote     : throw new Error('impossible')
    case _Splice    : return jApp_(str_('codegenExec_'), [() => ceval_(top._1)])()
    case _Return    : return jReturn_(() => ceval_(top._1))()
    case _Bind      : return jLet_(top._1, () => exec_(top._2), () => exec_(top._3))()
    case _Seq       : return jLet_(' ', () => exec_(top._1), () => exec_(top._2))()
    case _New       : return jReturn_(() => {put_('{_1 : '); ceval_(top._1); put_('}')})()
    case _Write     : return nonTail_(() => {ceval_(top._1); put_('._1 = '); ceval_(top._2)})()
    case _Read      : return jReturn_(() => {ceval_(top._1); put_('._1 = ')})()
    case _CSP       : return jReturn_(() => put_('csp_[' + top._1 + ']()'))() // running the CSP-d action
  }
}

/** @type {(t:Tm) => void} */
function ceval_(top){
  switch (top.tag){
    case _Var       : return jReturn_(() => put_(top._1))()
    case _Let       : return jLet_(top._1, () => ceval_(top._2), () => exec_(top._3))()
    case _Lam       : throw new Error('impossible')
    case _LiftedLam : return jReturn_(() => {
                        put_('_1 : ');
                        jAppClosure_(str_(closeVar_(top._1)), top._2.map(str_))();
                        put_(', _2 : ');
                        jAppClosure_(str_(openVar_(top._1)), top._2.map(str_))();
                        put_('}')
                        })()
    case _App       : return cApp_(() => ceval_(top._1), () => ceval_(top._2))()
    case _Erased    : return jReturn_(str_('undefined'))()
    case _Quote     : return inStage_(1, () => oeval_(top._1))()
    case _Splice    : return jApp_(str_('codegenClosed_'), [() => ceval_(top._1)])()
    case _Return    : return jLam_([], () => exec_(top))()
    case _Bind      : return jLam_([], () => exec_(top))()
    case _Seq       : return jLam_([], () => exec_(top))()
    case _New       : return jLam_([], () => exec_(top))()
    case _Write     : return jLam_([], () => exec_(top))()
    case _Read      : return jLam_([], () => exec_(top))()
    case _CSP       : return jReturn_(() => put_('csp_[' + top._1 + ']'))()
  }
}

/** @type {(t:Tm) => void} */
function oeval_(top){
  switch (top.tag){
    case _Var : return jReturn_(str_(top._1))()
    case _CSP : return jApp_(str_('Closed_'), [str_('csp_[' + top._1 + ']')])()

    case _Let : {
      if (stage_ === 0){
        return jLet_(top._1, () => oeval_(top._2), () => oeval_(top._3))()
      } else {
        return jApp_(str_('Let_'), [strLit_(top._1), () => oeval_(top._2), jLam_([top._1], () => oeval_(top._3))])()
      }
    }
    case _Lam : {
      if (stage_ === 0) {
        return jLam_([top._1], () => oeval_(top._2))()
      } else {
        return jApp_(str_('Lam_'), [strLit_(top._1), jLam_([top._1], () => oeval_(top._2))])()
      }
    }
    case _LiftedLam : throw Error('impossible')

    case _App : {
      if (stage_ === 0){
        return jApp_(str_('app_'), [() => oeval_(top._1), () => oeval_(top._2)])()
      } else {
        return jApp_(str_('App_'), [() => oeval_(top._1), () => oeval_(top._2)])()
      }
    }
    case _Erased : {
      if (stage_ === 0){
        return jReturn_(str_('Erased_'))()
      } else {
        return jReturn_(str_('undefined'))()
      }
    }
    case _Quote : {
      return jApp_(str_('Quote_'), [inStage_(stage_ + 1, () => oeval_(top._1))])()
    }
    case _Splice : {
      if (stage_ === 0){
        return jApp_(str_('codegenOpen_'), [() => oeval_(top._1)])()
      } else {
        return inStage_(stage_ - 1, jApp_(str_('splice_'), [() => oeval_(top._1)]))()
      }
    }
    case _Return : return jApp_(str_('Return_'), [() => oeval_(top._1)])()
    case _Bind   : return jApp_(str_('Bind_'), [strLit_(top._1), () => oeval_(top._2), jLam_([top._1], () => oeval_(top._3))])()
    case _Seq    : return jApp_(str_('Seq_'), [() => oeval_(top._1), () => oeval_(top._2)])()
    case _New    : return jApp_(str_('New_'), [() => oeval_(top._1)])()
    case _Write  : return jApp_(str_('Write_'), [() => oeval_(top._1), () => oeval_(top._2)])()
    case _Read   : return jApp_(str_('Read_'), [() => oeval_(top._1)])()
  }
}

/** @type {(t:Top) => void} */
function execTop_(top){
  tail_(() => {
    switch (top.tag){
      case _Let : {
        return jLet_(top._1, () => ceval_(top._2), () => {newl_(); execTop_(top._3)})()
      }
      case _Bind : {
        return jLet_(top._1, () => exec_(top._2), () => {newl_(); execTop_(top._3)})()
      }
      case _Seq : {
        return jLet_('_', (() => exec_(top._1)), (() => {newl_(); execTop_(top._2)}))()
      }
      case _Closure : {
        const x    = top._1
        const env  = top._2
        const arg  = top._3
        const body = top._4
        const t    = top._5
        return jLet_(closeVar_(x), () => jClosure_(env, arg, () => {ceval_(body)}),
               jLet_(openVar_(x), () => jClosure_(env, arg, () => inStage_(0, () => oeval_(body))), () =>
               execTop_(t)))()
      }
      case _Body : {
        return exec_(top._1)
      }
    }
  })()
}

/** @type {(t:Top) => void} */
function cevalTop_(top){
  switch (top.tag){
    case _Let : {
      return jLet_(top._1, () => ceval_(top._2), () => {newl_(); cevalTop_(top._3)})()
    }
    case _Bind : {
      return jLam_([], () => execTop_(top))()
    }
    case _Seq : {
      return jLam_([], () => execTop_(top))()
    }
    case _Closure : {
      const x    = top._1
      const env  = top._2
      const arg  = top._3
      const body = top._4
      const t    = top._5
      return jLet_(closeVar_(x), () => jClosure_(env, arg, () => {ceval_(body)}),
             jLet_(openVar_(x), () => jClosure_(env, arg, () => inStage_(0, () => oeval_(body))), () =>
             cevalTop_(t)))()
    }
    case _Body:
      return ceval_(top._1)
  }
}

/** @type {(t:Top) => void} */
function oevalTop_(top){
  switch (top.tag){
    case _Let: {
      if (stage_ === 0){
        return jLet_(top._1, () => oeval_(top._2), () => {newl_(); oevalTop_(top._3)})()
      } else {
        return jApp_(str_('Let_'), [strLit_(top._1), () => oeval_(top._2), jLam_([top._1], () => oevalTop_(top._3))])()
      }
    }
    case _Bind:
      return jApp_(str_('Bind_'), [strLit_(top._1), () => oeval_(top._2), jLam_([top._1], () => oevalTop_(top._3))])()
    case _Seq:
      return jApp_(str_('Seq_'), [() => oeval_(top._1), () => oevalTop_(top._2)])()
    case _Closure:
      throw new Error('impossible')
    case _Body:
      return oeval_(top._1)
  }
}

// Splicing in closed evaluation
/** @type {(t:Open) => Closed} */
function codegenClosed_(t){
  const t2_       = runCConv_(t, undefined)
  builder_.length = 0
  indentation_    = 0
  cevalTop_(t2_)
  const csp_ = cspArray_
  const src_ = builder_.join()
  const res_ = eval(src_)
  return res_
}

// Splicing at stage 0 in open evaluation
/** @type {(t:Open) => Open} */
function codegenOpen_(t){
  const t2_       = runCConv_(t, undefined)
  builder_.length = 0
  indentation_    = 0
  stage_          = 0
  oevalTop_(t2_)
  const csp_ = cspArray_
  const src_ = builder_.join()
  const res_ = eval(src_)
  return res_
}

/** @type {(t:Open) => Closed} */
function codegenExec_(t){
  const t2_       = runCConv_(t, undefined)
  builder_.length = 0
  indentation_    = 0
  execTop_(t2_)
  const src_ = builder_.join()
  const res_ = eval(src_)() // run the resulting action
  return res_
}

// ----------------------------------------------------------------------------------------------------