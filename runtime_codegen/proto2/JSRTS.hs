
module JSRTS where

jsrts :: String
jsrts =
  "//@ts-check\n\
  \'use strict';\n\
  \\n\
  \/** @typedef {String} Name */\n\
  \\n\
  \// Closed and Open values\n\
  \// ----------------------------------------------------------------------------------------------------\n\
  \\n\
  \const _Var       = 'Var'\n\
  \const _Let       = 'Let'\n\
  \const _Lam       = 'Lam'\n\
  \const _App       = 'App'\n\
  \const _Erased    = 'Erased'\n\
  \const _Quote     = 'Quote'\n\
  \const _Splice    = 'Splice'\n\
  \const _Return    = 'Return'\n\
  \const _Bind      = 'Bind'\n\
  \const _Seq       = 'Seq'\n\
  \const _New       = 'New'\n\
  \const _Write     = 'Write'\n\
  \const _Read      = 'Read'\n\
  \const _Closed    = 'Closed'\n\
  \const _Closure   = 'Closure'\n\
  \const _CSP       = 'CSP'\n\
  \const _LiftedLam = 'LiftedLam'\n\
  \const _Body      = 'Body'\n\
  \\n\
  \/**\n\
  \  @typedef {\n\
  \  {tag: _Var, name: Name, isTop: Boolean, isClosed: Boolean} |\n\
  \  {tag: _Let, _1: Name, _2: Open, _3: (v: Open) => Open} |\n\
  \  {tag: _Lam, _1: Name, _2: (v: Open) => Open} |\n\
  \  {tag: _App, _1: Open, _2: Open} |\n\
  \  {tag: _Erased} |\n\
  \  {tag: _Quote, _1: Open} |\n\
  \  {tag: _Splice, _1: Open} |\n\
  \  {tag: _Return, _1: Open} |\n\
  \  {tag: _Bind, _1: Name, _2: Open, _3: (v: Open) => Open} |\n\
  \  {tag: _Seq, _1: Open, _2: Open} |\n\
  \  {tag: _New, _1: Open} |\n\
  \  {tag: _Write, _1: Open, _2: Open} |\n\
  \  {tag: _Read, _1: Open} |\n\
  \  {tag: _CSP, _1: Closed}\n\
  \  } Open\n\
  \\n\
  \  @typedef {\n\
  \  undefined          |\n\
  \  {_1: (v:Closed) => Closed, _2: (v:Open) => Open} |\n\
  \  Open               |\n\
  \  {_1 : Closed}\n\
  \  } Closed\n\
  \*/\n\
  \\n\
  \/** @type {(x:Name, isTop:Boolean, isClosed:Boolean) => Open} */\n\
  \function Var_    (x, isTop, isClosed)       {return {tag: _Var, name: x, isTop: isTop, isClosed: isClosed}}\n\
  \/** @type {(x:Name, t:Open, u:(v: Open) => Open) => Open} */\n\
  \function Let_    (x, t, u) {return {tag: _Let, _1: x, _2: t, _3: u}}\n\
  \/** @type {(x:Name, t:(v: Open) => Open) => Open} */\n\
  \function Lam_    (x, t)    {return {tag: _Lam, _1: x, _2: t}}\n\
  \/** @type {(t:Open, u:Open) => Open} */\n\
  \function App_    (t, u)    {return {tag: _App, _1: t, _2: t}}\n\
  \/** @type {Open} */\n\
  \const    Erased_ =  {tag: _Erased}\n\
  \/** @type {(t: Open) => Open} */\n\
  \function Quote_  (t)       {return {tag: _Quote, _1: t}}\n\
  \/** @type {(t: Open) => Open} */\n\
  \function Splice_ (t)       {return {tag: _Splice, _1: t}}\n\
  \/** @type {(t: Open) => Open} */\n\
  \function Return_ (t)       {return {tag: _Return, _1: t}}\n\
  \/** @type {(x:Name, t:Open, u:(v: Open) => Open) => Open} */\n\
  \function Bind_   (x, t, u) {return {tag: _Bind, _1: x, _2: t, _3: u}}\n\
  \/** @type {(t:Open, u:Open) => Open} */\n\
  \function Seq_    (t, u)    {return {tag: _Seq, _1: t, _2: u}}\n\
  \/** @type {(t: Open) => Open} */\n\
  \function New_    (t)       {return {tag: _New, _1: t}}\n\
  \/** @type {(t:Open, u:Open) => Open} */\n\
  \function Write_  (t, u)    {return {tag: _Write, _1: t, _2: u}}\n\
  \/** @type {(t: Open) => Open} */\n\
  \function Read_   (t)       {return {tag: _Read, _1: t}}\n\
  \/** @type {(t: Closed) => Open} */\n\
  \function CSP_ (t)          {return {tag: _CSP, _1: t}}\n\
  \\n\
  \\n\
  \\n\
  \// Closure conversion\n\
  \// ----------------------------------------------------------------------------------------------------\n\
  \\n\
  \/**\n\
  \   @typedef {\n\
  \   {tag: _Var, _1: Name} |\n\
  \   {tag: _CSP, _1: Number} |\n\
  \   {tag: _Closed, _1: Name} |\n\
  \   {tag: _Let, _1: Name, _2: Tm, _3: Tm} |\n\
  \   {tag: _Lam, _1: Name, _2: Tm} |\n\
  \   {tag: _LiftedLam, _1: Name, _2: Array<Name>} |\n\
  \   {tag: _App, _1: Tm, _2: Tm} |\n\
  \   {tag: _Erased} |\n\
  \   {tag: _Quote, _1: Tm} |\n\
  \   {tag: _Splice, _1: Tm} |\n\
  \   {tag: _Return, _1: Tm} |\n\
  \   {tag: _Bind, _1: Name, _2: Tm, _3: Tm} |\n\
  \   {tag: _Seq, _1: Tm, _2: Tm} |\n\
  \   {tag: _New, _1: Tm} |\n\
  \   {tag: _Write, _1: Tm, _2: Tm} |\n\
  \   {tag: _Read, _1: Tm}\n\
  \   } Tm\n\
  \\n\
  \   @typedef {\n\
  \   {tag: _Let, _1: Name, _2: Tm, _3: Top} |\n\
  \   {tag: _Bind, _1 : Name, _2: Tm, _3: Top} |\n\
  \   {tag: _Seq, _1: Tm, _2: Top} |\n\
  \   {tag: _Body, _1: Tm} |\n\
  \   {tag: _Closure, _1: Name, _2: Array<Name>, _3: Name, _4: Tm, _5: Top}\n\
  \   } Top\n\
  \*/\n\
  \\n\
  \/** @type {(x: Name) => Tm} */\n\
  \function TVar_       (x) {return {tag: _Var, _1: x } }\n\
  \/** @type {(x:Number) => Tm} */\n\
  \function TCSP_       (i) {return {tag: _CSP, _1: i} }\n\
  \/** @type {(x:Name) => Tm} */\n\
  \function TClosed_    (x) {return {tag: _Closed, _1: x} }\n\
  \/** @type {(x:Name, t:Tm, u:Tm) => Tm} */\n\
  \function TLet_       (x,t,u) {return {tag: _Let, _1:x , _2:u , _3:u } }\n\
  \/** @type {(x:Name, t:Tm) => Tm} */\n\
  \function TLam_       (x,t) {return {tag: _Lam, _1:x , _2: t } }\n\
  \/** @type {(x:Name, args:Array<Name>) => Tm} */\n\
  \function TLiftedLam_ (x,args) {return {tag: _LiftedLam, _1: x , _2: args} }\n\
  \/** @type {(t:Tm, u:Tm) => Tm} */\n\
  \function TApp_       (t,u) {return {tag: _App, _1: t , _2: u}}\n\
  \/** @type {Tm} */\n\
  \const    TErased_    = {tag: _Erased}\n\
  \/** @type {(t: Tm) => Tm} */\n\
  \function TQuote_     (t) {return {tag: _Quote, _1: t } }\n\
  \/** @type {(t: Tm) => Tm} */\n\
  \function TSplice_    (t) {return {tag: _Splice, _1: t } }\n\
  \/** @type {(t: Tm) => Tm} */\n\
  \function TReturn_    (t) {return {tag: _Return, _1: t } }\n\
  \/** @type {(x:Name, t:Tm, u:Tm) => Tm} */\n\
  \function TBind_      (x, t, u) {return {tag: _Bind, _1: x , _2: t , _3: u } }\n\
  \/** @type {(t:Tm, u:Tm) => Tm} */\n\
  \function TSeq_       (t, u) {return {tag: _Seq, _1: t , _2: u } }\n\
  \/** @type {(t: Tm) => Tm} */\n\
  \function TNew_       (t) {return {tag: _New, _1: t } }\n\
  \/** @type {(t:Tm, u:Tm) => Tm} */\n\
  \function TWrite_     (t, u) {return {tag: _Write, _1: t , _2: u } }\n\
  \/** @type {(t: Tm) => Tm} */\n\
  \function TRead_      (t) {return {tag: _Read, _1: t }}\n\
  \\n\
  \/** @type {(x:Name, t:Tm, u:Top) => Top} */\n\
  \function TopLet_     (x, t, u) { return {tag: _Let, _1: x , _2: t, _3: u } }\n\
  \/** @type {(x:Name, t:Tm, u:Top) => Top} */\n\
  \function TopBind_    (x, t, u) { return {tag: _Bind, _1: x , _2: t, _3: u } }\n\
  \/** @type {(t:Tm, u:Top) => Top} */\n\
  \function TopSeq_     (t, u) { return {tag: _Seq, _1: t , _2: u } }\n\
  \/** @type {(t: Tm) => Top} */\n\
  \function TopBody_    (t) { return {tag: _Body, _1: t} }\n\
  \/** @type {(x:Name, env: Array<Name>, arg:Name, body:Tm, t:Top) => Top} */\n\
  \function TopClosure_ (x, env, arg, body, t) { return {tag: _Closure, _1: x , _2: env, _3: arg , _4: body , _5: t}}\n\
  \\n\
  \\n\
  \// closure-conversion state\n\
  \// ----------------------------------------------------------------------------------------------------\n\
  \\n\
  \/**\n\
  \   @typedef {{name : Name, env: Array<Name>, arg: Name, body: Tm}} Closure\n\
  \*/\n\
  \\n\
  \/** @type {undefined|Number} */\n\
  \let mode_ = undefined\n\
  \\n\
  \/** @type {Set<Name>} */\n\
  \const cxtNames_ = new Set()\n\
  \\n\
  \/** @type {Set<Name>} */\n\
  \let freeVars_ = new Set()\n\
  \\n\
  \/** @type {Number} */\n\
  \let nextClosureId_ = 0\n\
  \\n\
  \/** @type {Array<Closure>} */\n\
  \let closures_ = new Array()\n\
  \\n\
  \/** @type {Array<Closed>} */\n\
  \let cspArray_ = new Array()\n\
  \\n\
  \/** @type {Name} */\n\
  \let currentTopName_ = ''\n\
  \\n\
  \function resetCConvState_(){\n\
  \    mode_ = undefined\n\
  \    cxtNames_.clear()\n\
  \    freeVars_.clear()\n\
  \    nextClosureId_ = 0\n\
  \    closures_ = new Array()\n\
  \    cspArray_ = new Array()\n\
  \}\n\
  \\n\
  \\n\
  \//----------------------------------------------------------------------------------------------------\n\
  \\n\
  \/** @type {(x: String) => String} */\n\
  \function freshenName_(x){\n\
  \  let res = x\n\
  \  while (cxtNames_.has(res)){\n\
  \    res = res + cxtNames_.size\n\
  \  }\n\
  \  return res\n\
  \}\n\
  \\n\
  \/**\n\
  \   @type {(x: Name, act: (x: Name) => Tm) => Tm}\n\
  \*/\n\
  \function fresh_(x, act){\n\
  \    const x2 = freshenName_(x)\n\
  \    cxtNames_.add(x2)\n\
  \    const res = act(x2)\n\
  \    cxtNames_.delete(x)\n\
  \    return res\n\
  \}\n\
  \\n\
  \/**\n\
  \   @type {(x : Name, act : (x: Name) => Tm) => Tm}\n\
  \*/\n\
  \const bind_ = (x, act) =>\n\
  \  fresh_(x, (x) => {\n\
  \    const a = act(x)\n\
  \    freeVars_.delete(x)\n\
  \    return a\n\
  \    })\n\
  \\n\
  \/** @type {(t: Open) => Tm} */\n\
  \function cconv_(top){\n\
  \   switch (top.tag){\n\
  \\n\
  \     case _Var : {\n\
  \       if (!top.isTop) {\n\
  \         freeVars_.add(top.name)\n\
  \       }\n\
  \       if (mode_ === undefined) {\n\
  \         return TVar_(top.name)\n\
  \       } else {\n\
  \         if (top.isClosed) {\n\
  \           return TClosed_(top.name)\n\
  \         } else {\n\
  \           return TVar_(top.name)\n\
  \         }\n\
  \       }\n\
  \     }\n\
  \\n\
  \     case _Let : {\n\
  \       const x = top._1\n\
  \       const t = top._2\n\
  \       const u = top._3\n\
  \       const t2 = cconv_(t)\n\
  \       return bind_(x, (x) =>\n\
  \         TLet_(x, t2, cconv_(u(Var_(x, false, mode_ === undefined)))))\n\
  \     }\n\
  \\n\
  \     case _Lam : {\n\
  \       const x = top._1\n\
  \       const t = top._2\n\
  \       if (mode_ === undefined) {\n\
  \         return fresh_(x, (x) => {\n\
  \           let old_freeVars = freeVars_\n\
  \           freeVars_ = new Set()\n\
  \           const t2 = cconv_(t(Var_(x, false, true)))\n\
  \           freeVars_.delete(x)\n\
  \           const capture = Array.from(freeVars_.values())\n\
  \           const clName = currentTopName_ + nextClosureId_\n\
  \           nextClosureId_ += 1\n\
  \           closures_.push({name: clName, env: capture, arg: x, body: t2})\n\
  \           freeVars_.forEach((x) => old_freeVars.add(x))\n\
  \           freeVars_ = old_freeVars\n\
  \           return TLiftedLam_(x, capture)\n\
  \         })\n\
  \       } else {\n\
  \         return bind_(x, (x) => TLam_(x, cconv_(t(Var_(x, false, false)))))\n\
  \       }\n\
  \     }\n\
  \\n\
  \     case _App : {\n\
  \       return TApp_(cconv_(top._1), cconv_(top._2))\n\
  \     }\n\
  \\n\
  \     case _Erased : {\n\
  \       return TErased_\n\
  \     }\n\
  \\n\
  \     case _Quote : {\n\
  \       if (mode_ === undefined){\n\
  \         mode_ = 1\n\
  \       } else {\n\
  \         mode_ += 1\n\
  \       }\n\
  \       return TQuote_(cconv_(top._1))\n\
  \     }\n\
  \\n\
  \     case _Splice : {\n\
  \       if (mode_ && mode_ > 0) {\n\
  \         mode_ -= 1\n\
  \       }\n\
  \       return TSplice_(cconv_(top._1))\n\
  \     }\n\
  \\n\
  \     case _Return : {\n\
  \       return TReturn_(cconv_(top._1))\n\
  \     }\n\
  \\n\
  \     case _Bind : {\n\
  \       const t2 = cconv_(top._2)\n\
  \       return bind_(top._1, (x) =>\n\
  \         TBind_(x, t2, cconv_(top._3(Var_(x, false, mode_ === undefined)))))\n\
  \     }\n\
  \\n\
  \     case _Seq : {\n\
  \       return TSeq_(cconv_(top._1), cconv_(top._2))\n\
  \     }\n\
  \\n\
  \     case _New : {\n\
  \       return TNew_(cconv_(top._1))\n\
  \     }\n\
  \\n\
  \     case _Write : {\n\
  \       return TWrite_(cconv_(top._1), cconv_(top._2))\n\
  \     }\n\
  \\n\
  \     case _Read : {\n\
  \       return TRead_(cconv_(top._1))\n\
  \     }\n\
  \\n\
  \     case _CSP : {\n\
  \       const id = cspArray_.length\n\
  \       cspArray_.push(top._1)\n\
  \       return TCSP_(id)\n\
  \     }\n\
  \   }\n\
  \}\n\
  \\n\
  \/** @type {(t:Open) => Top} */\n\
  \function cconvTop_(top){\n\
  \\n\
  \  /** @type {(cs: Array<Closure>, t: Top) => Top} */\n\
  \  function addClosures(cs, t){\n\
  \    let res = t\n\
  \    for (const cl of cs){\n\
  \      res = TopClosure_(cl.name, cl.env, cl.arg, cl.body, res)\n\
  \    }\n\
  \    return res\n\
  \  }\n\
  \\n\
  \  /** @type {(name: Name) => void} */\n\
  \  function reset(name){\n\
  \    currentTopName_ = name\n\
  \    freeVars_.clear()\n\
  \    nextClosureId_ = 0\n\
  \    closures_ = new Array()\n\
  \  }\n\
  \\n\
  \  switch (top.tag) {\n\
  \    case _Let : {\n\
  \      const x = freshenName_(top._1)\n\
  \      const t = top._2\n\
  \      const u = top._3\n\
  \\n\
  \      reset(x)\n\
  \      const t2 = cconv_(t)\n\
  \      const new_closures = closures_\n\
  \      cxtNames_.add(x)\n\
  \      const u2 = cconvTop_(u(Var_(x, false, mode_ === undefined)))\n\
  \      return addClosures(new_closures, TopLet_(x, t2, u2))\n\
  \    }\n\
  \    case _Bind : {\n\
  \      const x = freshenName_(top._1)\n\
  \      const t = top._2\n\
  \      const u = top._3\n\
  \\n\
  \      reset(x)\n\
  \      const t2 = cconv_(t)\n\
  \      const new_closures = closures_\n\
  \      cxtNames_.add(x)\n\
  \      const u2 = cconvTop_(u(Var_(x, false, mode_ === undefined)))\n\
  \      return addClosures(new_closures, TopBind_(x, t2, u2))\n\
  \    }\n\
  \\n\
  \    case _Seq : {\n\
  \      const t = top._1\n\
  \      const u = top._2\n\
  \\n\
  \      reset('cl')\n\
  \      const t2 = cconv_(t)\n\
  \      const new_closures = closures_\n\
  \      const u2 = cconvTop_(u)\n\
  \      return addClosures(new_closures, TopSeq_(t2, u2))\n\
  \    }\n\
  \\n\
  \    default: {\n\
  \      reset('cl')\n\
  \      const t2 = cconv_(top)\n\
  \      return addClosures(closures_, TopBody_(t2))\n\
  \    }\n\
  \  }\n\
  \}\n\
  \\n\
  \/** @type {(t:Open, m: undefined|Number) => Top} */\n\
  \function runCConv_(t, m){\n\
  \  resetCConvState_()\n\
  \  mode_ = m\n\
  \  return cconvTop_(t)\n\
  \}\n\
  \\n\
  \// open eliminators\n\
  \//----------------------------------------------------------------------------------------------------\n\
  \\n\
  \/** @type {(t:Open, u:Open) => Open} */\n\
  \function app_(t, u) {\n\
  \    if (t.tag === _CSP) {\n\
  \        // t must be a closed closure\n\
  \        const v1 = /** @type{{_1: (v:Closed) => Closed, _2: (v:Open) => Open}} */ (t._1)\n\
  \        if (u.tag === _CSP) {\n\
  \            return CSP_(v1._1(u._1))\n\
  \        } else {\n\
  \            return v1._2(u)\n\
  \        }\n\
  \    } else if (t.tag === _Lam) {\n\
  \        return t._2(u)\n\
  \    } else {\n\
  \        throw new Error('Impossible')\n\
  \    }\n\
  \}\n\
  \\n\
  \// Splice in stage 1+, no code generation\n\
  \/** @type {(t:Open) => Open} */\n\
  \function splice_(t) {\n\
  \  if (t.tag == _CSP){\n\
  \    // t must be a quoted open value\n\
  \    return /** @type{Open} */ (t._1)\n\
  \  } else if (t.tag === _Quote) {\n\
  \    return t._1\n\
  \  } else {\n\
  \    return Splice_(t)\n\
  \  }\n\
  \}\n\
  \\n\
  \// Code generation\n\
  \//----------------------------------------------------------------------------------------------------\n\
  \\n\
  \// code builder is simply an array of strings\n\
  \/** @type {Array<String>} */\n\
  \const builder_ = new Array()\n\
  \\n\
  \/** @type {Number} */\n\
  \let indentation_ = 0\n\
  \\n\
  \/** @type {Boolean} */\n\
  \let isTail_ = true\n\
  \\n\
  \/** @type {(s:String) => void} */\n\
  \const put_ = (s) => {builder_.push(s)}\n\
  \\n\
  \/** @type {(s:String) => () => void} */\n\
  \const str_ = (s) => () => put_(s)\n\
  \\n\
  \/** @type {(s:String) => () => void} */\n\
  \const strLit_ = (s) => () => put_(\"'\" + s + \"'\")\n\
  \\n\
  \/** @type {() => void} */\n\
  \const newl_ = () => {put_(' '.repeat(indentation_))}\n\
  \\n\
  \/** @type{Number} */\n\
  \let stage_ = 0\n\
  \\n\
  \/** @type{(s : Number, act: () => void) => (() => void)} */\n\
  \const inStage_ = (s, act) => () => {\n\
  \  const backup = stage_\n\
  \  stage_ = s\n\
  \  const res = act()\n\
  \  stage_ = backup\n\
  \  return res\n\
  \}\n\
  \\n\
  \/** @type{ (act: () => void) => (() => void) } */\n\
  \const tail_ = (act) => () => {\n\
  \  const backup = isTail_\n\
  \  isTail_ = true\n\
  \  const res = act()\n\
  \  isTail_ = backup\n\
  \  return res\n\
  \}\n\
  \\n\
  \/** @type{ (act: () => void) => (() => void) } */\n\
  \const nonTail_ = (act) => () => {\n\
  \  const backup = isTail_\n\
  \  isTail_ = false\n\
  \  const res = act()\n\
  \  isTail_ = backup\n\
  \  return res\n\
  \}\n\
  \\n\
  \/** @type{ (act: () => void) => (() => void) } */\n\
  \const indent_ = (act) => () => {\n\
  \  const backup = indentation_\n\
  \  indentation_ += 2\n\
  \  const res = act()\n\
  \  indentation_ = backup\n\
  \  return res\n\
  \}\n\
  \\n\
  \/** @type{ () => void } */\n\
  \function semi_(){\n\
  \  put_(';')\n\
  \}\n\
  \\n\
  \/** @type{ (act: () => void) => (() => void) } */\n\
  \const par_ = (act) => () => {\n\
  \  put_('(')\n\
  \  act()\n\
  \  put_(')')\n\
  \}\n\
  \\n\
  \/** @type{ (x: Name, t: () => void, u: () => void) => (() => void) } */\n\
  \const jLet_ = (x, t, u) => () => {\n\
  \  if (isTail_){\n\
  \    put_('const ' + x + ' = ')\n\
  \    indent_(nonTail_(t))()\n\
  \    semi_()\n\
  \    newl_()\n\
  \    tail_(u)()\n\
  \  } else {\n\
  \    put_('((' + x + ') => ')\n\
  \    nonTail_(u)()\n\
  \    put_(')(')\n\
  \    nonTail_(t)()\n\
  \    put_(')')\n\
  \  }\n\
  \}\n\
  \\n\
  \/** @type{(xs: Array<() => void>) => (() => void)} */\n\
  \const jTuple_ = (xs) => () => {\n\
  \  if (xs.length === 0){\n\
  \    return\n\
  \  } else {\n\
  \    xs[0]()\n\
  \    xs.slice(1, xs.length).forEach((act) => {put_(', '); act()})\n\
  \  }\n\
  \}\n\
  \\n\
  \/** @type((t : () => void) => () => void)} */\n\
  \const jReturn_ = (t) => () => {\n\
  \  if (isTail_) {\n\
  \    put_('return ')\n\
  \    nonTail_(t)()\n\
  \  } else {\n\
  \    t()\n\
  \  }\n\
  \}\n\
  \\n\
  \/** @type{(xs : Array<Name>, t: () => void) => () => void} */\n\
  \const jLam_ = (xs, t) => () => {\n\
  \  jReturn_(() => {\n\
  \    jTuple_(xs.map(str_))();\n\
  \    put_(' =>  {')\n\
  \    tail_(t)()\n\
  \    put_('}')\n\
  \  })()\n\
  \}\n\
  \\n\
  \/** @type{(xs: Array<Name>, t: () => void) => () => void} */\n\
  \const jLamExp_ = (xs, t) => () => {\n\
  \  jReturn_(() => {\n\
  \    jTuple_(xs.map(str_))();\n\
  \    put_(' => ')\n\
  \    nonTail_(t)()\n\
  \  })()\n\
  \}\n\
  \\n\
  \/** @type{(t: () => void, u: () => void) => () => void} */\n\
  \const cApp_ = (t, u) => () => {\n\
  \  jReturn_(() => {\n\
  \    par_(t)()\n\
  \    put_('._1')\n\
  \    par_(u)()\n\
  \  })()\n\
  \}\n\
  \\n\
  \/** @type{(t : () => void, args: Array<() => void>) => () => void} */\n\
  \const jApp_ = (t, args) => () => {\n\
  \  jReturn_(() => {\n\
  \    t()\n\
  \    jTuple_(args)()\n\
  \  })()\n\
  \}\n\
  \\n\
  \/** @type{(t : () => void) => () => void} */\n\
  \const cRun_ = (t) => () => {\n\
  \  jReturn_(() => {\n\
  \    t()\n\
  \    put_('()')\n\
  \  })()\n\
  \}\n\
  \\n\
  \/** @type{(env: Array<Name>, x: Name, t : () => void) => () => void} */\n\
  \const jClosure_ = (env, x, t) => () => {\n\
  \  if (env.length ===0){\n\
  \    jLam_([x], t)()\n\
  \  } else {\n\
  \    jLamExp_(env, jLam_([x], t))()\n\
  \  }\n\
  \}\n\
  \\n\
  \/** @type{(t: () => void, args: Array<() => void>) => () => void} */\n\
  \const jAppClosure_ = (t, args) => () => {\n\
  \  if (args.length === 0){\n\
  \    t()\n\
  \  } else {\n\
  \    t()\n\
  \    jTuple_(args)()\n\
  \  }\n\
  \}\n\
  \\n\
  \/** @type{(x:Name) => Name} */\n\
  \const closeVar_ = (x) => x + 'c'\n\
  \/** @type{(x:Name) => Name} */\n\
  \const openVar_  = (x) => x + 'o'\n\
  \\n\
  \//----------------------------------------------------------------------------------------------------\n\
  \\n\
  \/** @type {(t:Tm) => void} */\n\
  \function exec_(top){\n\
  \  switch (top.tag){\n\
  \    case _Var       : return cRun_(() => put_(top._1))()\n\
  \    case _Let       : return jLet_(top._1, () => ceval_(top._2), () => exec_(top._3))()\n\
  \    case _Lam       : throw new Error('impossible')\n\
  \    case _LiftedLam : throw new Error('impossible')\n\
  \    case _App       : return cRun_(cApp_(() => ceval_(top._1), () => ceval_(top._2)))()\n\
  \    case _Erased    : throw new Error('impossible')\n\
  \    case _Quote     : throw new Error('impossible')\n\
  \    case _Splice    : return jApp_(str_('codegenExec_'), [() => ceval_(top._1)])()\n\
  \    case _Return    : return jReturn_(() => ceval_(top._1))()\n\
  \    case _Bind      : return jLet_(top._1, () => exec_(top._2), () => exec_(top._3))()\n\
  \    case _Seq       : return jLet_(' ', () => exec_(top._1), () => exec_(top._2))()\n\
  \    case _New       : return jReturn_(() => {put_('{_1 : '); ceval_(top._1); put_('}')})()\n\
  \    case _Write     : return nonTail_(() => {ceval_(top._1); put_('._1 = '); ceval_(top._2)})()\n\
  \    case _Read      : return jReturn_(() => {ceval_(top._1); put_('._1 = ')})()\n\
  \    case _CSP       : return jReturn_(() => put_('csp_[' + top._1 + ']()'))() // running the CSP-d action\n\
  \  }\n\
  \}\n\
  \\n\
  \/** @type {(t:Tm) => void} */\n\
  \function ceval_(top){\n\
  \  switch (top.tag){\n\
  \    case _Var       : return jReturn_(() => put_(top._1))()\n\
  \    case _Let       : return jLet_(top._1, () => ceval_(top._2), () => exec_(top._3))()\n\
  \    case _Lam       : throw new Error('impossible')\n\
  \    case _LiftedLam : return jReturn_(() => {\n\
  \                        put_('_1 : ');\n\
  \                        jAppClosure_(str_(closeVar_(top._1)), top._2.map(str_))();\n\
  \                        put_(', _2 : ');\n\
  \                        jAppClosure_(str_(openVar_(top._1)), top._2.map(str_))();\n\
  \                        put_('}')\n\
  \                        })()\n\
  \    case _App       : return cApp_(() => ceval_(top._1), () => ceval_(top._2))()\n\
  \    case _Erased    : return jReturn_(str_('undefined'))()\n\
  \    case _Quote     : return inStage_(1, () => oeval_(top._1))()\n\
  \    case _Splice    : return jApp_(str_('codegenClosed_'), [() => ceval_(top._1)])()\n\
  \    case _Return    : return jLam_([], () => exec_(top))()\n\
  \    case _Bind      : return jLam_([], () => exec_(top))()\n\
  \    case _Seq       : return jLam_([], () => exec_(top))()\n\
  \    case _New       : return jLam_([], () => exec_(top))()\n\
  \    case _Write     : return jLam_([], () => exec_(top))()\n\
  \    case _Read      : return jLam_([], () => exec_(top))()\n\
  \    case _CSP       : return jReturn_(() => put_('csp_[' + top._1 + ']'))()\n\
  \  }\n\
  \}\n\
  \\n\
  \/** @type {(t:Tm) => void} */\n\
  \function oeval_(top){\n\
  \  switch (top.tag){\n\
  \    case _Var : return jReturn_(str_(top._1))()\n\
  \    case _CSP : return jApp_(str_('Closed_'), [str_('csp_[' + top._1 + ']')])()\n\
  \\n\
  \    case _Let : {\n\
  \      if (stage_ === 0){\n\
  \        return jLet_(top._1, () => oeval_(top._2), () => oeval_(top._3))()\n\
  \      } else {\n\
  \        return jApp_(str_('Let_'), [strLit_(top._1), () => oeval_(top._2), jLam_([top._1], () => oeval_(top._3))])()\n\
  \      }\n\
  \    }\n\
  \    case _Lam : {\n\
  \      if (stage_ === 0) {\n\
  \        return jLam_([top._1], () => oeval_(top._2))()\n\
  \      } else {\n\
  \        return jApp_(str_('Lam_'), [strLit_(top._1), jLam_([top._1], () => oeval_(top._2))])()\n\
  \      }\n\
  \    }\n\
  \    case _LiftedLam : throw Error('impossible')\n\
  \\n\
  \    case _App : {\n\
  \      if (stage_ === 0){\n\
  \        return jApp_(str_('app_'), [() => oeval_(top._1), () => oeval_(top._2)])()\n\
  \      } else {\n\
  \        return jApp_(str_('App_'), [() => oeval_(top._1), () => oeval_(top._2)])()\n\
  \      }\n\
  \    }\n\
  \    case _Erased : {\n\
  \      if (stage_ === 0){\n\
  \        return jReturn_(str_('Erased_'))()\n\
  \      } else {\n\
  \        return jReturn_(str_('undefined'))()\n\
  \      }\n\
  \    }\n\
  \    case _Quote : {\n\
  \      return jApp_(str_('Quote_'), [inStage_(stage_ + 1, () => oeval_(top._1))])()\n\
  \    }\n\
  \    case _Splice : {\n\
  \      if (stage_ === 0){\n\
  \        return jApp_(str_('codegenOpen_'), [() => oeval_(top._1)])()\n\
  \      } else {\n\
  \        return inStage_(stage_ - 1, jApp_(str_('splice_'), [() => oeval_(top._1)]))()\n\
  \      }\n\
  \    }\n\
  \    case _Return : return jApp_(str_('Return_'), [() => oeval_(top._1)])()\n\
  \    case _Bind   : return jApp_(str_('Bind_'), [strLit_(top._1), () => oeval_(top._2), jLam_([top._1], () => oeval_(top._3))])()\n\
  \    case _Seq    : return jApp_(str_('Seq_'), [() => oeval_(top._1), () => oeval_(top._2)])()\n\
  \    case _New    : return jApp_(str_('New_'), [() => oeval_(top._1)])()\n\
  \    case _Write  : return jApp_(str_('Write_'), [() => oeval_(top._1), () => oeval_(top._2)])()\n\
  \    case _Read   : return jApp_(str_('Read_'), [() => oeval_(top._1)])()\n\
  \  }\n\
  \}\n\
  \\n\
  \/** @type {(t:Top) => void} */\n\
  \function execTop_(top){\n\
  \  tail_(() => {\n\
  \    switch (top.tag){\n\
  \      case _Let : {\n\
  \        return jLet_(top._1, () => ceval_(top._2), () => {newl_(); execTop_(top._3)})()\n\
  \      }\n\
  \      case _Bind : {\n\
  \        return jLet_(top._1, () => exec_(top._2), () => {newl_(); execTop_(top._3)})()\n\
  \      }\n\
  \      case _Seq : {\n\
  \        return jLet_('_', (() => exec_(top._1)), (() => {newl_(); execTop_(top._2)}))()\n\
  \      }\n\
  \      case _Closure : {\n\
  \        const x    = top._1\n\
  \        const env  = top._2\n\
  \        const arg  = top._3\n\
  \        const body = top._4\n\
  \        const t    = top._5\n\
  \        return jLet_(closeVar_(x), () => jClosure_(env, arg, () => {ceval_(body)}),\n\
  \               jLet_(openVar_(x), () => jClosure_(env, arg, () => inStage_(0, () => oeval_(body))), () =>\n\
  \               execTop_(t)))()\n\
  \      }\n\
  \      case _Body : {\n\
  \        return exec_(top._1)\n\
  \      }\n\
  \    }\n\
  \  })()\n\
  \}\n\
  \\n\
  \/** @type {(t:Top) => void} */\n\
  \function cevalTop_(top){\n\
  \  switch (top.tag){\n\
  \    case _Let : {\n\
  \      return jLet_(top._1, () => ceval_(top._2), () => {newl_(); cevalTop_(top._3)})()\n\
  \    }\n\
  \    case _Bind : {\n\
  \      return jLam_([], () => execTop_(top))()\n\
  \    }\n\
  \    case _Seq : {\n\
  \      return jLam_([], () => execTop_(top))()\n\
  \    }\n\
  \    case _Closure : {\n\
  \      const x    = top._1\n\
  \      const env  = top._2\n\
  \      const arg  = top._3\n\
  \      const body = top._4\n\
  \      const t    = top._5\n\
  \      return jLet_(closeVar_(x), () => jClosure_(env, arg, () => {ceval_(body)}),\n\
  \             jLet_(openVar_(x), () => jClosure_(env, arg, () => inStage_(0, () => oeval_(body))), () =>\n\
  \             cevalTop_(t)))()\n\
  \    }\n\
  \    case _Body:\n\
  \      return ceval_(top._1)\n\
  \  }\n\
  \}\n\
  \\n\
  \/** @type {(t:Top) => void} */\n\
  \function oevalTop_(top){\n\
  \  switch (top.tag){\n\
  \    case _Let: {\n\
  \      if (stage_ === 0){\n\
  \        return jLet_(top._1, () => oeval_(top._2), () => {newl_(); oevalTop_(top._3)})()\n\
  \      } else {\n\
  \        return jApp_(str_('Let_'), [strLit_(top._1), () => oeval_(top._2), jLam_([top._1], () => oevalTop_(top._3))])()\n\
  \      }\n\
  \    }\n\
  \    case _Bind:\n\
  \      return jApp_(str_('Bind_'), [strLit_(top._1), () => oeval_(top._2), jLam_([top._1], () => oevalTop_(top._3))])()\n\
  \    case _Seq:\n\
  \      return jApp_(str_('Seq_'), [() => oeval_(top._1), () => oevalTop_(top._2)])()\n\
  \    case _Closure:\n\
  \      throw new Error('impossible')\n\
  \    case _Body:\n\
  \      return oeval_(top._1)\n\
  \  }\n\
  \}\n\
  \\n\
  \// Splicing in closed evaluation\n\
  \/** @type {(t:Open) => Closed} */\n\
  \function codegenClosed_(t){\n\
  \  const t2_       = runCConv_(t, undefined)\n\
  \  builder_.length = 0\n\
  \  indentation_    = 0\n\
  \  cevalTop_(t2_)\n\
  \  const csp_ = cspArray_\n\
  \  const src_ = builder_.join()\n\
  \  const res_ = eval(src_)\n\
  \  return res_\n\
  \}\n\
  \\n\
  \// Splicing at stage 0 in open evaluation\n\
  \/** @type {(t:Open) => Open} */\n\
  \function codegenOpen_(t){\n\
  \  const t2_       = runCConv_(t, undefined)\n\
  \  builder_.length = 0\n\
  \  indentation_    = 0\n\
  \  stage_          = 0\n\
  \  oevalTop_(t2_)\n\
  \  const csp_ = cspArray_\n\
  \  const src_ = builder_.join()\n\
  \  const res_ = eval(src_)\n\
  \  return res_\n\
  \}\n\
  \\n\
  \/** @type {(t:Open) => Closed} */\n\
  \function codegenExec_(t){\n\
  \  const t2_       = runCConv_(t, undefined)\n\
  \  builder_.length = 0\n\
  \  indentation_    = 0\n\
  \  execTop_(t2_)\n\
  \  const src_ = builder_.join()\n\
  \  const res_ = eval(src_)() // run the resulting action\n\
  \  return res_\n\
  \}\n\
  \\n\
  \// ----------------------------------------------------------------------------------------------------"
