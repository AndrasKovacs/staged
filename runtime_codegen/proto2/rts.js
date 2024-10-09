
"use strict";

// closed value representation
//   CLam is a pair {_1: Closed -> Closed, _2: Lvl -> Open -> Open}
//   CAction is immediately a function with type () -> Closed
//   CRef is an object {_1 : Closed}
//   CQuote is immediately an Open value
//   CErased is undefined

// open values are a proper ADT
// open value constructor tags:
const _Var    = 0
const _Let    = 1
const _Lam    = 2
const _App    = 3
const _Erased = 4
const _Quote  = 5
const _Splice = 6
const _Return = 7
const _Bind   = 8
const _Seq    = 9
const _New    = 10
const _Write  = 11
const _Read   = 12
const _Closed = 13

const Var_    = (x)       => {return {tag: _Var, _1: x}}
const Let_    = (x, t, u) => {return {tag: _Let, _1: x, _2: t, _3: u}}
const Lam_    = (x, t)    => {return {tag: _Lam, _1: x, _2: t}}
const App_    = (t, u)    => {return {tag: _App, _1: t, _2: t}}
const Erased_ = ()        => {return {tag: _Erased}}
const Quote_  = (t)       => {return {tag: _Quote, _1: t}}
const Splice_ = (t)       => {return {tag: _Splice, _1: t}}
const Return_ = (t)       => {return {tag: _Return, _1: t}}
const Bind_   = (x, t, u) => {return {tag: _Bind, _1: x, _2: t, _3: u}}
const Seq_    = (t, u)    => {return {tag: _Seq, _1: t, _2: u}}
const New_    = (t)       => {return {tag: _New, _1: t}}
const Write_  = (t, u)    => {return {tag: _Write, _1: t, _2: u}}
const Read_   = (t)       => {return {tag: _Read, _1: t}}
const Closed_ = (t)       => {return {tag: _Closed, _1: t}}
