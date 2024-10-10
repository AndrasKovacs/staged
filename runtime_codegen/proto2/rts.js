
'use strict';

function impossible(){
    throw new Error('Impossible')
}

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

function openApp_(t, u) {
    if (t.tag == _Closed) {
        if (u.tag == _Closed) {
            return Closed_(t._1._1(u._1))
        } else {
            return t._1._2(u)
        }
    } else if (t.tag == _Lam) {
        return t._1(u)
    } else {
        impossible()
    }
}

function openSplice0_(t) {
    throw new Error('code generation not implemented')
}

function openSplice_(t) {
    if (t.tag == _Quote){
        return t._1
    } else {
        return Splice_(t)
    }
}

function closedExec_(t){
    throw new Error('code generation not implemented')
}

function closedEval_(t){
    throw new Error('code generation not implemented')
}

const _0c = (r) => {return (f) => {return () => {const a = r._1;r._1 = (f)._1(a)}}};
const _0o = (r) => {return (f) => {return Bind_('a', Read_(r), (a) => {return Write_(r, openApp_(f, a))})}};
const _1c = (r) => {return { _1 : _0c(r), _2 : _0o(r)}};
const _1o = (r) => {return _0o(r)};
const _2c = (A) => {return { _1 : _1c, _2 : _1o}};
const _2o = (A) => {return _1o};
const modify = { _1 : _2c, _2 : _2o};
return modify
