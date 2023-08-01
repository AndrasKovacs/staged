
{-# language UnliftedNewtypes, UnliftedDatatypes, StandaloneKindSignatures, LambdaCase,
   UnboxedTuples, MagicHash, PatternSynonyms  #-}

module Bench where

import GHC.Exts

type Tm :: UnliftedType
data Tm = Var Int# | App Tm Tm | Lam Tm

type Env :: UnliftedType
data Env = Nil | Cons Env Val

type Val :: UnliftedType
data Val = VVar Int# | VLam Env Tm | VApp Val Val

newtype UBool = UBool Int#

pattern UTrue = UBool 1#
pattern UFalse = UBool 0#

infixl 8 $$
($$) = App

uand :: UBool -> UBool -> UBool
uand UTrue y = y
uand x     y = y

var :: Env -> Int# -> Val
var (Cons _ v) 0# = v
var (Cons e _) x = var e (x -# 1#)
var _          _ = undefined

eval :: Env -> Tm -> Val
eval env = \case
  Var x -> var env x
  App t u -> case eval env t of
    VLam e' t -> eval (Cons e' (eval env u)) t
    t         -> VApp t (eval env u)
  Lam t -> VLam env t

conv :: Int# -> Val -> Val -> UBool
conv x t u = case (# t, u #) of
  (# VVar x, VVar x' #) -> UBool (x ==# x')
  (# VLam e t, VLam e' t' #) ->
    let v = VVar x in
    conv (x +# 1#) (eval (Cons e v) t) (eval (Cons e' v) t')
  (# VApp t u, VApp t' u' #) ->
    uand (conv x t t') (conv x u u')
  (# t, VLam e t' #) -> conv (x +# 1#) t (eval (Cons e (VVar x)) t')
  (# VLam e t, t' #) -> conv (x +# 1#) (eval (Cons e (VVar x)) t) t'
  _                  -> UFalse

let_ :: Tm -> Tm -> Tm
let_ t u = App (Lam t) u
{-# inline let_ #-}

pattern V0 = Var 0#
pattern V1 = Var 1#
pattern V2 = Var 2#
pattern V3 = Var 3#
pattern V4 = Var 4#
pattern V5 = Var 5#
