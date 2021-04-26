
module Pretty (showTm0, showTm1, showTm0Top, showTm1Top, showEx,
               showVal1', showVal1Top',
               showTm1Top', showTm0Top',
               showVal0, showVal1, showVal0Top, showVal1Top) where

import qualified Data.ByteString.Char8 as B
import IO
import Text.Printf

import Common
import Syntax
import Cxt
import ElabState
import InCxt
import Exceptions
import qualified Values as V
import qualified Evaluation as Eval

--------------------------------------------------------------------------------

showTm0 :: Cxt -> Tm0 -> String
showTm0 cxt t = tm0 tmp (localNames (_locals cxt)) t []

showTm1 :: Cxt -> Tm1 -> String
showTm1 cxt t = tm1 tmp (localNames (_locals cxt)) t []

showVal0 :: Cxt -> V.Val0 -> String
showVal0 cxt t = showTm0 cxt $ quote0 cxt t

showVal1 :: Cxt -> V.Val1 -> String
showVal1 cxt t = showTm1 cxt $ quote1 cxt t

showVal1' :: Cxt -> V.Val1 -> String
showVal1' cxt t = showTm1 cxt $ Eval.quote1 (_lvl cxt) DoUnfold t

showVal1Top' :: V.Val1 -> String
showVal1Top' t = showTm1Top $ Eval.quote1 0 DoUnfold t

showTm1Top' :: Tm1 -> String
showTm1Top' t = tm1 tmp [] (Eval.quote1 0 DoUnfold (Eval.eval1 V.Nil t)) []

showTm0Top' :: Tm0 -> String
showTm0Top' t = tm0 tmp [] (Eval.quote0 0 DoUnfold (Eval.eval0 V.Nil t)) []

showTm0Top :: Tm0 -> String
showTm0Top t = tm0 tmp [] t []

showTm1Top :: Tm1 -> String
showTm1Top t = tm1 tmp [] t []

showVal0Top :: V.Val0 -> String
showVal0Top t = showTm0Top $ Eval.quote0 0 DontUnfold t

showVal1Top :: V.Val1 -> String
showVal1Top t = showTm1Top $ Eval.quote1 0 DontUnfold t

--------------------------------------------------------------------------------

-- | Wrap in parens if expression precedence is lower than
--   enclosing expression precedence.
par :: Int -> Int -> ShowS -> ShowS
par p p' = showParen (p' < p)

{-
Precedences:
  tmp   : λ, let, case
  pip   : function types
  subp  : -
  addp  : +
  mulp  : *
  appp  : function application
  projp : projections
  atomp : atoms
-}

(tmp:pip:subp:addp:mulp:appp:projp:atomp:_) = [(0::Int)..]

fresh :: [Name] -> Name -> Name
fresh ns NX        = fresh ns (NName "x")
fresh ns NEmpty    = NEmpty
fresh ns (NName x) | elem (NName x) ns = go (1 :: Int) where
  go :: Int -> Name
  go n =
    let x' = NName (x <> coerce (B.pack (show n)))
    in if elem x' ns then go (n + 1) else x'
fresh ns x = x

brace :: ShowS -> ShowS
brace s = ('{':).s.('}':)

bracket :: ShowS -> ShowS
bracket s = ('[':).s.(']':)

name :: Name -> ShowS
name x = (show x++)

topVar :: Lvl -> ShowS
topVar x = runIO $ readTop x >>= \case
  TEDef0 _ _ _ _ _ x _  -> pure $! name x
  TEDef1 _ _ _ _ x _    -> pure $! name x
  TETyCon _ _ _ x _     -> pure $! name x
  TEDataCon _ _ _ _ x _ -> pure $! name x

lamBind :: Name -> Icit -> ShowS
lamBind x i = icit i brace id (name x)

recCon0 :: [Name] -> Fields Tm0 -> ShowS
recCon0 ns = \case
  FNil           -> id
  FCons x t FNil -> name x . (" = "++). tm0 tmp ns t
  FCons x t ts   -> name x . (" = "++). tm0 tmp ns t . (", "++) . recCon0 ns ts

recCon1 :: [Name] -> Fields Tm1 -> ShowS
recCon1 ns = \case
  FNil           -> id
  FCons x t FNil -> name x . (" = "++). tm1 tmp ns t
  FCons x t ts   -> name x . (" = "++). tm1 tmp ns t . (", "++) . recCon1 ns ts

rec0 :: [Name] -> Fields Tm1 -> ShowS
rec0 ns = \case
  FNil           -> id
  FCons x t FNil -> name x . (" : "++). tm1 tmp ns t
  FCons x t ts   -> name x . (" : "++). tm1 tmp ns t . (", "++) . rec0 ns ts

rec1 :: [Name] -> Fields Tm1 -> ShowS
rec1 ns = \case
  FNil -> id
  FCons (fresh ns -> x) t FNil ->
    name x . (" : "++). tm1 tmp ns t
  FCons (fresh ns -> x) t ts   ->
    name x . (" : "++). tm1 tmp ns t . (", "++) . rec1 (x:ns) ts

var :: [Name] -> Ix -> ShowS
var ns x = case ns !! coerce x of
  NName x -> (show x++)
  NEmpty  -> ("@"++).(show x++)
  NX      -> impossible

inserted :: Int -> [Name] -> MetaVar -> Ix -> Locals -> ShowS
inserted p ns m ix = \case
  Empty           -> (("?"++show m)++)
  Define ls _ _ _ -> inserted p ns m (ix + 1) ls
  Bind0 ls _ _ _  -> par p appp (inserted appp ns m (ix + 1) ls . (' ':) . var ns ix)
  Bind1 ls _ _    -> par p appp (inserted appp ns m (ix + 1) ls . (' ':) . var ns ix)

tm0 :: Int -> [Name] -> Tm0 -> ShowS
tm0 p ns = \case
  Var0 x -> var ns x
  Top0 x -> topVar x

  Let0 (fresh ns -> x) a t u ->
    par p tmp $
      ("let "++) . name x . (" : "++). tm1 tmp ns a . (" = "++)
      . tm0 tmp ns t . (" in "++) . tm0 tmp (x:ns) u

  Lam0 (fresh ns -> x) a t ->
    par p tmp $ ("λ "++) . lamBind x Expl . goLam (x:ns) t where
      goLam ns (Lam0 (fresh ns -> x) a t) =
        (' ':) . lamBind x Expl . goLam (x:ns) t
      goLam ns t =
        (". "++) . tm0 tmp ns t

  Case{} -> ("CASE_TODO"++)
  Fix{}  -> ("FIX_DEPRECATED"++)

  Down t       -> par p appp (('~':) . tm1 projp ns t)
  Field0 t x n -> par p projp $ tm0 projp ns t . ('.':) . name x
  RecCon0 ts   -> bracket (recCon0 ns ts)
  Sub t u      -> par p subp $ tm0 subp ns t . (" - "++) . tm0 addp ns u
  Add t u      -> par p addp $ tm0 addp ns t . (" + "++) . tm0 mulp ns u
  Mul t u      -> par p mulp $ tm0 mulp ns t . (" * "++) . tm0 appp ns u
  App0 t u     -> par p appp $ tm0 appp ns t . (' ':) . tm0 projp ns u
  IntLit n     -> (show n++)
  Wk10 t       -> par p appp (("_wk_ "++).tm0 p (tail ns) t)

piBind ns x Expl a = showParen True (name x . (" : "++) . tm1 tmp ns a)
piBind ns x Impl a = brace          (name x . (" : "++) . tm1 tmp ns a)

tm1 :: Int -> [Name] -> Tm1 -> ShowS
tm1 p ns = \case
  Var1 x -> var ns x
  Top1 x -> topVar x

  Let1 (fresh ns -> x) a t u ->
    par p tmp $
      ("let "++) . name x . (" : "++). tm1 tmp ns a . (" = "++)
      . tm1 tmp ns t . (" in "++) . tm1 tmp (x:ns) u

  Pi NEmpty Expl a b        -> par p pip $ tm1 appp ns a . (" → "++) . tm1 pip (NEmpty:ns) b

  Pi (fresh ns -> x) i a b  -> par p pip $ piBind ns x i a . goPi (x:ns) b where
                                 goPi ns (Pi (fresh ns -> x) i a b)
                                   | x /= NEmpty = piBind ns x i a . goPi (x:ns) b
                                 goPi ns b = (" → "++) . tm1 pip ns b

  Lam1 (fresh ns -> x) i a t -> par p tmp $ ("λ "++) . lamBind x i . goLam (x:ns) t where
                                 goLam ns (Lam1 (fresh ns -> x) i a t) =
                                   (' ':) . lamBind x i . goLam (x:ns) t
                                 goLam ns t =
                                   (". "++) . tm1 tmp ns t

  App1 t u Expl  -> par p appp $ tm1 appp ns t . (' ':) . tm1 projp ns u
  App1 t u Impl  -> par p appp $ tm1 appp ns t . (' ':) . brace (tm1 tmp ns u)

  Fun a b _      -> par p pip $ tm1 subp ns a . (" → "++) . tm1 pip ns b

  U1             -> ("U1"++)
  U0 cv          -> par p appp $ ("U0 "++) . tm1 projp ns cv
  CV             -> ("CV"++)
  Comp           -> ("Comp"++)
  Val            -> ("Val"++)

  -- Lift cv a      -> par p appp $ ("^ "++). tm1 projp ns cv . (" "++) . tm1 projp ns a
  Lift cv a      -> par p appp $ ("^"++).tm1 projp ns a

  Up t           -> ('<':). tm0 tmp ns t. ('>':)

  Rec0 ts        -> bracket (rec0 ns ts)
  Rec1 ts        -> bracket (rec1 ns ts)
  RecCon1 ts     -> bracket (recCon1 ns ts)

  Field1 t x n   -> par p projp $ tm1 projp ns t . ('.':) . name x

  TyCon x        -> topVar x

  DataCon x _    -> topVar x

  Inserted m ls  -> inserted p ns m 0 ls

  Wk11 t         -> par p appp (("_wk_ "++).tm1 p (tail ns) t)
  Wk01 t         -> par p appp (("_wk_ "++).tm1 p (tail ns) t)
  Meta m         -> (("?"++show m)++)

  Int            -> ("Int"++)



--------------------------------------------------------------------------------

-- TODO
showEx :: [Name] -> Ex -> String
showEx ns = \case
  UnifyError0 t u ->
    printf "Can't unify\n\n  %s\n\nwith\n\n  %s\n"
      (tm0 tmp ns t [])
      (tm0 tmp ns u [])
  UnifyError1 t u ->
    printf "Can't unify\n\n  %s\n\nwith\n\n  %s\n"
      (tm1 tmp ns t [])
      (tm1 tmp ns u [])
  e -> show e
