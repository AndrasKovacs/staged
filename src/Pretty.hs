
module Pretty (showTm0, showTm1, showTm0Top, showTm1Top,
               showVal1', showVal1Top',
               showTm1Top', showTm0Top', showElabError,
               showVal0, showVal1, showVal0Top, showVal1Top) where

import qualified Data.ByteString.Char8 as B
import qualified FlatParse.Stateful as FP
import IO
import Text.Printf

import Common
import Syntax
import Cxt.Fields
import ElabState
import Exceptions


import qualified Values     as V
import qualified Evaluation as Eval
import qualified Presyntax  as P
import qualified UnifyCxt   as Unif

--------------------------------------------------------------------------------

showTm0 :: (HasNames cxt [Name]) => cxt -> Tm0 -> String
showTm0 cxt t = tm0 tmp (cxt^.names) t []

showTm1 :: (HasNames cxt [Name]) => cxt -> Tm1 -> String
showTm1 cxt t = tm1 tmp (cxt^.names) t []

showVal0 :: (HasNames s [Name], HasLvl s Lvl) => s -> V.Val0 -> String
showVal0 cxt t = showTm0 cxt $ Eval.quote0 (cxt^.lvl) UnfoldFlex t

showVal1 :: (HasNames s [Name], HasLvl s Lvl) => s -> V.Val1 -> String
showVal1 cxt t = showTm1 cxt $ Eval.quote1 (cxt^.lvl) UnfoldFlex t

showVal1' :: (HasNames s [Name], HasLvl s Lvl) => s -> V.Val1 -> String
showVal1' cxt t = showTm1 cxt $ Eval.quote1 (cxt^.lvl) UnfoldAll t

showVal1Top' :: V.Val1 -> String
showVal1Top' t = showTm1Top $ Eval.quote1 0 UnfoldAll t

showTm1Top' :: Tm1 -> String
showTm1Top' t = tm1 tmp [] (Eval.quote1 0 UnfoldAll (Eval.eval1 V.Nil t)) []

showTm0Top' :: Tm0 -> String
showTm0Top' t = tm0 tmp [] (Eval.quote0 0 UnfoldAll (Eval.eval0 V.Nil t)) []

showTm0Top :: Tm0 -> String
showTm0Top t = tm0 tmp [] t []

showTm1Top :: Tm1 -> String
showTm1Top t = tm1 tmp [] t []

showVal0Top :: V.Val0 -> String
showVal0Top t = showTm0Top $ Eval.quote0 0 UnfoldFlex t

showVal1Top :: V.Val1 -> String
showVal1Top t = showTm1Top $ Eval.quote1 0 UnfoldFlex t

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

(tmp:pip:subp:addp:mulp:appp:projp:downp:atomp:_) = [(0::Int)..]

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
  FCons x t FNil
    | NEmpty <- x -> tm0 tmp ns t
    | True        -> name x . (" = "++). tm0 tmp ns t
  FCons x t ts
    | NEmpty <- x -> tm0 tmp ns t . (", "++) . recCon0 ns ts
    | True        -> name x . (" = "++). tm0 tmp ns t . (", "++) . recCon0 ns ts

recCon1 :: [Name] -> Fields Tm1 -> ShowS
recCon1 ns = \case
  FNil           -> id
  FCons x t FNil
    | NEmpty <- x -> tm1 tmp ns t
    | True        -> name x . (" = "++). tm1 tmp ns t
  FCons x t ts
    | NEmpty <- x -> tm1 tmp ns t . (", "++) . recCon1 ns ts
    | True        -> name x . (" = "++). tm1 tmp ns t . (", "++) . recCon1 ns ts

rec0 :: [Name] -> Fields Tm1 -> ShowS
rec0 ns = \case
  FNil           -> id
  FCons x t FNil
    | NEmpty <- x -> tm1 tmp ns t
    | True        -> name x . (" : "++). tm1 tmp ns t
  FCons x t ts
    | NEmpty <- x -> tm1 tmp ns t . (", "++) . rec0 ns ts
    | True        -> name x . (" : "++). tm1 tmp ns t . (", "++) . rec0 ns ts

rec1 :: [Name] -> Fields Tm1 -> ShowS
rec1 ns = \case
  FNil -> id
  FCons (fresh ns -> x) t FNil
    | NEmpty <- x -> tm1 tmp ns t
    | True        -> name x . (" : "++). tm1 tmp ns t
  FCons (fresh ns -> x) t ts
    | NEmpty <- x -> tm1 tmp ns t . (", "++) . rec1 (x:ns) ts
    | True        -> name x . (" : "++). tm1 tmp ns t . (", "++) . rec1 (x:ns) ts

-- var :: [Name] -> Ix -> ShowS
-- var ns x = case ns !! coerce x of
--   NName x -> (show x++)
--   NEmpty  -> ("@"++).(show x++)
--   NX      -> impossible

var :: [Name] -> Ix -> ShowS
var ns x | 0 <= x && coerce x < length ns = case ns !! coerce x of
  NName x -> (show x++)
  NEmpty  -> ("@"++).(show x++)
  NX      -> impossible
var ns x = ("@!"++).(show x++)

appPruning :: Tm1 -> Ix -> Pruning -> Tm1
appPruning t ix = \case
  []              -> t
  pr :> PESkip    -> appPruning t (ix + 1) pr
  pr :> PEBind0   -> App1 (appPruning t (ix + 1) pr) (Up (Var0 ix)) Expl
  pr :> PEBind1 i -> App1 (appPruning t (ix + 1) pr) (Var1 ix) i

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

  Down t            -> par p downp (('~':) . tm1 atomp ns t)
  Field0 t NEmpty n -> par p projp $ tm0 projp ns t . ('.':) . (show n++)
  Field0 t x n      -> par p projp $ tm0 projp ns t . ('.':) . name x
  RecCon0 ts        -> bracket (recCon0 ns ts)
  Sub t u           -> par p subp $ tm0 subp ns t . (" - "++) . tm0 addp ns u
  Add t u           -> par p addp $ tm0 addp ns t . (" + "++) . tm0 mulp ns u
  Mul t u           -> par p mulp $ tm0 mulp ns t . (" * "++) . tm0 appp ns u
  App0 t u          -> par p appp $ tm0 appp ns t . (' ':) . tm0 projp ns u
  IntLit n          -> (show n++)
  Wk10 t            -> tm0 p (tail ns) t
  -- Wk10 t       -> par p appp (("_wk_ "++).tm0 p (tail ns) t)

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

  Field1 t NEmpty n -> par p projp $ tm1 projp ns t . ('.':) . (show n++)
  Field1 t x n      -> par p projp $ tm1 projp ns t . ('.':) . name x

  TyCon x        -> topVar x

  DataCon x _    -> topVar x

  AppPruning t pr -> tm1 p ns (appPruning t 0 pr)

  -- Wk11 t         -> par p appp (("_wk_ "++).tm1 p (tail ns) t)
  -- Wk01 t         -> par p appp (("_wk_ "++).tm1 p (tail ns) t)
  Wk11 t         -> tm1 p (tail ns) t
  Wk01 t         -> tm1 p (tail ns) t
  Meta m         -> (("?"++show m)++)

  Int            -> ("Int"++)

--------------------------------------------------------------------------------

solutionEx :: Unif.Cxt -> SolutionEx -> String
solutionEx cxt = \case
  Occurs m       -> printf "Metavariable ?%s occurs in solution candidate" (show m)
  OutOfScope x   -> printf "Variable %s is out of the scope of solution candidate" (showVal1 cxt (V.Var1 x))
  SpineError     -> "Non-variable entry in meta spine"
  NeedExpansion  -> "Projection in meta spine"
  CSFlexSolution -> "IMPOSSIBLE: can't solve metavar in flexible unification state"

unifyEx :: Unif.Cxt -> UnifyEx -> String
unifyEx cxt = \case
  Unify0 t u ->
    printf "Can't unify\n\n  %s\n\nwith\n\n  %s\n" (showVal0 cxt t) (showVal0 cxt u)
  Unify1 t u ->
    printf "Can't unify\n\n  %s\n\nwith\n\n  %s\n" (showVal1 cxt t) (showVal1 cxt u)
  UnifyFieldName x x' ->
    printf "Record field name mismatch: can't unify\n\n  \"%s\"\n\nwith\n\n  \"%s\"\n" (show x) (show x')
  SolutionError t u e ->
    printf "%s\n\nwhile trying to unify\n\n  %s\n\nwith\n\n  %s\n"
      (solutionEx cxt e) (showVal1' cxt t) (showVal1' cxt u)

unifyInner :: UnifyInner -> String
unifyInner (UnifyInner cxt e) = unifyEx cxt e

elabEx :: Unif.Cxt -> ElabEx -> String
elabEx cxt = \case
  UnifyOuter t u e ->
    printf "%s\nwhile trying to unify\n\n  %s\n\nwith\n\n  %s\n"
      (unifyInner e)  (showVal1 cxt t) (showVal1 cxt u)
  NameNotInScope x ->
    "Name not in scope: " ++ show x ++ "\n"
  NoSuchFieldName ty x ->
    "Record has no field with name " ++ show x ++ "\n\n" ++
    "inferred record type:\n\n  " ++ showTm1 cxt ty ++ "\n"
  NoSuchFieldIx ty ix ->
    "Record has no field with index " ++ show ix ++ "\n\n" ++
    "inferred record type:\n\n  " ++ showTm1 cxt ty ++ "\n"
  NegativeFieldIndex ->
    "Invalid negative record field index\n"
  NoSuchArgument x ->
    "Function has no named argument with name " ++ show x ++ "\n"
  IcitMismatch i i' ->
    printf "Icit mismatch: expected %s, inferred: %s\n" (show i) (show i')
  NoImplicitLam0 ->
    "Implicit lambda expression are not supported at runtime stage\n"
  ExpectedVal ty ->
    printf "Expected a value type, inferred:\n\n  %s\n" (showTm1 cxt ty)
  FieldNameMismatch x x' ->
    printf "Record field name mismatch: expected %s, got %s\n" (show x) (show x')
  NoNamedLambdaInference ->
    "Can't infer type for named argument in lambda expression.\n"++
    "Provide a type annotation for the lambda\n"
  CantInfer ->
    "Can't infer type for expression\n"
  ExpectedNonEmptyRec ->
    "Expected non-empty record type\n"
  ExpectedEmptyRec ->
    "Expected empty record type\n"
  ExpectedEmptyRecCon ->
    "Expected empty record\n"
  ExpectedNonEmptyRecCon ->
    "Expected non-empty record\n"
  ExpectedType ty ->
    printf "Expected a type, inferred:\n\n  %s\n" (showTm1 cxt ty)
  CantInferTuple ->
    "Can't infer type for tuple at unknown stage\n"
  ExpectedRecord ty ->
    printf "Expected record type, inferred:\n\n %s\n" (showVal1 cxt ty)
  ExpectedRuntimeType ty ->
    printf "Expected runtime type, inferred:\n\n %s\n" (showTm1 cxt ty)
  CantInferRec1 ->
    "Can't infer type for meta stage record\n"

showElabError :: B.ByteString -> ElabError -> String
showElabError src (ElabError cxt t e) = let
  ls         = FP.lines src
  Span pos _ = P.span t
  ns         = cxt^.names
  [(l, c)]   = FP.posLineCols src [pos]
  line       = if l < length ls then ls !! l else ""
  linum      = show l
  lpad       = map (const ' ') linum
  in show l ++ ":" ++ show c ++ ":\n" ++
     lpad   ++ "|\n" ++
     linum  ++ "| " ++ line ++ "\n" ++
     lpad   ++ "| " ++ replicate c ' ' ++ "^\n" ++
     elabEx cxt e
