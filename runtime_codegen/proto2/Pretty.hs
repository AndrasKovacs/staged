
module Pretty (prettyTm, showTm0, displayMetas) where

import Control.Monad
import Data.IORef
import Text.Printf

import qualified Data.IntMap.Strict as IM

import Common
import Evaluation
import ElabState
import Syntax
import Cxt.Type

--------------------------------------------------------------------------------

fresh :: [Name] -> Name -> Name
fresh ns "_" = "_"
fresh ns x | elem x ns = fresh ns (x ++ "'")
           | otherwise = x

-- printing precedences
-- atomp   = 6  :: Int -- U, var
splicep = 5  :: Int -- splice
projp   = 4  :: Int -- field proj
appp    = 3  :: Int -- application
pip     = 2  :: Int -- pi
letp    = 1  :: Int -- let, lambda
tupp    = 0  :: Int -- record

-- Wrap in parens if expression precedence is lower than
-- enclosing expression precedence.
par :: Int -> Int -> ShowS -> ShowS
par p p' = showParen (p' < p)

-- Newline then indent the current indentation level
newl :: Int -> ShowS
newl i acc = '\n' : replicate i ' ' ++ acc

lamNewl :: Tm -> Bool
lamNewl = \case
  Bind{}  -> True
  Let{}   -> True
  Quote{} -> True
  _       -> False

prettyTm :: Int -> Int -> [Name] -> Tm -> ShowS
prettyTm prec i = goTop prec i where

  bracket :: ShowS -> ShowS
  bracket ss = ('{':).ss.('}':)

  piBind ns i x Expl a = showParen True ((x++) . (" : "++) . go tupp i ns a)
  piBind ns i x Impl a = bracket        ((x++) . (" : "++) . go tupp i ns a)

  lamBind x Impl = bracket (x++)
  lamBind x Expl = (x++)

  goPr :: Int -> Int -> [Name] -> [Name] -> Tm -> Pruning -> ShowS
  goPr p i topNs ns t pr = goPr' p i ns pr (0 :: Int) where
    goPr' p i ns pr x = case (ns, pr) of
      ([]      , []           )  -> go p i topNs t
      (ns :> n , pr :> Just ic ) -> par p appp $ goPr' appp i ns pr (x + 1) . (' ':)
                                   . icit ic bracket id (case n of "_" -> (("@"++show x)++); n -> (n++))
      (ns :> n , pr :> Nothing) -> goPr' appp i ns pr (x + 1)
      _                         -> impossible

  goIx :: [Name] -> Ix -> ShowS
  goIx ns topIx = go ns topIx where
    go []       _ = (("@"++show topIx)++)
    go ("_":ns) 0 = (("@"++show topIx)++)
    go (n:ns)   0 = (n++)
    go (n:ns)   x = go ns (x - 1)

  goCheck :: Int -> Int -> [Name] -> CheckVar -> ShowS
  goCheck p i ns c = case lookupCheck c of
    Unchecked cxt t a m -> go p i ns (appPruning (Meta m) (pruning cxt))
    Checked t           -> go p i ns t

  goRecTy :: Int -> [Name] -> [(Name, Tm)] -> ShowS
  goRecTy i ns = \case
    []        -> id
    [(x, a)]  -> (x++) . (" : "++) . go letp i ns a
    (x, a):fs -> (x++) . (" : "++) . go letp i ns a . (" , "++) . goRecTy i (x:ns) fs

  goRec :: Int -> [Name] -> [(Name, Tm)] -> ShowS
  goRec i ns = \case
    []        -> id
    [(x, t)]  -> (x++) . (" = "++) . go letp i ns t
    (x, t):fs -> (x++) . (" = "++) . go letp i ns t . (" , "++) . goRecTy i ns fs

  go :: Int -> Int -> [Name] -> Tm -> ShowS
  go p i ns = \case
    Var x                     -> goIx ns x
    App t u Expl              -> par p appp $ go appp i ns t . (' ':) . go projp i ns u
    App t u Impl              -> par p appp $ go appp i ns t . (' ':) . bracket (go tupp i ns u)

    Lam (fresh ns -> x) ic t  -> par p tupp $ ("λ "++) . lamBind x ic . goLam (ns:>x) t where
                                   goLam ns (Lam (fresh ns -> x) ic t) =
                                     (' ':) . lamBind x ic . goLam (ns:>x) t
                                   goLam ns t | lamNewl t =
                                     (". "++) . newl (i + 2) . go tupp (i + 2) ns t
                                   goLam ns t =
                                     (". "++) . go tupp i ns t

    U                         -> ("U"++)

    Pi "_" Expl a b           -> par p pip $ go appp i ns a . (" → "++) . go pip i (ns:>"_") b

    Pi (fresh ns -> x) ic a b -> par p pip $ piBind ns i x ic a . goPi (ns:>x) b where
                                   goPi ns (Pi (fresh ns -> x) ic a b)
                                     | x /= "_" = piBind ns i x ic a . goPi (ns:>x) b
                                   goPi ns b = (" → "++) . go pip i ns b

    Let (fresh ns -> x) a t u -> let i' = i + 2 in
                                 par p tupp $ ("let "++) . (x++) . (" : "++) . go tupp i' ns a
                                 . (" = "++). newl i' . go tupp i' ns t . (";"++) . newl i . go tupp i (ns:>x) u

    Meta m                    -> (("?"++show m)++)
    AppPruning t pr           -> goPr p i ns ns t pr
    PostponedCheck c          -> goCheck p i ns c

    Quote t                   -> ('<':) . go tupp i ns t . ('>':)
    Splice t _                -> ('~':) . go splicep i ns t
    Unit                      -> ('⊤':)
    Tt                        -> ("tt"++)
    Seq t u                   -> let i' = i + 2 in
                                 par p tupp $ ("do " ++) .  go tupp i' ns t
                                 . (";"++) . newl i . go tupp i ns u
    Bind (fresh ns -> x) t u  -> let i' = i + 2 in
                                 par p tupp $ ("do " ++) . (x++) . (" ← "++) . go tupp i' ns t
                                 . (";"++) . newl i . go tupp i (ns:>x) u

    Ref                       -> ("Ref"++)
    New                       -> ("new"++)
    Read                      -> ("read"++)
    Write                     -> ("write"++)
    Suc                       -> ("suc"++)
    Eff                       -> ("Eff"++)
    Return                    -> ("return"++)
    Box                       -> ('□':)
    NatElim                   -> ("ℕElim"++)

    Erased msg                -> (msg++)
    Nat                       -> ('ℕ':)
    NatLit n                  -> (show n ++)

    RecTy fs                  -> ("Σ(" ++) . goRecTy i ns fs . (')':)
    Rec fs                    -> ('(':) . goRec i ns fs . (')':)
    Proj t x                  -> par p projp $ go projp i ns t . ('.':) . (x++)


  goTop :: Int -> Int -> [Name] -> Tm -> ShowS
  goTop p i ns = \case
    Seq t u                   -> let i' = i + 2 in
                                 par p tupp $ ("do " ++) .  go tupp i' ns t
                                 . (";\n\n"++) . goTop tupp i ns u
    Bind (fresh ns -> x) t u  -> let i' = i + 2 in
                                 par p tupp $ ("do " ++) . (x++) . (" ← "++) . go tupp i' ns t
                                 . (";\n\n"++) . goTop tupp i (ns:>x) u
    Let (fresh ns -> x) a t u -> let i' = i + 2 in
                                 (x++) . (" : "++) . go tupp i ns a
                                 . (" =\n  "++) . go tupp i' ns t . (";\n\n"++) . goTop tupp i (ns:>x) u
    t                         -> go p i ns t

showTm0 :: Tm -> String
showTm0 t = prettyTm 0 0 [] t []

displayMetas :: IO ()
displayMetas = do
  ms <- readIORef mcxt
  forM_ (IM.toList ms) $ \(m, e) -> case e of
    Unsolved _ _ _ _ _ -> printf "?%s = UNSOLVED;\n"  (show m)
    Solved v a         -> printf "?%s = %s;\n" (show m) (showTm0 $ quote 0 v)
  putStrLn ""
