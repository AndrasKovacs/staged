
module Pretty (prettyTm, showTm0, displayMetas) where

import Control.Monad
import Data.IORef
import Text.Printf

import qualified Data.IntMap.Strict as IM

import Common
import Evaluation
import Metacontext
import Syntax
import Cxt.Type

--------------------------------------------------------------------------------

fresh :: [Name] -> Name -> Name
fresh ns "_" = "_"
fresh ns x | elem x ns = fresh ns (x ++ "'")
           | otherwise = x

-- printing precedences
atomp = 3  :: Int -- U, var
appp  = 2  :: Int -- application
pip   = 1  :: Int -- pi
letp  = 0  :: Int -- let, lambda

-- Wrap in parens if expression precedence is lower than
-- enclosing expression precedence.
par :: Int -> Int -> ShowS -> ShowS
par p p' = showParen (p' < p)

-- Newline then indent the current indentation level
newl :: Int -> ShowS
newl i acc = '\n' : replicate i ' ' ++ acc

isBindLet :: Tm -> Bool
isBindLet = \case
  Bind{} -> True
  Let{}  -> True
  _      -> False

prettyTm :: Int -> Int -> [Name] -> Tm -> ShowS
prettyTm prec i = goTop prec i where

  bracket :: ShowS -> ShowS
  bracket ss = ('{':).ss.('}':)

  piBind ns i x Expl a = showParen True ((x++) . (" : "++) . go letp i ns a)
  piBind ns i x Impl a = bracket        ((x++) . (" : "++) . go letp i ns a)

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

  go :: Int -> Int -> [Name] -> Tm -> ShowS
  go p i ns = \case
    Var x                     -> goIx ns x

    App t u Expl              -> par p appp $ go appp i ns t . (' ':) . go atomp i ns u
    App t u Impl              -> par p appp $ go appp i ns t . (' ':) . bracket (go letp i ns u)

    Lam (fresh ns -> x) ic t  -> par p letp $ ("λ "++) . lamBind x ic . goLam (ns:>x) t where
                                   goLam ns (Lam (fresh ns -> x) ic t) =
                                     (' ':) . lamBind x ic . goLam (ns:>x) t
                                   goLam ns t | isBindLet t =
                                     (". "++) . newl (i + 2) . go letp (i + 2) ns t
                                   goLam ns t =
                                     (". "++) . go letp i ns t

    U                         -> ("U"++)

    Pi "_" Expl a b           -> par p pip $ go appp i ns a . (" → "++) . go pip i (ns:>"_") b

    Pi (fresh ns -> x) ic a b -> par p pip $ piBind ns i x ic a . goPi (ns:>x) b where
                                   goPi ns (Pi (fresh ns -> x) ic a b)
                                     | x /= "_" = piBind ns i x ic a . goPi (ns:>x) b
                                   goPi ns b = (" → "++) . go pip i ns b

    Let (fresh ns -> x) a t u -> let i' = i + 2 in
                                 par p letp $ ("let "++) . (x++) . (" : "++) . go letp i' ns a
                                 . (" = "++). newl i' . go letp i' ns t . (";"++) . newl i . go letp i (ns:>x) u

    Meta m                    -> (("?"++show m)++)
    AppPruning t pr           -> goPr p i ns ns t pr
    PostponedCheck c          -> goCheck p i ns c

    Box t                     -> par p appp $ ('□':) . (' ':) . go atomp i ns t
    Quote t                   -> ('<':) . go letp i ns t . ('>':)
    Splice t                  -> ('~':) . go atomp i ns t
    Unit                      -> ('⊤':)
    Tt                        -> ("tt"++)
    Eff t                     -> par p appp $ ("Eff "++) . go atomp i ns t
    Return t                  -> par p appp $ ("return "++) . go atomp i ns t
    ConstBind t u             -> let i' = i + 2 in
                                 par p letp $ ("do " ++) .  go letp i' ns t
                                 . (";"++) . newl i . go letp i ns u
    Bind (fresh ns -> x) t u  -> let i' = i + 2 in
                                 par p letp $ ("do " ++) . (x++) . (" ← "++) . go letp i' ns t
                                 . (";"++) . newl i . go letp i (ns:>x) u

    Ref t                     -> par p appp $ ("Ref "++) . go atomp i ns t
    New t                     -> par p appp $ ("new "++) . go atomp i ns t
    Read t                    -> par p appp $ ("read "++) . go atomp i ns t
    Write t u                 -> par p appp $ ("write "++) . go atomp i ns t . (' ':) . go atomp i ns u

  goTop :: Int -> Int -> [Name] -> Tm -> ShowS
  goTop p i ns = \case
    ConstBind t u             -> let i' = i + 2 in
                                 par p letp $ ("do " ++) .  go letp i' ns t
                                 . (";\n\n"++) . goTop letp i ns u
    Bind (fresh ns -> x) t u  -> let i' = i + 2 in
                                 par p letp $ ("do " ++) . (x++) . (" ← "++) . go letp i' ns t
                                 . (";\n\n"++) . goTop letp i (ns:>x) u
    Let (fresh ns -> x) a t u -> let i' = i + 2 in
                                 par p letp $ ("let "++) . (x++) . (" : "++) . go letp i ns a
                                 . (" =\n  "++) . go letp i' ns t . (";\n\n"++) . goTop letp i (ns:>x) u
    t                         -> go p i ns t

showTm0 :: Tm -> String
showTm0 t = prettyTm 0 0 [] t []
-- showTm0 = show

displayMetas :: IO ()
displayMetas = do
  ms <- readIORef mcxt
  forM_ (IM.toList ms) $ \(m, e) -> case e of
    Unsolved _ a -> printf "let ?%s : %s = ?;\n"  (show m) (showTm0 $ quote 0 a)
    Solved v a   -> printf "let ?%s : %s = %s;\n" (show m) (showTm0 $ quote 0 a) (showTm0 $ quote 0 v)
  putStrLn ""
