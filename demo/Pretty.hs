
module Pretty (showTm, showTm0, showTopTm, displayMetas) where

import Control.Monad
import Data.IORef
import Text.Printf

import qualified Data.IntMap.Strict as IM

import Common
import Evaluation
import Metacontext
import Syntax

--------------------------------------------------------------------------------

fresh :: [Name] -> Name -> Name
fresh ns "_" = "_"
fresh ns x   = if elem x ns then go (1::Int) else x
  where go n | elem (x ++ show n) ns = go (n + 1)
             | True                  = x ++ show n


-- printing precedences
atomp = 3  :: Int -- U, var
appp  = 2  :: Int -- application
pip   = 1  :: Int -- pi
letp  = 0  :: Int -- let, lambda

-- Wrap in parens if expression precedence is lower than
-- enclosing expression precedence.
par :: Int -> Int -> ShowS -> ShowS
par p p' = showParen (p' < p)

braces :: ShowS -> ShowS
braces s = ('{':).s.('}':)

assign :: Stage -> ShowS
assign = \case S0 -> (":="++); _ -> ("="++)

lamBind x Impl = braces (x++)
lamBind x Expl = (x++)

goIx :: [Name] -> Ix -> ShowS
goIx ns topX = go ns topX topX where
  go ("_":ns) 0 wkx = (("@"++show wkx)++)
  go ("*":ns) x wkx = go ns (x + 1) (wkx + 1)
  go (n:ns)   0 _   = (n++)
  go (n:ns)   x wkx = go ns (x - 1) wkx
  go _        _ _   = impossible

goTm :: Verbosity -> Int -> [Name] -> Tm -> ShowS
goTm v = go where

  piBind ns x Expl a = ('(':) . (x++) . (" : "++) . go letp ns a .(')':)
  piBind ns x Impl a = braces  ((x++) . (" : "++) . go letp ns a)

  verbose o s1 s2 = if o <= v then s1 else s2

  goPr :: Int -> [Name] -> [Name] -> Tm -> Pruning -> ShowS
  goPr p topNs ns t pr = goPr' p ns pr (0 :: Int) where
    goPr' p ns pr x = case (ns, pr) of
      ([]      , []           ) -> go p topNs t
      (ns :> n , pr :> Just i ) -> par p appp $ goPr' appp ns pr (x + 1) . (' ':)
                                   . icit i braces id (case n of "_" -> (("@"++show x)++); n -> (n++))
      (ns :> n , pr :> Nothing) -> goPr' appp ns pr (x + 1)
      _                         -> impossible

  go :: Int -> [Name] -> Tm -> ShowS
  go p ns = \case
    Var x                       -> goIx ns x

    App t u Expl _              -> par p appp $ go appp ns t . (' ':) . go atomp ns u
    App t u Impl o              -> verbose o
                                     (par p appp $ go appp ns t . (' ':) . braces (go letp ns u))
                                     (go p ns t)

    Lam (fresh ns -> x) i _ t o -> let goLam ns (Lam (fresh ns -> x) i _ t o) =
                                         verbose o ((' ':) . lamBind x i) id . goLam (ns:>x) t
                                       goLam ns t =
                                         (". "++) . go letp ns t
                                   in verbose o
                                      (par p letp $ ("λ "++) . lamBind x i . goLam (ns:>x) t)
                                      (go p (ns:>x) t)

    U S0                        -> ("U0"++)
    U S1                        -> ("U1"++)

    Pi "_" Expl a b             -> par p pip $ go appp ns a . (" → "++) . go pip (ns:>"_") b

    Pi (fresh ns -> x) i a b    -> par p pip $ piBind ns x i a . goPi (ns:>x) b where
                                     goPi ns (Pi (fresh ns -> x) i a b)
                                       | x /= "_" = piBind ns x i a . goPi (ns:>x) b
                                     goPi ns b = (" → "++) . go pip ns b

    Let s (fresh ns -> x) a t u -> par p letp $ ("let "++) . (x++) . (" : "++) . go letp ns a
                                   . (" "++).assign s .(' ':). go letp ns t . ("; "++)
                                   . go letp (ns:>x) u


    Meta m                      -> verbose V1 (("?"++show m)++) ('_':)
    AppPruning t pr             -> goPr p ns ns t pr

    Quote t                     -> ('<':).go letp ns t.('>':)
    Splice t                    -> ('[':).go letp ns t.(']':)
    Lift a                      -> par p appp (("^"++) . go atomp ns a)
    Wk t                        -> go p (ns:>"*") t
    -- Wk t                        -> ("wk("++).go p (ns:>"*") t.(")"++)

    InsertedMeta m pr           -> verbose V1 (("?"++show m++"(..)")++) ('_':)

goTopTm :: Verbosity -> String -> String -> [Name] -> Tm -> ShowS
goTopTm v = top where

  go = goTm v

  top :: String -> String -> [Name] -> Tm -> ShowS
  top pre post ns (Lam (fresh ns -> x) i a t o) =
      (pre++)
    . icit i braces (showParen True) (
           ((if null x then "_" else x)++) . (" : "++) . go letp ns a)
    . top "\n " ".\n\n" (x:ns) t
  top pre post ns (Let st (fresh ns -> x) a t u) =
      (post++)
    . ("let "++).(x++).(" : "++). go letp ns a . ("\n  "++).assign st.(' ':)
    . go letp ns t . (";\n\n"++) . top "\nλ" "" (x:ns) u
  top pre post ns (Splice t) =
      (post++)
    . ("["++) . top "\nλ" "" ns t . ("]"++)
  top pre post ns (Quote t) =
      (post++)
    . ("<"++) . top "\nλ" "" ns t . (">"++)
  top pre post ns t =
    (post++) . go letp ns t

showTm :: Verbosity -> Int -> [Name] -> Tm -> String
showTm v p ns t = goTm v p ns t []

showTm0 :: Verbosity -> Tm -> String
showTm0 v t = goTm v 0 [] t []

showTopTm :: Verbosity -> Tm -> String
showTopTm v t = goTopTm v "λ" "" [] t []

displayMetas :: IO ()
displayMetas = do
  ms <- readIORef mcxt
  forM_ (IM.toList ms) $ \(m, e) -> case e of
    Unsolved a _ -> printf "?%s = ?;\n"  (show m)
    Solved v a _ -> printf "?%s = %s;\n" (show m) (showTm0 V1 $ quote 0 v)
  putStrLn ""
