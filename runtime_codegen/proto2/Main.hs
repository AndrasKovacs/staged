
module Main where

import Control.Exception
import System.Environment
import System.Exit

import Errors
import Evaluation
import Metacontext
import Parser
import Pretty
import Elaboration
import Zonk

import qualified Presyntax as P

--------------------------------------------------------------------------------

helpMsg = unlines [
  "usage: rtcg [--help|nf|type]",
  "  --help    : display this message",
  "  elab      : read & elaborate expression from stdin",
  "  zonk      : read & elaborate expression from stdin, zonk output",
  "  nf        : read & typecheck expression from stdin, print its normal form and type",
  "  type      : read & typecheck expression from stdin, print its type"]

mainWith :: IO [String] -> IO (P.Tm, String) -> IO ()
mainWith getOpt getRaw = do

  let handleErr file act = act `catch` \e -> displayError file e >> exitSuccess

  let elab = do
        (t, file) <- getRaw
        res <- handleErr file (inferTop file t)
        pure (res, file)

  reset
  getOpt >>= \case
    ["--help"] -> putStrLn helpMsg
    ["nf"]   -> do
      ((t, a), file) <- elab
      putStrLn $ showTm0 $ nf [] t
      putStrLn "  :"
      putStrLn $ showTm0 $ quote 0 a
    ["type"] -> do
      ((t, a), file) <- elab
      putStrLn $ showTm0 $ quote 0 a
    ["elab"] -> do
      ((t, a), file) <- elab
      displayMetas
      putStrLn (replicate 80 '-' ++ "\n")
      putStrLn $ showTm0 t
    ["zonk"] -> do
      ((t, a), file) <- elab
      t <- unzonk <$> handleErr file (zonk0 t)
      putStrLn $ showTm0 t
    _ -> putStrLn helpMsg

main :: IO ()
main = mainWith getArgs parseStdin

-- | Run main with inputs as function arguments.
main' :: String -> String -> IO ()
main' mode src = mainWith (pure [mode]) ((,src) <$> parseString src)
