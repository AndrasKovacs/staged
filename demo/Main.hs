
module Main where

import Control.Exception
import System.Environment
import System.Exit

import Common
import Cxt
import Errors
import Evaluation
import Metacontext
import Parser
import Pretty
import Elaboration
import Staging

import qualified Presyntax as P

--------------------------------------------------------------------------------

helpMsg = unlines [
  "usage: 2ltt COMMAND",
  "",
  "All commands read input from stdin. The input must be a single 2LTT",
  "expression at stage 0.",
  "",
  "Commands:",
  "  elab          : print elaboration output",
  "  elab-verbose  : print elaboration output, show metavars, inserted type",
  "                  annotations, implicit lambdas and applications",
  "  stage         : print staging output",
  "  stage-verbose : print staging output, show inserted implicits",
  "  nf            : print normal form",
  "  type          : print normal type of input"
  ]

mainWith :: IO [String] -> IO (P.Tm, String) -> IO ()
mainWith getOpt getRaw = do

  let elab = do
        (!t, !file) <- getRaw
        inferS (emptyCxt (initialPos file)) t S0
          `catch` \e -> displayError file e >> exitSuccess

  reset
  getOpt >>= \case
    ["--help"] -> putStrLn helpMsg
    ["elab"] -> do
      (!t, !a) <- elab
      putStrLn "-- elaboration output"
      putStrLn "------------------------------------------------------------"
      putStrLn ""
      putStrLn $ showTopTm V0 t
    ["elab-verbose"] -> do
      (!t, !a) <- elab
      putStrLn "-- metavariables "
      putStrLn "------------------------------------------------------------"
      putStrLn ""
      displayMetas
      putStrLn "-- elaboration output"
      putStrLn "------------------------------------------------------------"
      putStrLn ""
      putStrLn $ showTopTm V1 t
    ["nf"]   -> do
      (!t, !a) <- elab
      putStrLn $ showTopTm V1 $ nf [] t
      putStrLn "  :"
      putStrLn $ showTopTm V1 $ quote 0 a
    ["type"] -> do
      (!t, !a) <- elab
      putStrLn $ showTopTm V1 $ quote 0 a
    ["stage"] -> do
      (!t, !a) <- elab
      putStrLn $ showTopTm V0 $ stage $ zonk [] 0 t
    ["stage-verbose"] -> do
      (!t, !a) <- elab
      putStrLn $ showTopTm V1 $ stage $ zonk [] 0 t
    _ -> putStrLn helpMsg

main :: IO ()
main = mainWith getArgs parseStdin

-- | Run main with inputs as function arguments.
main' :: String -> String -> IO ()
main' mode src = mainWith (pure [mode]) ((,src) <$> parseString src)
