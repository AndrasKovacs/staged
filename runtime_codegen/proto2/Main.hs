
module Main where

import Control.Exception
import System.Environment
import System.Exit

import Errors
import Evaluation
import ElabState
import Parser
import Pretty
import StringBuilder
import Elaboration
import Zonk
import qualified Interpreter
import qualified Compiler

import qualified Presyntax as P

--------------------------------------------------------------------------------

helpMsg = unlines [
  "all input is read from stdin",
  "usage: rtcg [--help|elab|zonk|interp|compile|run|nf|type]",
  "  --help    : display this message",
  "  elab      : print elaboration output",
  "  zonk      : print zonking & erasure output",
  "  interp    : run program in interpreter",
  "  compile   : print compiled javascript output",
  "  run       : compile to javascript and run",
  "  nf        : print beta-normal form and beta-normal type",
  "  type      : print type of program"]

mainWith :: IO [String] -> IO (P.Tm, String) -> IO ()
mainWith getOpt getRaw = do

  let handleErr file act = act `catch` \e -> displayError e >> exitSuccess

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
    ["compile"] -> do
      ((t, a), file) <- elab
      t <- handleErr file (zonk0 t)
      let out = build $ Compiler.genTop t
      putStrLn out
      writeFile "out.js" out
    ["interp"] -> do
      ((t, a), file) <- elab
      t <- handleErr file (zonk0 t)
      res <- Interpreter.execTop (castTm t)
      res <- pure $ unzonk $ Interpreter.readBackClosed res
      putStrLn "RESULT:"
      putStrLn $ showTm0 res
    ["run"] -> do
      undefined
      -- ((t, a), file) <- elab
      -- t <- handleErr file (zonk0 t)
      -- let out = Compiler.genTop t
      -- putStrLn "\nOUT:"
      -- putStrLn (build out)
    _ -> putStrLn helpMsg

main :: IO ()
main = mainWith getArgs parseStdin

-- | Run main with inputs as function arguments.
main' :: String -> String -> IO ()
main' mode src = mainWith (pure [mode]) ((,src) <$> parseString src)

test :: String ->  IO ()
test cmd = do
  src <- readFile "test.rtcg"
  main' cmd src
  -- main' "elab" src


------------------------------------------------------------
