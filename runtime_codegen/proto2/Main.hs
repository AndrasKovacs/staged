
module Main where

import Control.Exception
import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import System.Process

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

--------------------------------------------------------------------------------

helpMsg = unlines [
  "usage: rtcg FILE [--help|elab|zonk|interp|compile|run|nf|type]",
  "  --help    : display this message",
  "  elab      : print elaboration output",
  "  zonk      : print zonking & erasure output",
  "  interp    : run program in interpreter",
  "  compile   : print compiled javascript output",
  "  run       : compile to javascript and run",
  "  nf        : print beta-normal form and beta-normal type",
  "  type      : print type of program"]

mainWith :: IO [String] -> IO ()
mainWith getOpt = do

  (path, opts) <- getOpt >>= \case
    path:opts -> pure (path, opts)
    _         -> putStrLn helpMsg >> exitSuccess

  let handleErr file act = act `catch` \e -> displayError e >> exitSuccess

  let elab = do
        file <- readFile path
        t    <- parseString file
        res  <- handleErr file (inferTop file t)
        pure (res, file)

  reset
  case opts of
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
      out <- build <$> Compiler.genTop t
      putStrLn out
      dir <- getCurrentDirectory
      putStrLn $ "written to: " ++ (dir </> "out.js")
      writeFile "out.js" out
    ["interp"] -> do
      ((t, a), file) <- elab
      t <- handleErr file (zonk0 t)
      res <- Interpreter.execTop (castTm t)
      res <- pure $ unzonk $ Interpreter.readBackClosed res
      putStrLn "RESULT:"
      putStrLn $ showTm0 res
    ["run"] -> do
      ((t, a), file) <- elab
      t <- handleErr file (zonk0 t)
      out <- build <$> Compiler.genTop t
      writeFile "out.js" out
      callCommand "node out.js"
    _ -> putStrLn helpMsg

main :: IO ()
main = mainWith getArgs

-- | Run main with inputs as function arguments.
main' :: String -> IO ()
main' opts = mainWith (pure $ words opts)

test :: String ->  IO ()
test cmd = main' ("test.rtcg " ++ cmd)

------------------------------------------------------------
