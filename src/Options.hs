module Options
  ( CompilerOptions(..)
  , getOptions
  ) where

import           Data.Maybe            (isNothing)
import           System.Environment    (getProgName)
import           System.Exit           (exitFailure, exitSuccess)

import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt, usageInfo)

-- | Represents possible options from the cmdline
data CompilerOptions = CompilerOptions
  { optPrintLLVM      :: Bool
  , optOutputFilePath :: FilePath
  , optInputFilePath :: FilePath
  , optReplMode :: Bool
  } deriving Show

-- One can run quoridor at local play, server host or client join modes
defaultOptions :: CompilerOptions
defaultOptions = CompilerOptions
  { optPrintLLVM = False
  , optOutputFilePath = "a.out"
  , optInputFilePath = ""
  , optReplMode = False
  }

-- | Given the args from the cmdline,
-- returns them parsed into an Options data value.
-- It runs in the IO monad to allow the ability to exit the program
-- if something fails in the parsing (upon which, a usageInfo will be displayed)
getOptions :: [String] -> IO CompilerOptions
getOptions args = do
  opts <- foldl (>>=) (return defaultOptions) actions
  if null (optInputFilePath opts) && not (optReplMode opts)
    then putUsageInfo >> exitFailure
    else return opts
 where
  (actions, _, _) = getOpt RequireOrder options args

--helpers

putUsageInfo :: IO ()
putUsageInfo = do
  prg <- getProgName
  putStrLn (usageInfo prg options)

options :: [ OptDescr (CompilerOptions -> IO CompilerOptions) ]
options =
  [ Option "i" ["input-filepath"]
      (ReqArg
          (\arg opts ->
            let trim = unwords . words
            in return opts { optInputFilePath = trim arg })
          "FILEPATH")
      "Path of input file. REQUIRED if not in repl-mode"

  , Option "o" ["output-filepath"]
      (ReqArg
          (\arg opts -> return opts { optOutputFilePath = arg })
          "FILEPATH")
      "Path of output file. Default: ./a.out"

  , Option "r" ["repl-mode"]
      (NoArg $
          \opts -> return opts { optReplMode = True })
      "Interactive REPL mode. Default: false"

  , Option "p" ["print-llvm"]
      (NoArg $
          \opts -> return opts { optPrintLLVM = True })
      "Output resulting LLVM IR. Default: false"

  , Option "h" ["help"]
      (NoArg $
          \_ -> do
            putUsageInfo
            exitSuccess)
      "Show help"
  ]
