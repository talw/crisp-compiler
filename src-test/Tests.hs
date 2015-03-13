module Main
 ( main
 ) where

import Paths_lc_hs (getBinDir, getDataDir)

import           Test.HUnit

import System.Process (StdStream(CreatePipe), CreateProcess(std_out)
                      ,proc, createProcess, callProcess, readProcess)
import System.FilePath ((</>), (<.>), takeExtension, dropExtension)
import Control.Concurrent (threadDelay)
import System.Directory (getDirectoryContents, getCurrentDirectory
                        ,removeFile)
import Data.Functor ((<$>))
import qualified System.IO.Strict as IOSTRICT

testExecutableName :: String
testExecutableName = "testExecutable"

-- helper functions

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO Counts
main = runTestTT =<< TestList <$> accumulateTests

scmFileTest :: String -> Test
scmFileTest testName = testCase testName $ do
  binDir <- getBinDir
  dataDir <- getDataDir
  currDir <- getCurrentDirectory
  let prefixFilePath = dataDir </> "tests" </> testName
      scmFilePath = prefixFilePath <.> "scm"
      ansFilePath = prefixFilePath <.> "ans"
      executableFilePath = currDir </> testExecutableName

  callProcess (binDir </> "crc")
    [ "-i", dataDir </> "tests" </> testName <.> "scm"
    , "-o", executableFilePath]
  result <- readProcess executableFilePath [] []

  correctResult <-
    IOSTRICT.readFile $ dataDir </> "tests" </> testName <.> "ans"
  removeFile executableFilePath

  correctResult @=? result

accumulateTests :: IO [Test]
accumulateTests = do
  dataDir <- getDataDir
  files <- getDirectoryContents $ dataDir </> "tests"

  return
    . map (scmFileTest . dropExtension)
    . filter (\f -> takeExtension f == ".scm")
    $ files
