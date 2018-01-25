module Main ( main ) where

import Cabal2Spec

import Distribution.Compiler
import Distribution.System
import Distribution.Version
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = do
  testCases <- findByExtension [".golden"] "test/golden-test-cases"
  defaultMain $ testGroup "regressions" (map regressionTest testCases)

regressionTest :: String -> TestTree
regressionTest goldenFile = do
  let specFile = dropExtension goldenFile
      cabalFile = specFile `replaceExtension` "cabal"
      pid = Platform X86_64 Linux
      cid = CompilerId GHC (mkVersion [8,2])
  goldenVsFileDiff specFile
                   (\ref new -> ["diff", "-u", ref, new])
                   goldenFile
                   specFile
                   (cabal2spec pid cid [] True cabalFile specFile)
