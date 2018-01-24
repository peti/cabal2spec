module Main ( main ) where

import Cabal2Spec

import Distribution.Compiler
import Distribution.System
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
  goldenVsFileDiff specFile
                   (\ref new -> ["diff", "-u", ref, new])
                   goldenFile
                   specFile
                   (cabal2spec buildPlatform buildCompilerId [] True cabalFile specFile)
