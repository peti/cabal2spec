module Main ( main ) where

import Cabal2Spec

import Data.Maybe
import Distribution.Compiler
import Distribution.System
import Distribution.Text
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = do
  testCases <- findByExtension [".golden"] "test/golden-test-cases"
  defaultMain $ testGroup "regressions" (map regressionTest testCases)

regressionTest :: String -> TestTree
regressionTest goldenFile = do
  let pid = takeBaseName (dropExtension goldenFile)
      PackageIdentifier pn _ = fromMaybe (error ("invalid package id " ++ show pid)) (simpleParse pid)
      cabalFile = "test" </> "golden-test-cases" </> pid </> unPackageName pn <.> "cabal"
      specFile = cabalFile -<.> ".spec"
  goldenVsFile pid goldenFile specFile
               (cabal2spec buildPlatform buildCompilerId [] True cabalFile specFile)
