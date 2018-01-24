module Main ( main ) where

import Cabal2Spec

import Distribution.Compiler
import Distribution.System
import Test.Tasty
import Test.Tasty.Golden
import System.FilePath

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "regressions"
  [ regressionTest "cabal2spec.cabal" "cabal2spec.spec"
  ]

regressionTest :: FilePath -> FilePath -> TestTree
regressionTest cabalFile specFile =
  goldenVsFile cabalFile (specFile <.> "golden") specFile
               (cabal2spec buildPlatform buildCompilerId [] True cabalFile specFile)
