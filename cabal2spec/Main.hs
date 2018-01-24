module Main ( main ) where

import Cabal2Spec

import Distribution.Compiler
import Distribution.System

main :: IO ()
main = cabal2spec buildPlatform buildCompilerId [] True "cabal2spec.cabal" "cabal2spec.spec"
