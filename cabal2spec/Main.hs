module Main ( main ) where

import Cabal2Spec

import Control.Monad
import Distribution.Compiler
import Distribution.System
import System.Environment
import System.FilePath

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \cabalFile -> do
    let specFile = cabalFile -<.> "spec"
    cabal2spec buildPlatform buildCompilerId [] True cabalFile specFile
