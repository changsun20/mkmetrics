module Main (main) where

import CLI (parseCLI)
import Output (printStats)
import Stats.Core (processFile)
import Stats.Types (inputPath)

main :: IO ()
main = do
  config <- parseCLI
  stats <- processFile (inputPath config)
  printStats stats
