module Main (main) where

import CLI (inputPath, parseCLI)
import Output (printStats)
import Stats.Core (processFile)

main :: IO ()
main = do
  config <- parseCLI
  stats <- processFile (inputPath config)
  printStats stats
