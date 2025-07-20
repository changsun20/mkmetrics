module Main (main) where

import CLI (parseCLI, inputPath)
import Stats.Core (processFile)
import Output (printStats)

main :: IO ()
main = do
  config <- parseCLI
  stats <- processFile (inputPath config)
  printStats stats
