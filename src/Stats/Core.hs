module Stats.Core (processFile) where

import Stats.Types (FileStats(FileStats))
import Stats.Basic (computeBasicStats)
import Stats.Markdown (computeMDStats)
import Data.Text (Text, pack)
import Prelude hiding (readFile)
import System.IO (readFile)

processFile :: FilePath -> IO FileStats
processFile path = do
  content <- readFileContent path
  let basicStats = computeBasicStats content
      mdStats = computeMDStats content
  return $ FileStats path basicStats mdStats

readFileContent :: FilePath -> IO Text
readFileContent path = do
  content <- readFile path
  return $ pack content