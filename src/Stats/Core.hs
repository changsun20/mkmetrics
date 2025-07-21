module Stats.Core (processFile) where

import qualified Data.Text.IO as TIO
import Stats.Basic (computeBasicStats)
import Stats.Markdown (computeMDStats)
import Stats.Types (FileStats (FileStats))
import Prelude hiding (readFile)

processFile :: FilePath -> IO FileStats
processFile path = do
  content <- TIO.readFile path
  let basicStats = computeBasicStats content
      mdStats = computeMDStats content
  return $ FileStats path basicStats mdStats