module Stats.Core (processFile) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Stats.Basic (computeBasicStats)
import Stats.Markdown (computeMDStats)
import Stats.Types (FileStats (FileStats))
import System.IO (IOMode (ReadMode), hClose, hSetEncoding, openFile, utf8)
import Prelude hiding (readFile)

processFile :: FilePath -> IO FileStats
processFile path = do
  content <- readContent path
  let basicStats = computeBasicStats content
      mdStats = computeMDStats content
  return $ FileStats path basicStats mdStats

readContent :: FilePath -> IO Text
readContent path = do
  handle <- openFile path ReadMode
  hSetEncoding handle utf8
  content <- TIO.hGetContents handle
  hClose handle
  return content
