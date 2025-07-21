module Stats.Basic (computeBasicStats) where

import Data.Text (Text)
import qualified Data.Text as T
import Stats.Types (BasicStats (..))

computeBasicStats :: Text -> BasicStats
computeBasicStats content =
  let charCount' = T.length content
      lines' = T.lines content
      lineCount' = length lines'
      emptyLineCount' = length $ filter T.null lines'
      wordCount' = length $ T.words content
   in BasicStats
        { charCount = charCount',
          wordCount = wordCount',
          lineCount = lineCount',
          emptyLineCount = emptyLineCount'
        }