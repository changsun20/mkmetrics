module Stats.Basic (computeBasicStats) where

import Stats.Types (BasicStats(..))
import Data.Text (Text)

computeBasicStats :: Text -> BasicStats
computeBasicStats _content = 
  BasicStats
    { charCount = 0
    , wordCount = 0
    , lineCount = 0
    , emptyLineCount = 0
    }