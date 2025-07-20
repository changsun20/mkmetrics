module Stats.Markdown (computeMDStats) where

import Stats.Types (MDStats(..))
import Data.Text (Text)

computeMDStats :: Text -> MDStats
computeMDStats _content = 
  MDStats
    { headerCount = 0
    , codeBlockCount = 0
    , linkCount = 0
    }