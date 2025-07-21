module Stats.Markdown (computeMDStats) where

import Data.Text (Text)
import qualified Data.Text as T
import Stats.Types (MDStats (..))

computeMDStats :: Text -> MDStats
computeMDStats content =
  let lines' = T.lines content
      headerCount' = length $ filter isHeader lines'
      codeBlockCount' = length (filter isCodeBlock lines') `div` 2
      linkCount' = sum $ map countLinksInLine lines'
   in MDStats
        { headerCount = headerCount',
          codeBlockCount = codeBlockCount',
          linkCount = linkCount'
        }

isHeader :: Text -> Bool
isHeader = T.isPrefixOf (T.pack "#")

isCodeBlock :: Text -> Bool
isCodeBlock = T.isPrefixOf (T.pack "```")

countLinksInLine :: Text -> Int
countLinksInLine line =
  let linkPattern = T.pack "]("
      parts = T.splitOn linkPattern line
   in length parts - 1