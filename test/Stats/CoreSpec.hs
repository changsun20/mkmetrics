module Stats.CoreSpec (spec) where

import Control.Exception (finally)
import Stats.Core (processFile)
import Stats.Types (BasicStats (..), FileStats (..), MDStats (..))
import System.Directory (removeFile)
import Test.Hspec

spec :: Spec
spec = do
  describe "processFile" $ do
    it "correctly processes a markdown file" $ do
      let testFile = "test-file.md"
      let content = "# Test Header\n\nThis is a paragraph with [link](https://example.com).\n\n```\ncode block\n```"

      writeFile testFile content
      stats <- processFile testFile `finally` removeFile testFile

      filePath stats `shouldBe` testFile

      lineCount (basic stats) `shouldBe` 7
      emptyLineCount (basic stats) `shouldBe` 2
      wordCount (basic stats) `shouldBe` 13

      headerCount (md stats) `shouldBe` 1
      codeBlockCount (md stats) `shouldBe` 1
      linkCount (md stats) `shouldBe` 1
