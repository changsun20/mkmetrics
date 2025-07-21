module Stats.MarkdownSpec (spec) where

import Data.Text (pack)
import Stats.Markdown (computeMDStats)
import Stats.Types (MDStats (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "computeMDStats" $ do
    it "counts headers correctly" $ do
      let text = pack "# Header 1\nNormal text\n## Header 2"
      headerCount (computeMDStats text) `shouldBe` 2

    it "counts code blocks correctly" $ do
      let text = pack "Some text\n```\ncode block\n```\nMore text\n```\nanother block\n```"
      codeBlockCount (computeMDStats text) `shouldBe` 2

    it "counts links correctly" $ do
      let text = pack "This is a [link](https://example.com) and [another](https://test.com)"
      linkCount (computeMDStats text) `shouldBe` 2

    it "handles text with no markdown elements" $ do
      let text = pack "Just plain text\nwith multiple lines\nbut no markdown"
      let stats = computeMDStats text
      headerCount stats `shouldBe` 0
      codeBlockCount stats `shouldBe` 0
      linkCount stats `shouldBe` 0
