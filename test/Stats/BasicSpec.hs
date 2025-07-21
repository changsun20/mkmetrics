module Stats.BasicSpec (spec) where

import Data.Text (pack)
import Stats.Basic (computeBasicStats)
import Stats.Types (BasicStats (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "computeBasicStats" $ do
    it "counts characters correctly" $ do
      let text = pack "Hello"
      charCount (computeBasicStats text) `shouldBe` 5

    it "counts words correctly" $ do
      let text = pack "Hello world"
      wordCount (computeBasicStats text) `shouldBe` 2

    it "counts lines correctly" $ do
      let text = pack "Line 1\nLine 2\nLine 3"
      lineCount (computeBasicStats text) `shouldBe` 3

    it "counts empty lines correctly" $ do
      let text = pack "Line 1\n\nLine 3"
      emptyLineCount (computeBasicStats text) `shouldBe` 1

    it "handles empty text" $ do
      let text = pack ""
      let stats = computeBasicStats text
      charCount stats `shouldBe` 0
      wordCount stats `shouldBe` 0
      lineCount stats `shouldBe` 0
      emptyLineCount stats `shouldBe` 0
