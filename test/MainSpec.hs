module MainSpec (spec) where

import Control.Exception (finally)
import Output (printStats)
import Stats.Core (processFile)
import System.Directory (removeFile)
import System.IO.Silently (capture)
import Test.Hspec

spec :: Spec
spec = do
  describe "End-to-end process" $ do
    it "correctly processes and outputs markdown file statistics" $ do
      let testFile = "test-integration.md"
      let content = "# Integration Test\n\nThis is a test paragraph.\n\nIt has multiple lines.\n\n```\nSome code\n```\n\nAnd a [link](https://example.com)."

      writeFile testFile content

      stats <- processFile testFile `finally` removeFile testFile

      (capturedOutput, _) <- capture $ printStats stats

      capturedOutput `shouldContain` "File: test-integration.md"
      capturedOutput `shouldContain` "Lines: 11"
      capturedOutput `shouldContain` "Empty Lines: 4"
      capturedOutput `shouldContain` "Words: 19"
      capturedOutput `shouldContain` "Headers: 1"
      capturedOutput `shouldContain` "Code Blocks: 1"
      capturedOutput `shouldContain` "Links: 1"