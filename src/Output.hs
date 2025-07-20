module Output (printStats) where

import Stats.Types (FileStats(..), BasicStats(..), MDStats(..))

printStats :: FileStats -> IO ()
printStats stats = do
  putStrLn $ "File: " ++ filePath stats
  let b = basic stats
      m = md stats
  putStrLn $ "Lines: " ++ show (lineCount b)
  putStrLn $ "Empty Lines: " ++ show (emptyLineCount b)
  putStrLn $ "Words: " ++ show (wordCount b)
  putStrLn $ "Characters: " ++ show (charCount b)
  putStrLn $ "Headers: " ++ show (headerCount m)
  putStrLn $ "Code Blocks: " ++ show (codeBlockCount m)
  putStrLn $ "Links: " ++ show (linkCount m)
