module Stats.Types 
  ( BasicStats(..)
  , MDStats(..)
  , FileStats(..)
  ) where

data BasicStats = BasicStats
  { charCount :: Int
  , wordCount :: Int
  , lineCount :: Int
  , emptyLineCount :: Int
  } deriving (Eq, Show)

data MDStats = MDStats
  { headerCount :: Int
  , codeBlockCount :: Int
  , linkCount :: Int
  } deriving (Eq, Show)

data FileStats = FileStats
  { filePath :: FilePath
  , basic :: BasicStats
  , md :: MDStats
  } deriving (Eq, Show)