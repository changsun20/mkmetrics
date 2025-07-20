module Stats.Types 
  ( BasicStats(..)
  , MDStats(..)
  , FileStats(..)
  , Config(..)
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

data Config = Config
  { inputPath :: FilePath
  } deriving (Eq, Show)