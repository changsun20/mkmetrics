module CLI (parseCLI, Config (..)) where

import Options.Applicative

newtype Config = Config
  { inputPath :: FilePath
  }
  deriving (Eq, Show)

parseCLI :: IO Config
parseCLI =
  execParser $
    info
      (configParser <**> versionOption <**> helper)
      (fullDesc <> progDesc "Markdown file statistics tool")

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    "MkMetrics v0.1.0"
    (short 'v' <> long "version" <> help "Show version information")

configParser :: Parser Config
configParser =
  Config
    <$> argument str (metavar "FILE" <> help "Path to the input Markdown file")