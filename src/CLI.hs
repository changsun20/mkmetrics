module CLI (parseCLI) where

import Options.Applicative
import Stats.Types (Config(..))

parseCLI :: IO Config
parseCLI = execParser $ info (configParser <**> helper)
  (fullDesc <> progDesc "Markdown file statistics tool")

configParser :: Parser Config
configParser = Config
  <$> argument str (metavar "FILE" <> help "Path to the input Markdown file")