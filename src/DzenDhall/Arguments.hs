module DzenDhall.Arguments where

import Options.Applicative
import Data.Semigroup ((<>))

data Arguments = Arguments
  { configPath :: Maybe String
  }
  deriving (Show, Eq)

argParser :: Parser Arguments
argParser = Arguments
  <$> optional
      ( strOption
      $ long "config-dir"
     <> metavar "DIRECTORY"
     <> help "Config directory" )

parserInfo :: ParserInfo Arguments
parserInfo = info (argParser <**> helper)
             ( fullDesc
            <> progDesc "Configure dzen2 bars in Dhall language" )
