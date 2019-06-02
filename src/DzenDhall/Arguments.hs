module DzenDhall.Arguments where

import Options.Applicative
import Data.Semigroup ((<>))

data Arguments = Arguments
  { configPath :: Maybe String
  , dzenBinary :: Maybe String
  }
  deriving (Show, Eq)

argParser :: Parser Arguments
argParser = Arguments
  <$> optional
      (  strOption
      $  long "config-dir"
      <> metavar "DIRECTORY"
      <> help "Config directory, used to store your config.dhall and some dhall source code. Default value is $XDG_CONFIG_HOME/dzen-dhall/" )

      (  strOption
      $  long "dzen-binary"
      <> metavar "FILE"
      <> help "Path to dzen2 executable. By default, 'dzen2' binary from $PATH will be used."
      )

parserInfo :: ParserInfo Arguments
parserInfo = info (argParser <**> helper)
             (  fullDesc
             <> progDesc "Configure dzen2 bars in Dhall language." )
