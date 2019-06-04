module DzenDhall.Arguments where

import Options.Applicative
import Data.Semigroup ((<>))

data Arguments
  = Arguments
  { mbConfigDir :: Maybe String
  , mbDzenBinary :: Maybe String
  , mbCommand  :: Maybe Command
  }
  deriving (Show, Eq)

data Command
  = Init
  deriving (Show, Eq)

argParser :: Parser Arguments
argParser = Arguments
  <$> optional
      ( strOption
      $ long "config-dir"
     <> metavar "DIRECTORY"
     <> help "Config directory, used to store your config.dhall and some dhall source code. Default value is $XDG_CONFIG_HOME/dzen-dhall/"
      )

  <*> optional
      ( strOption
      $ long "dzen-binary"
     <> metavar "FILE"
     <> help "Path to dzen2 executable. By default, 'dzen2' binary from $PATH will be used."
      )

  <*> optional
      ( hsubparser
      ( command "init" (info (pure Init) ( progDesc "Write default configuration files to configuration directory." )))
      )

parserInfo :: ParserInfo Arguments
parserInfo = info (argParser <**> helper)
             (  fullDesc
             <> progDesc "Configure dzen2 bars in Dhall language." )
