{-# LANGUAGE TemplateHaskell #-}
-- | Arguments data and parser.
module DzenDhall.Arguments where

import           Options.Applicative
import           Data.Semigroup ((<>))
import           Lens.Micro.TH

data StdoutFlag
  = ToDzen | ToStdout
  deriving (Show, Eq)

data Command
  = Init
  | Plug String
  deriving (Show, Eq)

data Explain
  = Explain | DontExplain
  deriving (Show, Eq)

data Arguments
  = Arguments
  { _mbConfigDir :: Maybe String
  , _mbDzenBinary :: Maybe String
  , _stdoutFlag :: StdoutFlag
  , _mbCommand  :: Maybe Command
  , _explain :: Explain
  }
  deriving (Show, Eq)

makeLenses ''Arguments

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

  <*> flag ToDzen ToStdout
      ( long "stdout"
     <> help "Write output to stdout."
      )

  <*> optional
      (
        hsubparser
        ( command "init" (
            info
              ( pure Init )
              ( progDesc "Write default configuration files to configuration directory." )
            )
       <> command "plug" (
            info
              (Plug <$> argument str (metavar "PLUGIN PATH"))
              ( progDesc "Download plugin to plugins directory")
            )
        )
      )

  <*> flag DontExplain Explain
      ( long "explain"
     <> help "Explain error messages in more detail."
      )


argumentsParser :: ParserInfo Arguments
argumentsParser = info (argParser <**> helper)
             (  fullDesc
             <> progDesc "Configure dzen2 bars in Dhall language." )
