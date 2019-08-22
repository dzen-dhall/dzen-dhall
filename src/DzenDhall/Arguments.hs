{-# LANGUAGE TemplateHaskell #-}
-- | Arguments data and parser.
module DzenDhall.Arguments where

import           Options.Applicative
import           Data.Semigroup ((<>))
import           Lens.Micro.TH

-- * Flags

data Explain
  = Explain | DontExplain
  deriving (Show, Eq)

-- | Whether to skip confirmation when running `plug` command.
data ConfirmPlug
  = Confirm | DontConfirm
  deriving (Show, Eq)

data StdoutFlag
  = ToDzen | ToStdout
  deriving (Show, Eq)

data SkipAssertions
  = SkipAssertions | DontSkip
  deriving (Show, Eq)

-- * Main data

data PlugCommand
  = PlugCommand { source :: String
                , confirm :: ConfirmPlug
                }
  deriving (Show, Eq)

data ValidateCommand
  = ValidateCommand { skipAssertions :: SkipAssertions
                    }
  deriving (Show, Eq)

data Command
  = Init
  | Plug PlugCommand
  | Validate ValidateCommand
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
     <> help "Path to Dzen executable. By default, 'dzen2' binary from $PATH will be used."
      )

  <*> flag ToDzen ToStdout
      ( long "stdout"
     <> help "Write output to stdout."
      )

  <*> optional
      (
        hsubparser
        ( command "init"
          ( info
            ( pure Init )
            ( progDesc "Write default configuration files to configuration directory." )
          )
       <> command "plug"
          ( info
              ( Plug <$> plugCommandParser )
              ( progDesc "Install a plugin to the `plugins` directory. Acceptable formats: `name`, `github-username/repository`, a file name or URL." )
            )
       <> command "validate"
          ( info
              ( Validate <$> validateCommandParser )
              ( progDesc "Validate the configuration without running it." )
          )
        )
      )

  <*> flag DontExplain Explain
      ( long "explain"
     <> help "Explain error messages in more detail."
      )


plugCommandParser :: Parser PlugCommand
plugCommandParser =
  PlugCommand <$> argument str (metavar "PLUGIN SOURCE")
              <*> flag Confirm DontConfirm
                  ( long "yes"
                 <> help "Skip manual code review and confirmation."
                  )

validateCommandParser :: Parser ValidateCommand
validateCommandParser =
  ValidateCommand <$> flag SkipAssertions DontSkip
                      ( long "include-assertions"
                     <> short 'a'
                     <> help "Validate assertions too (this will result in running executable code)."
                      )

argumentsParser :: ParserInfo Arguments
argumentsParser = info (argParser <**> helper)
             ( fullDesc
            <> progDesc "Configure dzen2 bars in Dhall language." )
