module DzenDhall.Event where

import           DzenDhall.AST.Render (runRender)
import           DzenDhall.App
import           DzenDhall.Config
import           DzenDhall.Extra
import           DzenDhall.Runtime.Data

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.IORef
import           Data.Maybe
import           Data.Text (Text)
import           Data.Void
import           Lens.Micro
import           Pipes hiding (liftIO)
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Read (readMaybe)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Pipes.Prelude as P


data PipeCommand
  = RoutedEvent Event Slot Scope
  | Click Scope Int
  deriving (Eq, Show)

-- | Start reading lines from a named pipe used to route events.
-- On each event, try to parse it, and find which event subscriptions does the event affect.
launchEventListener :: AutomataHandles -> ClickableAreas -> App Forked ()
launchEventListener handles clickableAreas = do
  barRuntime <- get

  let
    namedPipe = barRuntime ^. brNamedPipe

    handler (e :: IOError) = do
      putStrLn $ "Couldn't open named pipe " <> namedPipe <> ": " <> displayException e
      exitWith (ExitFailure 1)

  environment <- liftIO getEnvironment


  liftIO $ runEffect $ do

    fh <- lift $ handle handler $ do
      fh <- openFile namedPipe ReadWriteMode
      hSetBuffering fh LineBuffering
      pure fh

    for (P.fromHandle fh) $ \line -> do
      lift $ do

        case parsePipeCommand line of

          Just (RoutedEvent event slot scope) ->
            case H.lookup (slot, scope) handles of
              Just subscriptions -> do
                processSubscriptions barRuntime slot scope event subscriptions

              Nothing ->
                T.putStrLn $
                "Failed to find subscriptions for address: " <> slot <> "@" <> scope

          Just (Click scope identifier) -> do
            whenJust (H.lookup identifier clickableAreas) $
              \command -> do
                void $ forkIO $ do

                  let emitter =
                        barRuntime ^. brEmitterScript <> " " <> T.unpack scope
                      getter =
                        barRuntime ^. brGetterScript  <> " " <> T.unpack scope
                      setter =
                        barRuntime ^. brSetterScript  <> " " <> T.unpack scope

                  let process =
                        (shell $ T.unpack command)
                        { env = Just $
                          [ ("EMIT", emitter)
                          , ("GET",  getter)
                          , ("SET",  setter)
                          ] <>
                          environment
                        }

                  void $ readCreateProcess process ""

          Nothing ->
            putStrLn $ "Failed to parse routed event from string: " <> line


processSubscriptions :: BarRuntime -> Slot -> Scope -> Event -> [Subscription] -> IO ()
processSubscriptions barRuntime slot scope event subscriptions = do

  environment <- getEnvironment

  forM_ subscriptions $ \case

    AutomatonSubscription address stt stateMap stateRef barRef -> do

      currentState <- readIORef stateRef

      let
        transitions =
          unSTT stt :: H.HashMap (Slot, Scope, Event, Text) (Text, [Hook])
        mbNext =
          H.lookup (slot, scope, event, currentState) transitions <|>
          -- Match "any" event.
          H.lookup (slot, scope, CustomEvent "*", currentState) transitions
        -- An environment extended with EVENT variable containing the name of the
        -- event.
        environment' =
          ("EVENT", T.unpack $ runRender event) : environment

      whenJust mbNext $ \(nextState, hooks) -> void $ forkIO $ do

        mbUnit <- runMaybeT (runHooks environment' barRuntime scope hooks)

        case H.lookup nextState stateMap of

          Nothing -> do
            -- TODO: make this error static
            T.putStrLn $ "Didn't find state " <> showPack nextState <> " in the state map for " <> showPack address

          Just nextBar -> do
            -- Multiple state transitions are executed simultaneously.
            -- This is fine, we don't want to eliminate race conditions.
            -- Somethimes a transition is only added for its outside-world effects,
            -- and we can't distinguish between such a transition and a normal one.

            when (isJust mbUnit) $ do
              writeIORef barRef nextBar
              writeIORef stateRef nextState
              runStateVariableSetter barRuntime scope address nextState

-- | Set a variable named `STATE_address`
runStateVariableSetter :: BarRuntime -> Scope -> AutomatonAddress -> AutomatonState -> IO ()
runStateVariableSetter barRuntime scope address state = do
  let process = shell $
        barRuntime ^. brSetterScript <> " " <>
        T.unpack scope <>
        " STATE_" <> T.unpack address <> " " <>
        T.unpack state

  (exitCode, _stdOut, _stdErr) <- readCreateProcessWithExitCode process ""

  when (exitCode /= ExitSuccess) $
    putStrLn "Setter script exited unsuccessfully. Please report as bug."

runHooks
  :: [(String, String)]
  -> BarRuntime
  -> Scope
  -> [Hook]
  -> MaybeT IO ()
runHooks environment barRuntime scope hooks = do
  forM_ hooks $ \hook -> do

    let binary = T.unpack $
          head $ hook ^. hookCommand
          -- ^ this is safe, because we checked the list for emptiness
          -- during validation.
        args   = map T.unpack $
          tail $ hook ^. hookCommand
        input  = hook ^. hookInput

        emitter =
          barRuntime ^. brEmitterScript <> " " <> T.unpack scope
        getter =
          barRuntime ^. brGetterScript  <> " " <> T.unpack scope
        setter =
          barRuntime ^. brSetterScript  <> " " <> T.unpack scope

        process =
          (proc binary args) { std_out = CreatePipe
                             , std_in  = CreatePipe
                             , std_err = CreatePipe
                             , env = Just $
                               [ ("EMIT", emitter)
                               , ("SET",  setter)
                               , ("GET",  getter)
                               ] <> environment
                             }

    (exitCode, _stdOut, _stdErr) <- lift $
      readCreateProcessWithExitCode process (T.unpack input)
    when (exitCode /= ExitSuccess) $
      throwMaybe


parsePipeCommand :: String -> Maybe PipeCommand
parsePipeCommand = parseMaybe (routedEventParser <|> clickParser)

type Parser = Parsec Void String

-- | E.g.
--
-- @
-- parseMaybe routedEventParser
--   "event:MouseLeft,slot:name@some-scope" ==
--      Just (RoutedEvent (MouseEvent MouseLeft) "name" "some-scope")
-- @
routedEventParser :: Parser PipeCommand
routedEventParser = do
  void $ string "event:"
  event <- (MouseEvent  <$> buttonParser) <|>
           (CustomEvent <$> customEventParser)
  void $ char ','
  void $ string "slot:"
  slotName <- slotNameParser
  void $ char '@'
  scope <- scopeParser
  pure $ RoutedEvent event slotName scope

buttonParser :: Parser Button
buttonParser =
  -- Names are for the user, numbers are used to actually render buttons before
  -- feeding the output to dzen.
      MouseLeft        <$ (string "MouseLeft"        <|> string "1")
  <|> MouseMiddle      <$ (string "MouseMiddle"      <|> string "2")
  <|> MouseRight       <$ (string "MouseRight"       <|> string "3")
  <|> MouseScrollUp    <$ (string "MouseScrollUp"    <|> string "4")
  <|> MouseScrollDown  <$ (string "MouseScrollDown"  <|> string "5")
  <|> MouseScrollLeft  <$ (string "MouseScrollLeft"  <|> string "6")
  <|> MouseScrollRight <$ (string "MouseScrollRight" <|> string "7")

automatonAddressParser :: Parser Text
automatonAddressParser = capitalized

slotNameParser :: Parser Text
slotNameParser = capitalized

customEventParser :: Parser Text
customEventParser = camelCased

capitalized :: Parser Text
capitalized = T.pack <$>
  liftM2 (:) upperChar (many (upperChar <|> digitChar <|> char '_'))

camelCased :: Parser Text
camelCased = T.pack <$>
  liftM2 (:) upperChar (many (alphaNumChar <|> char '_'))

scopeParser :: Parser Text
scopeParser = T.pack <$> some (alphaNumChar <|> char '-')

clickParser :: Parser PipeCommand
clickParser = do
  void $ string "click:"
  identifier <- some digitChar
  void $ string ",scope:"
  scope <- scopeParser
  pure $ Click scope $ fromMaybe 0 $ readMaybe identifier
