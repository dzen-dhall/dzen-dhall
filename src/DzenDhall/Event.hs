module DzenDhall.Event where

import           DzenDhall.Config
import           DzenDhall.Data
import           DzenDhall.Extra

import           Control.Exception
import           Control.Monad
import qualified Data.HashMap.Strict as H
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text
import           Pipes
import qualified Pipes.Prelude as P
import           System.Exit
import           System.IO
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Text.IO
import           Data.Void


type AutomatonState = Text

-- | 'StateTransitionTable' is needed to know *how* to update, 'IORef' 'Bar' is needed
-- to know *what* to update. Int parameter is a unique identifier for automata.
data Subscription
  = AutomatonSubscription
    StateTransitionTable
    (H.HashMap AutomatonState Bar)
    (IORef AutomatonState)
    (IORef Bar)

type AutomataHandles = H.HashMap SlotAddr [Subscription]

type SlotAddr = Text

data RoutedEvent = RoutedEvent Event SlotAddr
  deriving (Eq, Show)

-- | Start reading lines from a named pipe used to route events.
-- On each event, try to parse it, and find which event subscriptions does the event affect.
launchEventListener :: String -> AutomataHandles -> IO ()
launchEventListener namedPipe handles = runEffect $ do
  fh <- lift $ handle handler $ do
    fh <- openFile namedPipe ReadWriteMode
    hSetBuffering fh LineBuffering
    pure fh

  for (P.fromHandle fh) $ \line -> do
    lift $ do
      case parseRoutedEvent line of
        Just (RoutedEvent button slotAddr) ->
          case H.lookup slotAddr handles of
            Just subscriptions ->
              processSubscriptions slotAddr button subscriptions
            Nothing ->
              Data.Text.IO.putStrLn $ "Failed to find subscriptions for slot address: " <> slotAddr

        Nothing ->
          putStrLn $ "Failed to parse routed event from string: " <> line

  where

    handler (e :: IOError) = do
      putStrLn $ "Couldn't open named pipe " <> namedPipe <> ": " <> displayException e
      exitWith (ExitFailure 1)

processSubscriptions :: SlotAddr -> Event -> [Subscription] -> IO ()
processSubscriptions slotAddr button = mapM_ $ \case

  AutomatonSubscription stt stateMap stateRef barRef -> do

    currentState <- readIORef stateRef

    let
      transitions = unSTT stt :: H.HashMap (SlotAddr, Event, Text) Text
      mbNextState = H.lookup (slotAddr, button, currentState) transitions

    whenJust mbNextState $ \nextState -> do
      whenJust (H.lookup nextState stateMap) $ \nextBar -> do
        writeIORef barRef nextBar
        writeIORef stateRef nextState

-- | E.g. @parseRoutedEvent "event:1,slot:name@some-scope" == Just (RoutedEvent (MouseEvent MouseLeft) "name@some-scope")@
parseRoutedEvent :: String -> Maybe RoutedEvent
parseRoutedEvent = parseMaybe routedEventParser

type Parser = Parsec Void String

routedEventParser :: Parser RoutedEvent
routedEventParser = do
  void $ string "event:"
  mb <- (MouseEvent  <$> buttonParser) <|>
        (CustomEvent <$> customEventParser)
  void $ char ','
  void $ string "slot:"
  slotName <- slotNameParser
  void $ string "@"
  scope <- scopeParser
  pure $ RoutedEvent mb (slotName <> "@" <> scope)

buttonParser :: Parser Button
buttonParser = do
      MouseLeft        <$ char '1'
  <|> MouseMiddle      <$ char '2'
  <|> MouseRight       <$ char '3'
  <|> MouseScrollUp    <$ char '4'
  <|> MouseScrollDown  <$ char '5'
  <|> MouseScrollLeft  <$ char '6'
  <|> MouseScrollRight <$ char '7'

automatonAddressParser :: Parser Text
automatonAddressParser = capitalized

slotNameParser :: Parser Text
slotNameParser = capitalized

customEventParser :: Parser Text
customEventParser = capitalized

capitalized :: Parser Text
capitalized = Data.Text.pack <$>
  liftM2 (:) upperChar (many (upperChar <|> digitChar <|> char '_'))

scopeParser :: Parser Text
scopeParser = Data.Text.pack <$> some (alphaNumChar <|> char '-')
