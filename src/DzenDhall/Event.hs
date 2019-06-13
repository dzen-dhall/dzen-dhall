module DzenDhall.Event where

import           Control.Exception
import           Control.Monad
import qualified Data.HashMap.Strict as H
import           Data.IORef
import           Data.Text
import           Pipes
import qualified Pipes.Prelude as P
import           System.Exit
import           System.IO
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           DzenDhall.Config
import           DzenDhall.Data
import           DzenDhall.Extra

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

data RoutedEvent = RoutedEvent MouseButton SlotAddr
  deriving (Eq, Show)

launchEventListener :: String -> AutomataHandles -> IO ()
launchEventListener namedPipe handles = runEffect $ do
  fh <- lift $ handle handler $ do
    fh <- openFile namedPipe ReadWriteMode
    hSetBuffering fh LineBuffering
    pure fh

  for (P.fromHandle fh) $ \line -> do
    lift $ do
      whenJust (parseRoutedEvent line) $ \(RoutedEvent mb slotAddr) -> do
        whenJust (H.lookup slotAddr handles) (processSubscriptions slotAddr mb)

  where
    processSubscriptions :: SlotAddr -> MouseButton -> [Subscription] -> IO ()
    processSubscriptions slotAddr mb = mapM_ $ \case

      AutomatonSubscription stt stateMap stateRef barRef -> do
        currentState <- readIORef stateRef
        let mbNextState = H.lookup (slotAddr, mb, currentState)
                          (unSTT stt :: H.HashMap (Text, MouseButton, Text) Text)
        whenJust mbNextState $ \nextState -> do
          whenJust (H.lookup nextState stateMap) $ \nextBar -> do
            writeIORef barRef nextBar
            writeIORef stateRef nextState

    handler (e :: IOError) = do
      putStrLn $ "Couldn't open named pipe " <> namedPipe <> ": " <> displayException e
      exitWith (ExitFailure 1)

-- | E.g. @parseRoutedEvent "event:1,slot:name@some-scope" == Just (Event MouseLeft 10)@
parseRoutedEvent :: String -> Maybe RoutedEvent
parseRoutedEvent = parseMaybe routedEventParser

routedEventParser :: Parsec () String RoutedEvent
routedEventParser = do
  void $ string "event:"
  mb <- MouseLeft        <$ char '1'
    <|> MouseMiddle      <$ char '2'
    <|> MouseRight       <$ char '3'
    <|> MouseScrollUp    <$ char '4'
    <|> MouseScrollDown  <$ char '5'
    <|> MouseScrollLeft  <$ char '6'
    <|> MouseScrollRight <$ char '7'
  void $ char ','
  void $ string "slot:"
  slotName <- some alphaNumChar
  void $ string "@"
  scope <- some (alphaNumChar <|> char '-')
  pure $ RoutedEvent mb (Data.Text.pack (slotName <> "@" <> scope))
