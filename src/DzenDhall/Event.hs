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
import           Text.Read (readMaybe)

import           DzenDhall.Config
import           DzenDhall.Data
import           DzenDhall.Extra

-- | 'StateTransitionTable' is needed to know *how* to update, 'IORef' 'Bar' is needed
-- to know *what* to update. Int parameter is unique identifier for automata.
type AutomataHandles = H.HashMap Int (StateTransitionTable, H.HashMap Text Bar, IORef Text, IORef Bar)

data RoutedEvent = RoutedEvent MouseButton Int
  deriving (Eq, Show)

launchEventListener :: String -> AutomataHandles -> IO ()
launchEventListener namedPipe handles = runEffect $ do
  fh <- lift $ handle handler $ do
    fh <- openFile namedPipe ReadWriteMode
    hSetBuffering fh LineBuffering
    pure fh

  for (P.fromHandle fh) $ \line -> do
    lift $ do

      whenJust (parseRoutedEvent line) $ \(RoutedEvent mb identifier) -> do
        whenJust (H.lookup identifier handles) $ \(stt, stateMap, stateRef, barRef) -> do

          currentState <- readIORef stateRef
          let mbNextState = H.lookup (mb, currentState)
                            (unSTT stt :: H.HashMap (MouseButton, Text) Text)

          whenJust mbNextState $ \nextState -> do
            whenJust (H.lookup nextState stateMap) $ \nextBar -> do
              writeIORef barRef nextBar
              writeIORef stateRef nextState

  where
    handler (e :: IOError) = do
      putStrLn $ "Couldn't open named pipe " <> namedPipe <> ": " <> displayException e
      exitWith (ExitFailure 1)

-- | E.g. @parseRoutedEvent "id:10,event:1" == Just (Event MouseLeft 10)@
parseRoutedEvent :: String -> Maybe RoutedEvent
parseRoutedEvent = parseMaybe routedEventParser

routedEventParser :: Parsec () String RoutedEvent
routedEventParser = do
  void $ string "id:"
  mIdentifier :: Maybe Int <- readMaybe <$> some digitChar
  case mIdentifier of
    Nothing -> customFailure ()
    Just identifier -> do
      void $ string ",event:"

      mb <-  MouseLeft        <$ char '1'
         <|> MouseMiddle      <$ char '2'
         <|> MouseRight       <$ char '3'
         <|> MouseScrollUp    <$ char '4'
         <|> MouseScrollDown  <$ char '5'
         <|> MouseScrollLeft  <$ char '6'
         <|> MouseScrollRight <$ char '7'

      pure $ RoutedEvent mb identifier
