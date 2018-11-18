{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Extra (ifM, whenJust)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, parseJSON)
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.Foldable (find)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import GHC.Generics
import System.Directory (getHomeDirectory, doesFileExist)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

import qualified Data.Map as Map
import qualified Data.Yaml as Y
import qualified Options.Applicative as O

data Config = Config { commands :: Map Command Focus }
  deriving (Eq, Show, Generic)

instance FromJSON Config

data Args = Args
  { argsCommand :: Command
  , argsReverse :: Bool
  }

newtype Command = Command String
  deriving (Eq, Ord, Show, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data Focus = FocusOne String | FocusIter [String]
  deriving (Eq, Show)

instance FromJSON Focus where
  parseJSON v =
        (FocusOne <$> parseJSON v)
    <|> (FocusIter <$> parseJSON v)

newtype State = State { unState :: Map Command String }
  deriving (Eq, Show, Semigroup, Monoid, FromJSON, ToJSON)

{-# NOINLINE home #-}
home :: FilePath
home = unsafePerformIO getHomeDirectory

configPath :: FilePath
configPath = home <> "/.refocus.yaml"

statePath :: FilePath
statePath = home <> "/.refocus.state"

main :: IO ()
main = run

parseArgs :: IO Args
parseArgs = O.execParser $ O.info parser $ O.fullDesc
  where
  parser = Args
    <$> (Command <$> O.strArgument (O.metavar "<command>"))
    <*> O.flag False True (O.long "reverse")

run :: IO ()
run = do
  Args{..} <- parseArgs
  config@Config {..} <- Y.decodeFileThrow configPath
  let focus = fromMaybe (error $ "Command not in " <> configPath) $ Map.lookup argsCommand commands
  case focus of
    FocusOne app -> do
      active <- getActiveApp
      whenJust (findCommandForApp app config) (\c -> writeStateLastWindow c active)
      doFocus app
    FocusIter apps -> do
      active <- getActiveApp
      if active `elem` apps then do
        let apps' = (if argsReverse then reverse else id) apps
        let nextApp = head $ tail $ dropWhile (/= active) $ cycle apps'
        doFocus nextApp
        writeStateLastWindow argsCommand nextApp
      else do
        maybeWindow <- readStateLastWindow argsCommand
        doFocus $ fromMaybe (head apps) maybeWindow

findCommandForApp :: String -> Config -> Maybe Command
findCommandForApp app Config{..} =
  fmap fst $ flip find (Map.toList commands) $ \(cmd, focus) ->
    case focus of
      FocusOne _ -> False
      FocusIter apps -> app `elem` apps

getActiveApp :: IO String
getActiveApp = rtrim <$> osascript (unlines
    [ "tell application \"System Events\""
    , "item 1 of (get name of processes whose frontmost is true)"
    , "end tell"
    ])

doFocus :: String -> IO ()
doFocus app = osascript_ $ "tell application " <> show app <> " to activate"

readState :: IO State
readState =
  ifM (doesFileExist statePath)
    (Y.decodeFileThrow statePath)
    mempty

readStateLastWindow :: Command -> IO (Maybe String)
readStateLastWindow command = Map.lookup command <$> coerce readState

writeState :: State -> IO ()
writeState s = Y.encodeFile statePath s

writeStateLastWindow :: Command -> String -> IO ()
writeStateLastWindow command app =
  writeState =<< (State . Map.insert command app . unState <$> readState)

osascript :: String -> IO String
osascript code = readProcess "osascript" ["-e", code] ""

osascript_ :: String -> IO ()
osascript_ = void . osascript

rtrim :: String -> String
rtrim = reverse . dropWhile isSpace . reverse
