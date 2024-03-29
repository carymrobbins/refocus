module Main where

import Control.Applicative ((<|>))
import Control.Monad (mfilter)
import Control.Monad.Extra (ifM, whenJust)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, parseJSON)
import Data.Char (isSpace, toLower)
import Data.Coerce (coerce)
import Data.Foldable (find)
import Data.Functor (void)
import Data.Map (Map)
import Data.Maybe (maybeToList)
import GHC.Generics (Generic)
import System.Directory (getHomeDirectory, doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map
import qualified Data.Yaml as Y
import qualified Options.Applicative as O

import AppKitUtil (NSRunningApplication, getActiveAppName, getAppName, getRunningApps, focusApp)

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

fromMaybeM :: (Monad m) => m a -> Maybe a -> m a
fromMaybeM = flip maybe pure

run :: IO ()
run = do
  Args{..} <- parseArgs
  config@Config {..} <- Y.decodeFileThrow configPath
  focus <- fromMaybeM
    (abort $ "Command not in " <> configPath)
    (Map.lookup argsCommand commands)
  case focus of
    FocusOne app -> do
      active <- getActiveAppNameOrError
      whenJust (findCommandForApp app config) (\c -> writeStateLastWindow c active)
      void $ doFocus [app]
    FocusIter apps -> do
      active <- getActiveAppNameOrError
      if active `elem` apps then do
        let apps' = (if argsReverse then reverse else id) apps
        let nextApps = take (length apps') $ tail $ dropWhile (/= active) $ cycle apps'
        nextApp <- doFocus nextApps
        writeStateLastWindow argsCommand nextApp
      else do
        maybeWindow <- readStateLastWindow argsCommand
        void $ doFocus $ maybeToList maybeWindow ++ apps

abort :: String -> IO a
abort msg = do
  hPutStrLn stderr msg
  exitFailure

doFocus :: [String] -> IO String
doFocus appNames = do
  apps <- getRunningApps
  loop $ (,) <$> appNames <*> apps
  where
  matches :: String -> String -> Bool
  matches appName name = map toLower name == map toLower appName

  loop :: [(String, NSRunningApplication)] -> IO String
  loop = \case
    [] -> abort $ "No app found matching any of " ++ show appNames
    (appName, app) : rest ->
      (mfilter (matches appName) <$> getAppName app) >>= \case
        Nothing -> loop rest
        Just name ->
          ifM (not <$> focusApp app)
            (abort $ "Failed to focus app " ++ name)
            (pure name)

findCommandForApp :: String -> Config -> Maybe Command
findCommandForApp app Config{..} =
  fmap fst $ flip find (Map.toList commands) $ \(_, focus) ->
    case focus of
      FocusOne _ -> False
      FocusIter apps -> app `elem` apps

getActiveAppNameOrError :: IO String
getActiveAppNameOrError =
  fromMaybeM (abort "Active app not found!") =<< getActiveAppName

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

rtrim :: String -> String
rtrim = reverse . dropWhile isSpace . reverse
