{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson (FromJSON, FromJSONKey, parseJSON)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Yaml as Y
import GHC.Generics
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.Process (readProcess)

data Config = Config { commands :: Map Command Focus }
  deriving (Eq, Show, Generic)

instance FromJSON Config

newtype Command = Command String
  deriving (Eq, Ord, Show, Generic, FromJSON, FromJSONKey)

data Focus = FocusOne String | FocusIter [String]
  deriving (Eq, Show)

instance FromJSON Focus where
  parseJSON v =
        (FocusOne <$> parseJSON v)
    <|> (FocusIter <$> parseJSON v)

main :: IO ()
main = do
  [cmd] <- map Command <$> getArgs
  home <- getHomeDirectory
  Config {..} <- Y.decodeFileEither (home <> "/.refocus.yaml") >>=
                  either throwIO return
  let focus = maybe (error "Command not in ~/.refocus.yaml") id $ Map.lookup cmd commands
  case focus of
    FocusOne app -> doFocus app
    FocusIter apps -> do
      active <- getActiveApp
      putStrLn $ "active: " <> show active <> "\napps: " <> show apps
      if active `elem` apps then do
        let nextApp = head $ tail $ dropWhile (/= active) $ cycle apps
        doFocus nextApp
      else
        doFocus $ head apps

getActiveApp :: IO String
getActiveApp = osascript $ intercalate "\n"
    [ "tell application \"System Events\""
    , "item 1 of (get name of processes whose frontmost is true)"
    , "end tell"
    ]

doFocus :: String -> IO ()
doFocus app = void $ osascript $ "tell application " <> show app <> " to activate"

osascript :: String -> IO String
osascript code =
  (reverse . dropWhile (== '\n') . reverse)
    <$> readProcess "osascript" ["-e", code] ""
