{-# LANGUAGE OverloadedStrings #-}

-- | Integration test setup utilities
module Helpers
    ( getTestConfig
    , generateUniqueName
    , withTestConfig
    ) where

import qualified Configuration.Dotenv as Dotenv
import Control.Applicative (pure)
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.Function ((.))
import Data.Functor (fmap, (<&>))
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (pack)
import System.Environment (lookupEnv)
import System.IO (IO, putStrLn)
import System.Random (randomRIO)
import Text.Show (show)
import Web.Todoist.Internal.Config (TodoistConfig)
import Web.Todoist.Runner (newTodoistConfig)

-- | Load .env file, ignoring errors if file doesn't exist
loadEnvFile :: IO ()
loadEnvFile = catch (void (Dotenv.loadFile Dotenv.defaultConfig)) handler
    where
        handler :: SomeException -> IO ()
        handler _ = pure ()

{- | Get TodoistConfig from TODOIST_TEST_API_TOKEN environment variable
Returns Nothing if the environment variable is not set
Attempts to load .env file first
-}
getTestConfig :: IO (Maybe TodoistConfig)
getTestConfig = do
    loadEnvFile
    lookupEnv "TODOIST_TEST_API_TOKEN" <&> fmap (newTodoistConfig . pack)

{- | Generate a unique name by appending a random number to a prefix
Example: generateUniqueName "TestProject" -> "TestProject-847392"
-}
generateUniqueName :: String -> IO String
generateUniqueName prefix = randomRIO (100 :: Int, 999 :: Int) <&> ((prefix <> "-") <>) . show

{- | Execute an action with test config, or skip if config is not available
Useful for conditional test execution based on environment variable presence
-}
withTestConfig :: (TodoistConfig -> IO ()) -> IO ()
withTestConfig action = do
    maybeConfig <- getTestConfig
    case maybeConfig of
        Nothing -> putStrLn "Skipping: TODOIST_TEST_API_TOKEN not set"
        Just config -> action config
