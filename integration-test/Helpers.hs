{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Integration test setup utilities
module Helpers
    ( getTestConfig
    , generateUniqueName
    , withTestConfig
    , liftTodoist
    , assertSucceeds
    , buildTestProject
    ) where

import qualified Configuration.Dotenv as Dotenv
import Control.Applicative (pure)
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Function (($), (.))
import Data.Functor (fmap, (<&>))
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (Text, pack)
import GHC.Err (error)
import System.Environment (lookupEnv)
import System.IO (IO, putStrLn)
import System.Random (randomRIO)
import Test.Hspec (shouldBe)
import Text.Show (Show, show)
import Web.Todoist.Builder
    ( runBuilder
    , setDescription
    , setViewStyle
    )
import Web.Todoist.Domain.Project (ProjectCreate, TodoistProjectM (..), newProject)
import Web.Todoist.Domain.Types (ViewStyle (..))
import Web.Todoist.Internal.Config (TodoistConfig)
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Runner (newTodoistConfig, todoist)

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

{- | Lift a Todoist operation into ExceptT
Makes it easier to chain API calls in do-notation
-}
liftTodoist :: TodoistConfig -> (forall m. (TodoistProjectM m) => m a) -> ExceptT TodoistError IO a
liftTodoist config operation = ExceptT $ todoist config operation

{- | Unwrap an ExceptT computation and fail the test if it resulted in an error
Prints the error message and fails the test using Hspec's shouldBe
-}
assertSucceeds :: (Show e) => ExceptT e IO a -> IO a
assertSucceeds action = do
    result <- runExceptT action
    case result of
        Left err -> do
            putStrLn $ "Test failed with error: " <> show err
            False `shouldBe` True
            error "unreachable: test should have failed"
        Right val -> pure val

{- | Build a test project with all fields set using the Builder pattern
Creates a ProjectCreate with all possible fields populated for testing
-}
buildTestProject :: Text -> ProjectCreate
buildTestProject projectName =
    runBuilder
        ( newProject projectName
            <> setDescription "Test project description for integration testing"
            <> setViewStyle Board
        )
