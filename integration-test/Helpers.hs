{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Integration test setup utilities
module Helpers
    ( getTestConfig
    , generateUniqueName
    , withTestConfig
    , withTestProject
    , withTestProjects
    , liftTodoist
    , assertSucceeds
    ) where

import qualified Configuration.Dotenv as Dotenv
import Control.Applicative (pure)
import Control.Exception (SomeException, bracket, catch)
import Control.Monad (mapM, mapM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Function (($), (.))
import Data.Functor (fmap, (<&>))
import Data.Int (Int)
import qualified Data.List as L
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (Text, pack)
import System.Environment (lookupEnv)
import Prelude (error)
import System.IO (IO, putStrLn)
import System.Random (randomRIO)
import Test.Hspec (shouldBe)
import Text.Show (Show, show)
import Web.Todoist.Domain.Project (TodoistProjectM (..), newProject)
import qualified Web.Todoist.Domain.Project as P
import Web.Todoist.Internal.Config (TodoistConfig)
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Runner (todoist, newTodoistConfig)

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
liftTodoist :: TodoistConfig -> (forall m. TodoistProjectM m => m a) -> ExceptT TodoistError IO a
liftTodoist config operation = ExceptT $ todoist config operation

{- | Unwrap an ExceptT computation and fail the test if it resulted in an error
Prints the error message and fails the test using Hspec's shouldBe
-}
assertSucceeds :: Show e => ExceptT e IO a -> IO a
assertSucceeds action = do
    result <- runExceptT action
    case result of
        Left err -> do
            putStrLn $ "Test failed with error: " <> show err
            False `shouldBe` True
            error "unreachable: test should have failed"
        Right val -> pure val

{- | Create a test project, run an action with its ID, then delete it
Uses bracket to ensure cleanup happens even if the action fails
The action runs in ExceptT for clean error handling
-}
withTestProject :: TodoistConfig -> Text -> (P.ProjectId -> ExceptT TodoistError IO a) -> IO ()
withTestProject config projectName action = do
    let createProject = do
            liftIO $ putStrLn $ "Creating test project: " <> show projectName
            liftTodoist config (addProject $ newProject projectName)

    let deleteProject projectId = do
            liftIO $ putStrLn $ "Cleaning up test project: " <> show projectName
            void $ todoist config (Web.Todoist.Domain.Project.deleteProject projectId)

    let runAction projectId = void $ assertSucceeds $ action projectId

    bracket (assertSucceeds createProject) deleteProject runAction

{- | Create multiple test projects, run an action with their IDs, then delete all
Uses bracket to ensure cleanup of all projects even if the action fails
The action runs in ExceptT for clean error handling
-}
withTestProjects :: TodoistConfig -> [Text] -> ([P.ProjectId] -> ExceptT TodoistError IO a) -> IO ()
withTestProjects config projectNames action = do
    let createProjects = do
            liftIO $ putStrLn $ "Creating " <> show (L.length projectNames) <> " test projects"
            mapM (\name -> liftTodoist config (addProject $ newProject name)) projectNames

    let deleteProjects projectIds = do
            liftIO $ putStrLn $ "Cleaning up " <> show (L.length projectIds) <> " test projects"
            mapM_ (todoist config . Web.Todoist.Domain.Project.deleteProject) projectIds

    let runAction projectIds = void $ assertSucceeds $ action projectIds

    bracket (assertSucceeds createProjects) deleteProjects runAction
