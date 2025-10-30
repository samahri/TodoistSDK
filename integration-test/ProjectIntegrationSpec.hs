{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ProjectIntegrationSpec (spec) where

import Helpers (generateUniqueName, getTestConfig)

import Web.Todoist.Domain.Project (TodoistProjectM (..))
import qualified Web.Todoist.Domain.Project as P
import Web.Todoist.Patch (newProject)
import Web.Todoist.Runner (todoist)

import Control.Applicative (pure)
import Control.Monad (void)
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Data.Text (pack)
import System.IO (putStrLn)
import Test.Hspec (Spec, describe, it, pendingWith, runIO, shouldBe, shouldSatisfy)
import Text.Show (show)

import Web.Todoist.Internal.Config (TodoistConfig)

spec :: Spec
spec = do
    maybeConfig <- runIO getTestConfig
    case maybeConfig of
        Nothing ->
            it "requires TODOIST_TEST_API_TOKEN" $
                pendingWith "TODOIST_TEST_API_TOKEN not set"
        Just config ->
            describe "Project Integration Tests" $
                projectLifecycleSpec config

projectLifecycleSpec :: TodoistConfig -> Spec
projectLifecycleSpec config = describe "Project lifecycle (create, get, delete)" $ do
    it "creates, retrieves, and deletes a project successfully" $ do
        -- Generate unique project name
        projectName <- pack <$> generateUniqueName "IntegTest-ProjectLifecycle"

        -- Create a project
        let projectCreate = newProject projectName
        createResult <- todoist config (addProject projectCreate)

        case createResult of
            Left err -> do
                putStrLn $ "Failed to create project: " <> show err
                createResult `shouldSatisfy` (\case Right _ -> True; _ -> False)
            Right projectId -> do
                -- Get the project by ID
                getResult <- todoist config (getProject projectId)

                case getResult of
                    Left err -> do
                        -- Cleanup before failing
                        void $ todoist config (deleteProject projectId)
                        putStrLn $ "Failed to get project: " <> show err
                        getResult `shouldSatisfy` (\case Right _ -> True; _ -> False)
                    Right (P.Project {_id = projId, _name = projName}) -> do
                        -- Verify the project details
                        let P.ProjectId {_id = pidId} = projectId
                        projId `shouldBe` pidId
                        projName `shouldBe` projectName

                        -- Cleanup and verify deletion
                        deleteResult <- todoist config (deleteProject projectId)
                        deleteResult `shouldSatisfy` (\case Right _ -> True; _ -> False)

                        -- Verify project no longer exists
                        verifyResult <- todoist config (getProject projectId)
                        verifyResult `shouldSatisfy` (\case Left _ -> True; _ -> False)
