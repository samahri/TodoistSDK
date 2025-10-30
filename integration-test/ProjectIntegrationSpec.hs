{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ProjectIntegrationSpec (spec) where

import Helpers (generateUniqueName, getTestConfig, withTestProject, withTestProjects, liftTodoist)

import Web.Todoist.Domain.Project (TodoistProjectM (..), newProject)
import qualified Web.Todoist.Domain.Project as P
import Web.Todoist.Runner (todoist)

import Control.Monad (void, return, mapM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import qualified Data.List as L
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Data.Text (pack, isInfixOf)
import qualified Data.Text as T
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
        Just config -> do
            describe "Project Integration Tests" $ do
                projectLifecycleSpec config
                archiveUnarchiveSpec config
                getAllProjectsSpec config
                getProjectCollaboratorsSpec config

projectLifecycleSpec :: TodoistConfig -> Spec
projectLifecycleSpec config = describe "Project lifecycle (create, get, delete)" $ do
    it "creates, retrieves, and deletes a project successfully" $ do
        -- Generate unique project name
        projectName <- pack <$> generateUniqueName "IntegTest-ProjectLifecycle"

        -- Use withTestProject to handle creation and cleanup
        withTestProject config projectName $ \projectId -> do
            -- Get the project by ID
            P.Project {_id = projId, _name = projName} <- liftTodoist config (getProject projectId)

            -- Verify the project details
            let P.ProjectId {_id = pidId} = projectId
            liftIO $ projId `shouldBe` pidId
            liftIO $ projName `shouldBe` projectName

            -- Delete the project
            liftTodoist config (deleteProject projectId)

            -- Verify project no longer exists (should get an error)
            verifyResult <- liftIO $ todoist config (getProject projectId)
            liftIO $ verifyResult `shouldSatisfy` (\case Left _ -> True; _ -> False)

archiveUnarchiveSpec :: TodoistConfig -> Spec
archiveUnarchiveSpec config = describe "Project archive/unarchive lifecycle" $ do
    it "archives and unarchives a project successfully" $ do
        -- Generate unique project name
        projectName <- pack <$> generateUniqueName "IntegTest-ArchiveUnarchive"

        -- Use withTestProject to handle creation and cleanup
        withTestProject config projectName $ \projectId -> do
            -- Archive the project
            archivedId <- liftTodoist config (archiveProject projectId)

            -- Verify returned ID matches
            let P.ProjectId {_id = pidId} = projectId
            let P.ProjectId {_id = archivedPidId} = archivedId
            liftIO $ archivedPidId `shouldBe` pidId

            -- Verify project is archived by getting it
            P.Project {_is_archived = isArchived} <- liftTodoist config (getProject projectId)
            liftIO $ isArchived `shouldBe` True

            -- Unarchive the project
            unarchivedId <- liftTodoist config (unarchiveProject projectId)

            -- Verify returned ID matches
            let P.ProjectId {_id = unarchivedPidId} = unarchivedId
            liftIO $ unarchivedPidId `shouldBe` pidId

            -- Verify project is no longer archived
            P.Project {_is_archived = isStillArchived} <- liftTodoist config (getProject projectId)
            liftIO $ isStillArchived `shouldBe` False

getAllProjectsSpec :: TodoistConfig -> Spec
getAllProjectsSpec config = describe "Get all projects" $ do
    it "creates 3 projects, retrieves them via getAllProjects, validates count and properties, then deletes them" $ do
        -- Generate unique base name for this test run
        baseName <- generateUniqueName "IntegTest-GetAll"

        -- Create 3 projects with unique names
        let projectName1 = pack $ baseName <> "-Project1"
        let projectName2 = pack $ baseName <> "-Project2"
        let projectName3 = pack $ baseName <> "-Project3"
        let projectNames = [projectName1, projectName2, projectName3]

        -- Use withTestProjects to handle creation and cleanup
        withTestProjects config projectNames $ \[projectId1, projectId2, projectId3] -> do
            -- Get all projects
            allProjects <- liftTodoist config getAllProjects

            -- Filter to only our test projects
            let testPrefix = pack baseName
            let ourProjects = L.filter (\(P.Project {_name = n}) -> testPrefix `isInfixOf` n) allProjects

            -- Verify we got exactly 3 projects
            let projectCount = L.length ourProjects
            liftIO $ projectCount `shouldBe` (3 :: Int)

            -- Extract the project names from our filtered list
            let projectNamesResult = L.map (\(P.Project {_name = n}) -> n) ourProjects

            -- Verify all 3 project names are present
            liftIO $ (projectName1 `L.elem` projectNamesResult) `shouldBe` True
            liftIO $ (projectName2 `L.elem` projectNamesResult) `shouldBe` True
            liftIO $ (projectName3 `L.elem` projectNamesResult) `shouldBe` True

            -- Verify each project has the expected properties
            let project1Maybe = L.find (\(P.Project {_name = n}) -> n == projectName1) ourProjects
            let project2Maybe = L.find (\(P.Project {_name = n}) -> n == projectName2) ourProjects
            let project3Maybe = L.find (\(P.Project {_name = n}) -> n == projectName3) ourProjects

            -- Verify all projects were found
            case (project1Maybe, project2Maybe, project3Maybe) of
                (Just (P.Project {_id = id1, _name = name1}), 
                  Just (P.Project {_id = id2, _name = name2}), 
                  Just (P.Project {_id = id3, _name = name3})) -> do
                    -- Verify project IDs match
                    let P.ProjectId {_id = pid1} = projectId1
                    let P.ProjectId {_id = pid2} = projectId2
                    let P.ProjectId {_id = pid3} = projectId3

                    liftIO $ id1 `shouldBe` pid1
                    liftIO $ id2 `shouldBe` pid2
                    liftIO $ id3 `shouldBe` pid3

                    -- Verify project names match
                    liftIO $ name1 `shouldBe` projectName1
                    liftIO $ name2 `shouldBe` projectName2
                    liftIO $ name3 `shouldBe` projectName3
                _ -> do
                    liftIO $ putStrLn "Failed to find all 3 projects in the filtered list"
                    liftIO $ False `shouldBe` True

getProjectCollaboratorsSpec :: TodoistConfig -> Spec
getProjectCollaboratorsSpec config = describe "Get project collaborators" $ do
    it "retrieves collaborators for a project and validates their structure" $ do
        -- Generate unique project name
        projectName <- pack <$> generateUniqueName "IntegTest-Collaborators"

        -- Use withTestProject to handle creation and cleanup
        withTestProject config projectName $ \projectId -> do
            -- Get project collaborators
            collaborators <- liftTodoist config (getProjectCollaborators projectId)

            -- Validate structure of each collaborator if any exist
            -- Note: Personal projects may have no collaborators, which is valid
            let validateCollaborator (P.Collaborator {_id = collabId, _name = collabName, _email = collabEmail}) = do
                    -- Verify _id is non-empty
                    T.null collabId `shouldBe` False

                    -- Verify _name is non-empty
                    T.null collabName `shouldBe` False

                    -- Verify _email is non-empty and contains '@'
                    T.null collabEmail `shouldBe` False
                    ("@" `T.isInfixOf` collabEmail) `shouldBe` True

            -- Validate each collaborator's structure
            liftIO $ mapM_ validateCollaborator collaborators
