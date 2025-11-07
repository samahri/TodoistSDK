{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ProjectIntegrationSpec (spec) where

import Helpers (assertSucceeds, buildTestProject, generateUniqueName, getTestConfig, liftTodoist)

import Web.Todoist.Builder (runBuilder, setDescription)
import Web.Todoist.Domain.Project
    ( ProjectCreate
    , ProjectUpdate (..)
    , TodoistProjectM (..)
    , newProject
    )
import qualified Web.Todoist.Domain.Project as P
import Web.Todoist.Domain.Types (ProjectId (..))
import Web.Todoist.Internal.Config (TodoistConfig)
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Internal.Types
    ( Action (..)
    , ProjectPermissions (..)
    , RoleActions (..)
    )
import Web.Todoist.Runner (todoist)

import Control.Exception (bracket)
import Control.Monad (forM_, mapM, mapM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Eq ((==))
import Data.Function (const, ($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import qualified Data.List as L
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Data.Text (Text, isInfixOf, pack)
import qualified Data.Text as T
import System.IO (IO, putStrLn)
import Test.Hspec (Spec, describe, it, pendingWith, runIO, shouldBe, shouldSatisfy)
import Text.Show (show)

import GHC.Base (mempty, undefined)

spec :: Spec
spec = do
    maybeConfig <- runIO getTestConfig
    case maybeConfig of
        Nothing ->
            it "requires TODOIST_TEST_API_TOKEN" $
                pendingWith "TODOIST_TEST_API_TOKEN not set"
        Just config -> do
            projectLifecycleSpec config
            archiveUnarchiveSpec config
            getAllProjectsSpec config
            getProjectCollaboratorsSpec config
            getProjectPermissionsSpec config
            updateProjectSpec config

projectLifecycleSpec :: TodoistConfig -> Spec
projectLifecycleSpec config = describe "Project lifecycle (create, get, delete)" $ do
    it "creates, retrieves, and deletes a project with all fields set" $ do
        -- Generate unique project name
        newProjectName <- pack <$> generateUniqueName "IntegTest-ProjectLifecycle"

        -- Build a complete project with all fields using the Builder pattern
        let testProject = buildTestProject newProjectName

        -- Extract expected values from testProject for verification
        let P.ProjectCreate
                { _name = expectedName
                , _description = expectedDescription
                , _view_style = expectedViewStyle
                , _is_favorite = expectedIsFavorite
                } = testProject

        -- Use withTestProjectCreate to handle creation and cleanup
        withTestProjectCreate config testProject $ \projectId -> do
            -- Get the project by ID and verify all fields
            project <- liftTodoist config (getProject projectId)

            let P.Project
                    { _id = projId
                    , _name = projName
                    , _description = projDescription
                    , _view_style = projViewStyle
                    , _is_favorite = projIsFavorite
                    } = project

            -- Verify the project details match expected values
            let ProjectId {getProjectId = pidId} = projectId
            liftIO $ projId `shouldBe` pidId
            liftIO $ projName `shouldBe` expectedName

            -- Verify description field matches testProject
            liftIO $ Just projDescription `shouldBe` expectedDescription

            -- Verify view_style field matches testProject
            liftIO $ Just projViewStyle `shouldBe` expectedViewStyle

            -- Verify is_favorite field matches testProject
            liftIO $ projIsFavorite `shouldBe` expectedIsFavorite

            -- Test deletion within the action
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
            let ProjectId {getProjectId = pidId} = projectId
            let ProjectId {getProjectId = archivedPidId} = archivedId
            liftIO $ archivedPidId `shouldBe` pidId

            -- Verify project is archived by getting it
            P.Project {_is_archived = isArchived} <- liftTodoist config (getProject projectId)
            liftIO $ isArchived `shouldBe` True

            -- Unarchive the project
            unarchivedId <- liftTodoist config (unarchiveProject projectId)

            -- Verify returned ID matches
            let ProjectId {getProjectId = unarchivedPidId} = unarchivedId
            liftIO $ unarchivedPidId `shouldBe` pidId

            -- Verify project is no longer archived
            P.Project {_is_archived = isStillArchived} <- liftTodoist config (getProject projectId)
            liftIO $ isStillArchived `shouldBe` False

getAllProjectsSpec :: TodoistConfig -> Spec
getAllProjectsSpec config = describe "Get all projects" $ do
    it
        "creates 3 projects, retrieves them via getAllProjects, validates count and properties, then deletes them"
        $ do
            -- Generate unique base name for this test run
            baseName <- generateUniqueName "IntegTest-GetAll"

            -- Create 3 projects with unique names
            let projectName1 = pack $ baseName <> "-Project1"
            let projectName2 = pack $ baseName <> "-Project2"
            let projectName3 = pack $ baseName <> "-Project3"
            let projectNames = [projectName1, projectName2, projectName3]

            -- Use withTestProjects to handle creation and cleanup
            withTestProjects config projectNames $ \case
                [projectId1, projectId2, projectId3] -> do
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
                        ( Just (P.Project {_id = id1, _name = name1})
                            , Just (P.Project {_id = id2, _name = name2})
                            , Just (P.Project {_id = id3, _name = name3})
                            ) -> do
                                -- Verify project IDs match
                                let ProjectId {getProjectId = pid1} = projectId1
                                let ProjectId {getProjectId = pid2} = projectId2
                                let ProjectId {getProjectId = pid3} = projectId3

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
                _ -> undefined -- impossible case

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

getProjectPermissionsSpec :: TodoistConfig -> Spec
getProjectPermissionsSpec config = describe "Get project permissions" $ do
    it "creates 3 projects, retrieves permissions, validates structure, then deletes projects" $ do
        -- Generate unique base name for this test run
        baseName <- generateUniqueName "IntegTest-Permissions"

        -- Create 3 project names
        let projectName1 = pack $ baseName <> "-Project1"
        let projectName2 = pack $ baseName <> "-Project2"
        let projectName3 = pack $ baseName <> "-Project3"
        let projectNames = [projectName1, projectName2, projectName3]

        -- Use withTestProjects to handle creation and cleanup
        withTestProjects config projectNames $ \projectIds -> do
            -- Verify we created 3 projects
            liftIO $ L.length projectIds `shouldBe` (3 :: Int)

            -- Get permissions (static endpoint - doesn't use project IDs)
            permissions <- liftTodoist config getProjectPermissions

            -- Validate structure exists
            liftIO $ do
                -- Both arrays should be present (may be empty but must exist)
                p_project_collaborator_actions permissions `shouldSatisfy` const True
                p_workspace_collaborator_actions permissions `shouldSatisfy` const True

                -- If there are role actions, validate their structure
                let validateRoleAction (RoleActions {p_name = roleName, p_actions = actions}) = do
                        -- p_name is type-safe CollaboratorRole - if it parsed, it's valid
                        -- Don't check for specific role, just that it parsed correctly
                        roleName `shouldSatisfy` const True
                        -- Validate each action has a non-empty name
                        forM_ actions $ \(Action {p_name = actionName}) -> do
                            T.null actionName `shouldBe` False

                -- Validate all roles in both arrays
                mapM_ validateRoleAction (p_project_collaborator_actions permissions)
                mapM_ validateRoleAction (p_workspace_collaborator_actions permissions)

-- Projects will be automatically deleted by withTestProjects bracket

updateProjectSpec :: TodoistConfig -> Spec
updateProjectSpec config = describe "Update project" $ do
    it "creates a project, updates its properties, verifies changes, then deletes it" $ do
        -- Generate unique project name
        originalName <- pack <$> generateUniqueName "IntegTest-Update-Original"

        -- Define description values
        let originalDescription = "Original description"
        let updatedDescription = "Updated description"

        -- Create initial project with specific properties
        let initialProject = runBuilder (newProject originalName) (setDescription originalDescription)

        withTestProjectCreate config initialProject $ \projectId -> do
            -- Verify initial state
            project1 <- liftTodoist config (getProject projectId)
            liftIO $ do
                let P.Project {P._name = proj1Name, P._description = proj1Desc, P._is_favorite = proj1Fav} = project1
                proj1Name `shouldBe` originalName
                proj1Desc `shouldBe` originalDescription
                proj1Fav `shouldBe` False -- default from newProject

            -- Update the project (change name, description, and favorite status)
            let updatedName = originalName <> "-Updated"
            let projectUpdate =
                    ProjectUpdate
                        { _name = Just updatedName
                        , _description = Just updatedDescription
                        , _color = Nothing -- don't change color
                        , _is_favorite = Just True
                        , _view_style = Nothing -- don't change view style
                        }

            updatedProject <- liftTodoist config (updateProject projectId projectUpdate)

            -- Verify the response contains updated values
            liftIO $ do
                let P.Project
                        { P._name = updatedProjName
                        , P._description = updatedProjDesc
                        , P._is_favorite = updatedProjFav
                        } = updatedProject
                updatedProjName `shouldBe` updatedName
                updatedProjDesc `shouldBe` updatedDescription
                updatedProjFav `shouldBe` True

            -- Fetch the project again to double-check persistence
            project2 <- liftTodoist config (getProject projectId)
            liftIO $ do
                let P.Project
                        { P._name = proj2Name
                        , P._description = proj2Desc
                        , P._is_favorite = proj2Fav
                        , P._view_style = proj2ViewStyle
                        } = project2
                let P.Project {P._view_style = proj1ViewStyle} = project1
                proj2Name `shouldBe` updatedName
                proj2Desc `shouldBe` updatedDescription
                proj2Fav `shouldBe` True
                -- Verify unchanged fields remain unchanged
                proj2ViewStyle `shouldBe` proj1ViewStyle

    it "supports partial updates (only updating specific fields)" $ do
        -- Generate unique project name
        projectName <- pack <$> generateUniqueName "IntegTest-PartialUpdate"

        -- Define initial description
        let initialDescription = "Initial description"

        -- Create initial project
        let initialProject = runBuilder (newProject projectName) (setDescription initialDescription)

        withTestProjectCreate config initialProject $ \projectId -> do
            -- Get initial state
            project1 <- liftTodoist config (getProject projectId)
            let P.Project {P._description = originalDescription} = project1

            -- Partial update: only change is_favorite
            let partialUpdate =
                    ProjectUpdate
                        { _name = Nothing
                        , _description = Nothing
                        , _color = Nothing
                        , _is_favorite = Just True
                        , _view_style = Nothing
                        }

            updatedProject <- liftTodoist config (updateProject projectId partialUpdate)

            -- Verify only is_favorite changed
            liftIO $ do
                let P.Project {P._is_favorite = updatedFav, P._name = updatedName, P._description = updatedDesc} = updatedProject
                updatedFav `shouldBe` True
                -- Other fields should remain unchanged
                updatedName `shouldBe` projectName
                updatedDesc `shouldBe` originalDescription

{- | Create a test project from a ProjectCreate, run an action with its ID, then delete it
Uses bracket to ensure cleanup happens even if the action fails
The action runs in ExceptT for clean error handling
-}
withTestProjectCreate ::
    TodoistConfig -> ProjectCreate -> (ProjectId -> ExceptT TodoistError IO a) -> IO ()
withTestProjectCreate config projectCreate action = do
    let createProject = do
            liftTodoist config (addProject projectCreate)

    let deleteProject' projectId = do
            void $ todoist config (Web.Todoist.Domain.Project.deleteProject projectId)

    let runAction projectId = void $ assertSucceeds $ action projectId

    bracket (assertSucceeds createProject) deleteProject' runAction

{- | Create a test project, run an action with its ID, then delete it
Uses bracket to ensure cleanup happens even if the action fails
The action runs in ExceptT for clean error handling
-}
withTestProject :: TodoistConfig -> Text -> (ProjectId -> ExceptT TodoistError IO a) -> IO ()
withTestProject config projectName action = do
    let createProject = do
            liftIO $ putStrLn $ "Creating test project: " <> show projectName
            liftTodoist config (addProject $ runBuilder (newProject projectName) mempty)

    let deleteProject' projectId = do
            liftIO $ putStrLn $ "Cleaning up test project: " <> show projectName
            void $ todoist config (Web.Todoist.Domain.Project.deleteProject projectId)

    let runAction projectId = void $ assertSucceeds $ action projectId

    bracket (assertSucceeds createProject) deleteProject' runAction

{- | Create multiple test projects, run an action with their IDs, then delete all
Uses bracket to ensure cleanup of all projects even if the action fails
The action runs in ExceptT for clean error handling
-}
withTestProjects :: TodoistConfig -> [Text] -> ([ProjectId] -> ExceptT TodoistError IO a) -> IO ()
withTestProjects config projectNames action = do
    let createProjects = do
            liftIO $ putStrLn $ "Creating " <> show (L.length projectNames) <> " test projects"
            mapM (\name -> liftTodoist config (addProject $ runBuilder (newProject name) mempty)) projectNames

    let deleteProjects projectIds = do
            liftIO $ putStrLn $ "Cleaning up " <> show (L.length projectIds) <> " test projects"
            mapM_ (todoist config . Web.Todoist.Domain.Project.deleteProject) projectIds

    let runAction projectIds = void $ assertSucceeds $ action projectIds

    bracket (assertSucceeds createProjects) deleteProjects runAction
