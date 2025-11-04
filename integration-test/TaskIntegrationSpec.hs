{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TaskIntegrationSpec (spec) where

import Helpers
    ( assertSucceeds
    , buildTestTask
    , generateUniqueName
    , getTestConfig
    , liftTodoist
    )

import Web.Todoist.Builder
    ( runBuilder
    , setContent
    , setDescription
    , setDueDate
    , setPriority
    , setProjectId
    )
import qualified Web.Todoist.Domain.Project as P
import Web.Todoist.Domain.Task
    ( CompletedTasksQueryParam (..)
    , NewTask (..)
    , Task (..)
    , TaskId (..)
    , TaskParam (..)
    , TodoistTaskM (..)
    , addTaskQuickWithQuery
    , emptyTaskPatch
    , newMoveTask
    , taskFilterWithQuery
    )
import qualified Web.Todoist.Domain.Task as T
import Web.Todoist.Internal.Config (TodoistConfig)
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Runner (todoist)

import Control.Applicative (pure)
import Control.Exception (bracket)
import Control.Monad (forM_, mapM, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import qualified Data.List as L
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Data.Text (Text, pack)
import System.IO (IO, putStrLn)
import Test.Hspec (Spec, describe, it, pendingWith, runIO, shouldBe, shouldSatisfy)
import Text.Show (show)

import GHC.Base (mempty)

spec :: Spec
spec = do
    maybeConfig <- runIO getTestConfig
    case maybeConfig of
        Nothing ->
            it "requires TODOIST_TEST_API_TOKEN" $
                pendingWith "TODOIST_TEST_API_TOKEN not set"
        Just config -> do
            taskLifecycleSpec config
            taskCompletionSpec config
            getTasksSpec config
            updateTaskSpec config
            taskFilterSpec config
            moveTaskSpec config

taskLifecycleSpec :: TodoistConfig -> Spec
taskLifecycleSpec config = describe "Task lifecycle (create, get, delete)" $ do
    it "creates, retrieves, and deletes a task with all fields verified" $ do
        -- Generate unique names
        projectName <- pack <$> generateUniqueName "IntegTest-TaskLifecycle-Project"
        taskContent <- pack <$> generateUniqueName "IntegTest-TaskLifecycle-Task"

        -- Use withTestTask for automatic cleanup
        withTestTask config projectName taskContent $ \projectId taskId -> do
            -- Verify we can retrieve the task
            task <- liftTodoist config (getTask taskId)

            -- Extract task fields for verification
            let Task
                    { _id = retrievedId
                    , _content = retrievedContent
                    , _description = retrievedDescription
                    , _project_id = retrievedProjectId
                    } = task

            -- Verify task ID matches
            let TaskId {_id = expectedIdText} = taskId
            liftIO $ retrievedId `shouldBe` expectedIdText

            -- Verify content matches
            liftIO $ retrievedContent `shouldBe` taskContent

            -- Verify description was set
            liftIO $ retrievedDescription `shouldBe` "Test task description for integration testing"

            -- Verify project ID matches
            let P.ProjectId {_id = expectedProjectId} = projectId
            liftIO $ retrievedProjectId `shouldBe` expectedProjectId

            -- Test explicit delete (cleanup will handle if this fails)
            liftTodoist config (deleteTask taskId)

taskCompletionSpec :: TodoistConfig -> Spec
taskCompletionSpec config = describe "Task completion/uncompletion lifecycle" $ do
    it "marks a task as complete then reopens it" $ do
        -- Generate unique names
        projectName <- pack <$> generateUniqueName "IntegTest-TaskCompletion-Project"
        taskContent <- pack <$> generateUniqueName "IntegTest-TaskCompletion-Task"

        withTestTask config projectName taskContent $ \_ taskId -> do
            -- Verify task starts as not completed
            task1 <- liftTodoist config (getTask taskId)
            let Task {_completed_at = initialCompletedAt} = task1
            liftIO $ initialCompletedAt `shouldBe` Nothing

            -- Close the task
            liftTodoist config (closeTask taskId)

            -- Try to get the task after closing
            getResult <- liftIO $ todoist config (getTask taskId)
            case getResult of
                Right closedTask -> do
                    -- If we can still get it, verify it's marked as completed
                    let Task {_completed_at = completedAt} = closedTask
                    liftIO $ completedAt `shouldSatisfy` (\case Just _ -> True; Nothing -> False)
                Left _ -> do
                    -- If we can't get it, that's also acceptable (API behavior)
                    liftIO $ putStrLn "Task not retrievable after closing (expected API behavior)"

            -- Unclose the task
            liftTodoist config (uncloseTask taskId)

            -- Verify task is now uncompleted
            task2 <- liftTodoist config (getTask taskId)
            let Task {_completed_at = finalCompletedAt} = task2
            liftIO $ finalCompletedAt `shouldBe` Nothing

getTasksSpec :: TodoistConfig -> Spec
getTasksSpec config = describe "Get multiple tasks" $ do
    it "creates 3 tasks, retrieves them via getTasks, validates count and properties" $ do
        -- Generate unique names
        projectName <- pack <$> generateUniqueName "IntegTest-GetTasks-Project"
        baseName <- generateUniqueName "IntegTest-GetTasks-Task"

        let taskContent1 = pack $ baseName <> "-Task1"
        let taskContent2 = pack $ baseName <> "-Task2"
        let taskContent3 = pack $ baseName <> "-Task3"
        let taskContents = [taskContent1, taskContent2, taskContent3]

        withTestTasks config projectName taskContents $ \projectId taskIds -> do
            -- Get all tasks for this project
            let P.ProjectId {_id = projIdText} = projectId
            let taskParam =
                    T.setProjectId
                        projIdText
                        TaskParam
                            { project_id = Nothing
                            , section_id = Nothing
                            , parent_id = Nothing
                            , task_ids = []
                            , cursor = Nothing
                            , limit = Nothing
                            }

            tasks <- liftTodoist config (getTasks taskParam)

            -- Verify we got exactly 3 tasks
            let taskCount = L.length tasks
            liftIO $ taskCount `shouldBe` (3 :: Int)

            -- Extract task IDs and contents from results
            let taskIdsResult = L.map (\(Task {_id = tid}) -> TaskId {_id = tid}) tasks
            let taskContentsResult = L.map (\(Task {_content = content}) -> content) tasks

            -- Verify all 3 task IDs are present
            let [expectedId1, expectedId2, expectedId3] = taskIds
            liftIO $ (expectedId1 `L.elem` taskIdsResult) `shouldBe` True
            liftIO $ (expectedId2 `L.elem` taskIdsResult) `shouldBe` True
            liftIO $ (expectedId3 `L.elem` taskIdsResult) `shouldBe` True

            -- Verify all 3 task contents are present
            liftIO $ (taskContent1 `L.elem` taskContentsResult) `shouldBe` True
            liftIO $ (taskContent2 `L.elem` taskContentsResult) `shouldBe` True
            liftIO $ (taskContent3 `L.elem` taskContentsResult) `shouldBe` True

            -- Verify each task has the correct project_id
            forM_ tasks $ \(Task {_project_id = taskProjId}) -> do
                liftIO $ taskProjId `shouldBe` projIdText

updateTaskSpec :: TodoistConfig -> Spec
updateTaskSpec config = describe "Update task" $ do
    it "creates a task, updates its properties, verifies changes" $ do
        -- Generate unique names
        projectName <- pack <$> generateUniqueName "IntegTest-UpdateTask-Project"
        originalContent <- pack <$> generateUniqueName "IntegTest-UpdateTask-Original"

        withTestTask config projectName originalContent $ \projectId taskId -> do
            -- Verify initial state
            task1 <- liftTodoist config (getTask taskId)
            let Task
                    { _content = initialContent
                    , _description = initialDescription
                    , _priority = initialPriority
                    } = task1

            liftIO $ initialContent `shouldBe` originalContent
            liftIO $ initialDescription `shouldBe` "Test task description for integration testing"
            liftIO $ initialPriority `shouldBe` 1 -- default priority

            -- Update the task
            let updatedContent = originalContent <> "-Updated"
            let updatedDescription = "Updated task description"
            let updatedPriority = 3

            let taskPatch =
                    runBuilder
                        emptyTaskPatch
                        ( setContent updatedContent
                            <> setDescription updatedDescription
                            <> setPriority updatedPriority
                        )

            updatedNewTask <- liftTodoist config (updateTask taskId taskPatch)

            -- Verify the response contains updated values
            let NewTask
                    { _content = responseContent
                    , _description = responseDescription
                    , _priority = responsePriority
                    } = updatedNewTask

            liftIO $ responseContent `shouldBe` updatedContent
            liftIO $ responseDescription `shouldBe` updatedDescription
            liftIO $ responsePriority `shouldBe` updatedPriority

            -- Fetch the task again to verify persistence
            task2 <- liftTodoist config (getTask taskId)
            let Task
                    { _content = persistedContent
                    , _description = persistedDescription
                    , _priority = persistedPriority
                    , _project_id = persistedProjectId
                    } = task2

            liftIO $ persistedContent `shouldBe` updatedContent
            liftIO $ persistedDescription `shouldBe` updatedDescription
            liftIO $ persistedPriority `shouldBe` updatedPriority

            -- Verify project_id unchanged
            let P.ProjectId {_id = expectedProjectId} = projectId
            liftIO $ persistedProjectId `shouldBe` expectedProjectId

    it "supports partial updates (only updating specific fields)" $ do
        -- Generate unique names
        projectName <- pack <$> generateUniqueName "IntegTest-PartialUpdate-Project"
        taskContent <- pack <$> generateUniqueName "IntegTest-PartialUpdate-Task"

        withTestTask config projectName taskContent $ \_ taskId -> do
            -- Get initial state
            task1 <- liftTodoist config (getTask taskId)
            let Task
                    { _content = originalContent
                    , _description = originalDescription
                    } = task1

            -- Partial update: only change priority
            let taskPatch =
                    runBuilder
                        emptyTaskPatch
                        (setPriority 4)

            _ <- liftTodoist config (updateTask taskId taskPatch)

            -- Verify only priority changed
            task2 <- liftTodoist config (getTask taskId)
            let Task
                    { _content = finalContent
                    , _description = finalDescription
                    , _priority = finalPriority
                    } = task2

            liftIO $ finalPriority `shouldBe` 4
            -- Other fields should remain unchanged
            liftIO $ finalContent `shouldBe` originalContent
            liftIO $ finalDescription `shouldBe` originalDescription

taskFilterSpec :: TodoistConfig -> Spec
taskFilterSpec config = describe "Task filtering" $ do
    it "searches tasks using text filter" $ do
        -- Generate unique names with distinctive search term
        projectName <- pack <$> generateUniqueName "IntegTest-Filter-Project"
        searchTerm <- generateUniqueName "UNIQUE_SEARCHABLE_TERM"
        let taskContent = pack $ "Task with " <> searchTerm

        withTestTask config projectName taskContent $ \_ taskId -> do
            -- Search for tasks containing our unique term
            -- Use search: prefix for text search in Todoist filter syntax
            let filter = taskFilterWithQuery (pack $ "search: " <> searchTerm)
            taskIds <- liftTodoist config (getTasksByFilter filter)

            -- Verify our task is in the results
            let TaskId {_id = expectedIdText} = taskId
            let taskIdTexts = L.map (\(TaskId {_id = tid}) -> tid) taskIds
            liftIO $ (expectedIdText `L.elem` taskIdTexts) `shouldBe` True

    it "retrieves completed tasks by due date range" $ do
        -- Generate unique names
        projectName <- pack <$> generateUniqueName "IntegTest-CompletedDue-Project"
        taskContent <- pack <$> generateUniqueName "IntegTest-CompletedDue-Task"

        withTestTask config projectName taskContent $ \projectId taskId -> do
            -- Set a due date on the task first
            let taskPatch =
                    runBuilder
                        emptyTaskPatch
                        (setDueDate "2025-11-03")

            _ <- liftTodoist config (updateTask taskId taskPatch)

            -- Complete the task
            liftTodoist config (closeTask taskId)

            -- Query for completed tasks by due date
            -- Use a wide range to ensure we catch our task
            let CompletedTasksQueryParam
                    { since
                    , until
                    , workspace_id
                    , section_id
                    , parent_id
                    , filter_query
                    , filter_lang
                    , cursor
                    , limit
                    } = addTaskQuickWithQuery "2025-11-01" "2025-11-30"
            let P.ProjectId {_id = projIdText} = projectId
            let queryParamWithProject =
                    CompletedTasksQueryParam
                        { since = since
                        , until = until
                        , workspace_id = workspace_id
                        , project_id = Just projIdText
                        , section_id = section_id
                        , parent_id = parent_id
                        , filter_query = filter_query
                        , filter_lang = filter_lang
                        , cursor = cursor
                        , limit = limit
                        }

            completedTaskIds <- liftTodoist config (getCompletedTasksByDueDate queryParamWithProject)

            -- Verify our task is in the results
            let TaskId {_id = expectedIdText} = taskId
            let taskIdTexts = L.map (\(TaskId {_id = tid}) -> tid) completedTaskIds
            liftIO $ (expectedIdText `L.elem` taskIdTexts) `shouldBe` True

            -- Unclose for cleanup
            liftTodoist config (uncloseTask taskId)

    it "retrieves completed tasks by completion date range" $ do
        -- Generate unique names
        projectName <- pack <$> generateUniqueName "IntegTest-CompletedDate-Project"
        taskContent <- pack <$> generateUniqueName "IntegTest-CompletedDate-Task"

        withTestTask config projectName taskContent $ \projectId taskId -> do
            -- Complete the task
            liftTodoist config (closeTask taskId)

            -- Query for completed tasks by completion date (today)
            -- Use a wide range to ensure we catch our task
            let CompletedTasksQueryParam
                    { since
                    , until
                    , workspace_id
                    , section_id
                    , parent_id
                    , filter_query
                    , filter_lang
                    , cursor
                    , limit
                    } = addTaskQuickWithQuery "2025-11-01" "2025-11-30"
            let P.ProjectId {_id = projIdText} = projectId
            let queryParamWithProject =
                    CompletedTasksQueryParam
                        { since = since
                        , until = until
                        , workspace_id = workspace_id
                        , project_id = Just projIdText
                        , section_id = section_id
                        , parent_id = parent_id
                        , filter_query = filter_query
                        , filter_lang = filter_lang
                        , cursor = cursor
                        , limit = limit
                        }

            completedTaskIds <- liftTodoist config (getCompletedTasksByCompletionDate queryParamWithProject)

            -- Verify our task is in the results
            let TaskId {_id = expectedIdText} = taskId
            let taskIdTexts = L.map (\(TaskId {_id = tid}) -> tid) completedTaskIds
            liftIO $ (expectedIdText `L.elem` taskIdTexts) `shouldBe` True

            -- Unclose for cleanup
            liftTodoist config (uncloseTask taskId)

moveTaskSpec :: TodoistConfig -> Spec
moveTaskSpec config = describe "Move task between projects" $ do
    it "creates two projects, moves a task from one to the other" $ do
        -- Generate unique names
        project1Name <- pack <$> generateUniqueName "IntegTest-MoveTask-Project1"
        project2Name <- pack <$> generateUniqueName "IntegTest-MoveTask-Project2"
        taskContent <- pack <$> generateUniqueName "IntegTest-MoveTask-Task"

        -- Create project 1 with task
        withTestTask config project1Name taskContent $ \project1Id taskId -> do
            -- Create project 2
            project2Id <- liftTodoist config (P.addProject $ runBuilder (P.newProject project2Name) mempty)

            -- Verify task is in project 1
            task1 <- liftTodoist config (getTask taskId)
            let Task {_project_id = originalProjectId} = task1
            let P.ProjectId {_id = project1IdText} = project1Id
            liftIO $ originalProjectId `shouldBe` project1IdText

            -- Move task to project 2
            let P.ProjectId {_id = project2IdText} = project2Id
            let moveTaskData = runBuilder newMoveTask (setProjectId project2IdText)

            movedTaskId <- liftTodoist config (moveTask taskId moveTaskData)

            -- Verify returned task ID matches
            let TaskId {_id = expectedIdText} = taskId
            let TaskId {_id = movedIdText} = movedTaskId
            liftIO $ movedIdText `shouldBe` expectedIdText

            -- Verify task is now in project 2
            task2 <- liftTodoist config (getTask taskId)
            let Task {_project_id = newProjectId} = task2
            liftIO $ newProjectId `shouldBe` project2IdText

            -- Clean up project 2 (task will be deleted by withTestTask cleanup)
            liftTodoist config (P.deleteProject project2Id)

{- | Create a test project and task, run an action with the task ID, then clean up
Uses bracket to ensure cleanup happens even if the action fails
Tasks are deleted before the project
-}
withTestTask ::
    TodoistConfig ->
    Text -> -- project name
    Text -> -- task content
    (P.ProjectId -> TaskId -> ExceptT TodoistError IO a) ->
    IO ()
withTestTask config projectName taskContent action = do
    let createResources = do
            liftIO $ putStrLn $ "Creating test project: " <> show projectName
            projectId <- liftTodoist config (P.addProject $ runBuilder (P.newProject projectName) mempty)

            liftIO $ putStrLn $ "Creating test task: " <> show taskContent
            let P.ProjectId {_id = projIdText} = projectId
            let taskCreate = buildTestTask taskContent projIdText
            newTaskResult <- liftTodoist config (addTask taskCreate)
            let NewTask {_id = newTaskIdText} = newTaskResult
            let taskId = TaskId {_id = newTaskIdText}

            pure (projectId, taskId)

    let deleteResources (projectId, taskId) = do
            liftIO $ putStrLn $ "Cleaning up test task: " <> show taskContent
            void $ todoist config (deleteTask taskId)

            liftIO $ putStrLn $ "Cleaning up test project: " <> show projectName
            void $ todoist config (P.deleteProject projectId)

    let runAction (projectId, taskId) = void $ assertSucceeds $ action projectId taskId

    bracket (assertSucceeds createResources) deleteResources runAction

{- | Create a test project and multiple tasks, run an action with their IDs, then clean up
Ensures all tasks are deleted before the project is deleted
-}
withTestTasks ::
    TodoistConfig ->
    Text -> -- project name
    [Text] -> -- task contents
    (P.ProjectId -> [TaskId] -> ExceptT TodoistError IO a) ->
    IO ()
withTestTasks config projectName taskContents action = do
    let createResources = do
            liftIO $ putStrLn $ "Creating test project: " <> show projectName
            projectId <- liftTodoist config (P.addProject $ runBuilder (P.newProject projectName) mempty)

            liftIO $ putStrLn $ "Creating " <> show (L.length taskContents) <> " test tasks"
            let P.ProjectId {_id = projIdText} = projectId
            taskIds <-
                mapM
                    ( \content -> do
                        let taskCreate = buildTestTask content projIdText
                        newTaskResult <- liftTodoist config (addTask taskCreate)
                        let NewTask {_id = newTaskIdText} = newTaskResult
                        pure $ TaskId {_id = newTaskIdText}
                    )
                    taskContents

            pure (projectId, taskIds)

    let deleteResources (projectId, taskIds) = do
            liftIO $ putStrLn $ "Cleaning up " <> show (L.length taskIds) <> " test tasks"
            forM_ taskIds $ \taskId ->
                void $ todoist config (deleteTask taskId)

            liftIO $ putStrLn $ "Cleaning up test project: " <> show projectName
            void $ todoist config (P.deleteProject projectId)

    let runAction (projectId, taskIds) = void $ assertSucceeds $ action projectId taskIds

    bracket (assertSucceeds createResources) deleteResources runAction
