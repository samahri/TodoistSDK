{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module CommentIntegrationSpec (spec) where

import Helpers
    ( assertSucceeds
    , buildTestTask
    , generateUniqueName
    , getTestConfig
    , liftTodoist
    )
import Web.Todoist.Builder (runBuilder, setProjectId, setTaskId)
import Web.Todoist.Domain.Comment
    ( Comment (..)
    , CommentCreate
    , CommentId (..)
    , CommentParam (..)
    , TodoistCommentM (..)
    , newComment
    , newCommentUpdate
    )
import qualified Web.Todoist.Domain.Project as P
import Web.Todoist.Domain.Task (NewTask (..), TaskId (..), TodoistTaskM (..))
import Web.Todoist.Internal.Config (TodoistConfig)
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Runner (todoist)

import Control.Applicative (pure)
import Control.Exception (bracket)
import Control.Monad (forM_, mapM, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Bool (Bool (..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import qualified Data.List as L
import Data.Maybe (Maybe (..))
import Data.Ord ((>=))
import Data.Semigroup ((<>))
import Data.Text (Text, pack)
import GHC.Base (mempty)
import System.IO (IO, putStrLn)
import Test.Hspec (Spec, describe, it, pendingWith, runIO, shouldBe, shouldSatisfy)
import Text.Show (show)

spec :: Spec
spec = do
    maybeConfig <- runIO getTestConfig
    case maybeConfig of
        Nothing ->
            it "requires TODOIST_TEST_API_TOKEN" $
                pendingWith "TODOIST_TEST_API_TOKEN not set"
        Just config -> do
            commentOnProjectLifecycleSpec config
            commentOnTaskLifecycleSpec config
            updateCommentSpec config
            getCommentsSpec config
            getSingleCommentSpec config

commentOnProjectLifecycleSpec :: TodoistConfig -> Spec
commentOnProjectLifecycleSpec config = describe "Comment on project lifecycle (create, get, delete)" $ do
    it "creates, retrieves, and deletes a comment on a project" $ do
        -- Generate unique names
        projectName <- pack <$> generateUniqueName "IntegTest-CommentProject-Project"
        commentContent <- pack <$> generateUniqueName "IntegTest-CommentProject-Comment"

        -- Use withTestProjectComment for automatic cleanup
        withTestProjectComment config projectName commentContent $ \projectId commentId -> do
            -- Verify we can retrieve the comment
            comment <- liftTodoist config (getComment commentId)

            -- Extract comment fields for verification
            let Comment
                    { _id = retrievedId
                    , _content = retrievedContent
                    , _project_id = retrievedProjectId
                    , _task_id = retrievedTaskId
                    } = comment

            -- Verify comment ID matches
            let CommentId {_id = expectedIdText} = commentId
            liftIO $ retrievedId `shouldBe` expectedIdText

            -- Verify content matches
            liftIO $ retrievedContent `shouldBe` commentContent

            -- Verify project ID matches and task ID is Nothing
            let P.ProjectId {_id = expectedProjectId} = projectId
            liftIO $ retrievedProjectId `shouldBe` Just expectedProjectId
            liftIO $ retrievedTaskId `shouldBe` Nothing

            -- Test explicit delete (cleanup will handle if this fails)
            liftTodoist config (deleteComment commentId)

commentOnTaskLifecycleSpec :: TodoistConfig -> Spec
commentOnTaskLifecycleSpec config = describe "Comment on task lifecycle (create, get, delete)" $ do
    it "creates, retrieves, and deletes a comment on a task" $ do
        -- Generate unique names
        projectName <- pack <$> generateUniqueName "IntegTest-CommentTask-Project"
        taskContent <- pack <$> generateUniqueName "IntegTest-CommentTask-Task"
        commentContent <- pack <$> generateUniqueName "IntegTest-CommentTask-Comment"

        -- Use withTestTaskComment for automatic cleanup
        withTestTaskComment config projectName taskContent commentContent $ \taskId commentId -> do
            -- Verify we can retrieve the comment
            comment <- liftTodoist config (getComment commentId)

            -- Extract comment fields for verification
            let Comment
                    { _id = retrievedId
                    , _content = retrievedContent
                    , _task_id = retrievedTaskId
                    , _project_id = retrievedProjectId
                    } = comment

            -- Verify comment ID matches
            let CommentId {_id = expectedIdText} = commentId
            liftIO $ retrievedId `shouldBe` expectedIdText

            -- Verify content matches
            liftIO $ retrievedContent `shouldBe` commentContent

            -- Verify task ID matches and project ID is Nothing
            let TaskId {_id = expectedTaskId} = taskId
            liftIO $ retrievedTaskId `shouldBe` Just expectedTaskId
            liftIO $ retrievedProjectId `shouldBe` Nothing

            -- Test explicit delete (cleanup will handle if this fails)
            liftTodoist config (deleteComment commentId)

updateCommentSpec :: TodoistConfig -> Spec
updateCommentSpec config = describe "Update comment" $ do
    it "creates a comment, updates its content, verifies changes" $ do
        -- Generate unique names
        projectName <- pack <$> generateUniqueName "IntegTest-UpdateComment-Project"
        originalContent <- pack <$> generateUniqueName "IntegTest-UpdateComment-Original"

        withTestProjectComment config projectName originalContent $ \_ commentId -> do
            -- Verify initial state
            comment1 <- liftTodoist config (getComment commentId)
            let Comment {_content = initialContent} = comment1
            liftIO $ initialContent `shouldBe` originalContent

            -- Update the comment
            let updatedContent = originalContent <> "-Updated"
            let commentUpdate = runBuilder (newCommentUpdate updatedContent) mempty

            updatedComment <- liftTodoist config (updateComment commentId commentUpdate)

            -- Verify the response contains updated value
            let Comment {_content = responseContent} = updatedComment
            liftIO $ responseContent `shouldBe` updatedContent

            -- Fetch the comment again to verify persistence
            comment2 <- liftTodoist config (getComment commentId)
            let Comment {_content = persistedContent} = comment2
            liftIO $ persistedContent `shouldBe` updatedContent

getCommentsSpec :: TodoistConfig -> Spec
getCommentsSpec config = describe "Get multiple comments" $ do
    it "creates 3 comments on a project, retrieves them via getComments" $ do
        -- Generate unique names
        projectName <- pack <$> generateUniqueName "IntegTest-GetComments-Project"
        baseName <- generateUniqueName "IntegTest-GetComments-Comment"

        let commentContent1 = pack $ baseName <> "-Comment1"
        let commentContent2 = pack $ baseName <> "-Comment2"
        let commentContent3 = pack $ baseName <> "-Comment3"
        let commentContents = [commentContent1, commentContent2, commentContent3]

        withTestProjectComments config projectName commentContents $ \projectId commentIds -> do
            -- Get all comments for this project
            let P.ProjectId {_id = projIdText} = projectId
            let commentParam =
                    CommentParam
                        { project_id = Just projIdText
                        , task_id = Nothing
                        , cursor = Nothing
                        , limit = Nothing
                        , public_key = Nothing
                        }

            comments <- liftTodoist config (getComments commentParam)

            -- Verify we got at least 3 comments (there might be more from previous tests)
            let commentCount = L.length comments
            liftIO $ commentCount `shouldSatisfy` (\n -> n >= (3 :: Int))

            -- Extract comment IDs and contents from results
            let commentIdsResult = L.map (\(Comment {_id = cid}) -> CommentId {_id = cid}) comments
            let commentContentsResult = L.map (\(Comment {_content = content}) -> content) comments

            -- Verify all 3 comment IDs are present
            let [expectedId1, expectedId2, expectedId3] = commentIds
            liftIO $ (expectedId1 `L.elem` commentIdsResult) `shouldBe` True
            liftIO $ (expectedId2 `L.elem` commentIdsResult) `shouldBe` True
            liftIO $ (expectedId3 `L.elem` commentIdsResult) `shouldBe` True

            -- Verify all 3 comment contents are present
            liftIO $ (commentContent1 `L.elem` commentContentsResult) `shouldBe` True
            liftIO $ (commentContent2 `L.elem` commentContentsResult) `shouldBe` True
            liftIO $ (commentContent3 `L.elem` commentContentsResult) `shouldBe` True

            -- Verify each comment has the correct project_id
            forM_ comments $ \(Comment {_project_id = commentProjId, _task_id = commentTaskId}) -> do
                case (commentProjId, commentTaskId) of
                    (Just pid, Nothing) -> liftIO $ pid `shouldBe` projIdText
                    _ -> pure () -- Skip comments that don't match our filter

getSingleCommentSpec :: TodoistConfig -> Spec
getSingleCommentSpec config = describe "Get single comment by ID" $ do
    it "creates a comment and retrieves it by ID" $ do
        -- Generate unique names
        projectName <- pack <$> generateUniqueName "IntegTest-GetSingleComment-Project"
        commentContent <- pack <$> generateUniqueName "IntegTest-GetSingleComment-Comment"

        withTestProjectComment config projectName commentContent $ \projectId commentId -> do
            -- Get the comment by ID
            comment <- liftTodoist config (getComment commentId)

            -- Verify the comment
            let Comment
                    { _id = retrievedId
                    , _content = retrievedContent
                    , _project_id = retrievedProjectId
                    } = comment

            let CommentId {_id = expectedIdText} = commentId
            liftIO $ retrievedId `shouldBe` expectedIdText
            liftIO $ retrievedContent `shouldBe` commentContent

            let P.ProjectId {_id = expectedProjectId} = projectId
            liftIO $ retrievedProjectId `shouldBe` Just expectedProjectId

{- | Create a test project and comment, run an action with the comment ID, then clean up
Uses bracket to ensure cleanup happens even if the action fails
Comments are deleted before the project
-}
withTestProjectComment ::
    TodoistConfig ->
    Text -> -- project name
    Text -> -- comment content
    (P.ProjectId -> CommentId -> ExceptT TodoistError IO a) ->
    IO ()
withTestProjectComment config projectName commentContent action = do
    let createResources = do
            liftIO $ putStrLn $ "Creating test project: " <> show projectName
            projectId <- liftTodoist config (P.addProject $ runBuilder (P.newProject projectName) mempty)

            liftIO $ putStrLn $ "Creating test comment: " <> show commentContent
            let P.ProjectId {_id = projIdText} = projectId
            let commentCreate = buildTestCommentForProject commentContent projIdText
            createdComment <- liftTodoist config (addComment commentCreate)
            let Comment {_id = commentIdText} = createdComment
            let commentId = CommentId {_id = commentIdText}

            pure (projectId, commentId)

    let deleteResources (projectId, commentId) = do
            liftIO $ putStrLn $ "Cleaning up test comment: " <> show commentContent
            void $ todoist config (deleteComment commentId)

            liftIO $ putStrLn $ "Cleaning up test project: " <> show projectName
            void $ todoist config (P.deleteProject projectId)

    let runAction (projectId, commentId) = void $ assertSucceeds $ action projectId commentId

    bracket (assertSucceeds createResources) deleteResources runAction

{- | Create a test project, task, and comment on the task, run an action, then clean up
Uses bracket to ensure cleanup happens even if the action fails
Comments are deleted before tasks, tasks before projects
-}
withTestTaskComment ::
    TodoistConfig ->
    Text -> -- project name
    Text -> -- task content
    Text -> -- comment content
    (TaskId -> CommentId -> ExceptT TodoistError IO a) ->
    IO ()
withTestTaskComment config projectName taskContent commentContent action = do
    let createResources = do
            liftIO $ putStrLn $ "Creating test project: " <> show projectName
            projectId <- liftTodoist config (P.addProject $ runBuilder (P.newProject projectName) mempty)

            liftIO $ putStrLn $ "Creating test task: " <> show taskContent
            let P.ProjectId {_id = projIdText} = projectId
            let taskCreate = buildTestTask taskContent projIdText
            newTaskResult <- liftTodoist config (addTask taskCreate)
            let NewTask {_id = newTaskIdText} = newTaskResult
            let taskId = TaskId {_id = newTaskIdText}

            liftIO $ putStrLn $ "Creating test comment: " <> show commentContent
            let commentCreate = buildTestCommentForTask commentContent newTaskIdText
            createdComment <- liftTodoist config (addComment commentCreate)
            let Comment {_id = commentIdText} = createdComment
            let commentId = CommentId {_id = commentIdText}

            pure (projectId, taskId, commentId)

    let deleteResources (projectId, taskId, commentId) = do
            liftIO $ putStrLn $ "Cleaning up test comment: " <> show commentContent
            void $ todoist config (deleteComment commentId)

            liftIO $ putStrLn $ "Cleaning up test task: " <> show taskContent
            void $ todoist config (deleteTask taskId)

            liftIO $ putStrLn $ "Cleaning up test project: " <> show projectName
            void $ todoist config (P.deleteProject projectId)

    let runAction (_, taskId, commentId) = void $ assertSucceeds $ action taskId commentId

    bracket (assertSucceeds createResources) deleteResources runAction

{- | Create a test project and multiple comments, run an action with their IDs, then clean up
Ensures all comments are deleted before the project is deleted
-}
withTestProjectComments ::
    TodoistConfig ->
    Text -> -- project name
    [Text] -> -- comment contents
    (P.ProjectId -> [CommentId] -> ExceptT TodoistError IO a) ->
    IO ()
withTestProjectComments config projectName commentContents action = do
    let createResources = do
            liftIO $ putStrLn $ "Creating test project: " <> show projectName
            projectId <- liftTodoist config (P.addProject $ runBuilder (P.newProject projectName) mempty)

            liftIO $ putStrLn $ "Creating " <> show (L.length commentContents) <> " test comments"
            let P.ProjectId {_id = projIdText} = projectId
            commentIds <-
                mapM
                    ( \content -> do
                        let commentCreate = buildTestCommentForProject content projIdText
                        createdComment <- liftTodoist config (addComment commentCreate)
                        let Comment {_id = commentIdText} = createdComment
                        pure $ CommentId {_id = commentIdText}
                    )
                    commentContents

            pure (projectId, commentIds)

    let deleteResources (projectId, commentIds) = do
            liftIO $ putStrLn $ "Cleaning up " <> show (L.length commentIds) <> " test comments"
            forM_ commentIds $ \commentId ->
                void $ todoist config (deleteComment commentId)

            liftIO $ putStrLn $ "Cleaning up test project: " <> show projectName
            void $ todoist config (P.deleteProject projectId)

    let runAction (projectId, commentIds) = void $ assertSucceeds $ action projectId commentIds

    bracket (assertSucceeds createResources) deleteResources runAction

{- | Build a test comment for a project using the Builder pattern
Creates a CommentCreate with content and project_id for testing
-}
buildTestCommentForProject :: Text -> Text -> CommentCreate
buildTestCommentForProject commentContent projectId =
    runBuilder
        (newComment commentContent)
        (setProjectId projectId)

{- | Build a test comment for a task using the Builder pattern
Creates a CommentCreate with content and task_id for testing
-}
buildTestCommentForTask :: Text -> Text -> CommentCreate
buildTestCommentForTask commentContent taskId =
    runBuilder
        (newComment commentContent)
        (setTaskId taskId)
