{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | TodoistTaskM instance for the TodoistIO monad
module Web.Todoist.Runner.TodoistIO.Task () where

import Web.Todoist.Domain.Task
import Web.Todoist.Internal.HTTP (PostResponse (..), apiDelete, apiGet, apiPost)
import Web.Todoist.Internal.Request (mkTodoistRequest)
import Web.Todoist.Internal.Types (TodoistReturn (results))
import Web.Todoist.QueryParam (QueryParam (toQueryParam))

import Control.Applicative (pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Reader (ask)
import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Maybe (Maybe (..))
import Data.Proxy (Proxy (Proxy))
import Data.Void (Void)

-- Import TodoistIO type from Core module to avoid circular dependencies
import Web.Todoist.Runner.TodoistIO.Core (TodoistIO (..))

instance TodoistTaskM TodoistIO where
    getTasks :: TaskParam -> TodoistIO [TaskId]
    getTasks taskparams = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks"] (Just $ toQueryParam taskparams) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn TaskId)) config apiRequest
        case resp of
            Right res -> pure $ results res
            Left err -> lift $ except (Left err)

    getTask :: TaskId -> TodoistIO Task
    getTask TaskId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", _id] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @Task) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    addTask :: TaskCreate -> TodoistIO NewTask
    addTask taskCreate = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @TaskCreate ["tasks"] Nothing Nothing
        resp <- liftIO $ apiPost (Just taskCreate) (JsonResponse (Proxy @NewTask)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    updateTask :: TaskId -> TaskPatch -> TodoistIO NewTask
    updateTask TaskId {..} taskPatch = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @TaskPatch ["tasks", _id] Nothing Nothing
        resp <- liftIO $ apiPost (Just taskPatch) (JsonResponse (Proxy @NewTask)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    closeTask :: TaskId -> TodoistIO ()
    closeTask TaskId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", _id, "close"] Nothing Nothing
        resp <- liftIO $ apiPost (Nothing @Void) IgnoreResponse config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    uncloseTask :: TaskId -> TodoistIO ()
    uncloseTask TaskId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", _id, "reopen"] Nothing Nothing
        resp <- liftIO $ apiPost (Nothing @Void) IgnoreResponse config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    deleteTask :: TaskId -> TodoistIO ()
    deleteTask TaskId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", _id] Nothing Nothing
        resp <- liftIO $ apiDelete config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    getTasksByFilter :: TaskFilter -> TodoistIO [TaskId]
    getTasksByFilter taskFilter = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", "filter"] (Just $ toQueryParam taskFilter) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn TaskId)) config apiRequest
        case resp of
            Right res -> pure $ results res
            Left err -> lift $ except (Left err)

    moveTask :: TaskId -> MoveTask -> TodoistIO TaskId
    moveTask TaskId {..} moveTaskBody = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @MoveTask ["tasks", _id, "move"] Nothing Nothing
        resp <- liftIO $ apiPost (Just moveTaskBody) (JsonResponse (Proxy @TaskId)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    addTaskQuick :: AddTaskQuick -> TodoistIO ()
    addTaskQuick atqBody = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @AddTaskQuick ["tasks", "quick"] Nothing Nothing
        resp <- liftIO $ apiPost (Just atqBody) IgnoreResponse config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    getCompletedTasksByDueDate :: CompletedTasksQueryParam -> TodoistIO [TaskId]
    getCompletedTasksByDueDate completedTasksQueryParam = TodoistIO $ do
        config <- ask
        let apiRequest =
                mkTodoistRequest @CompletedTasksQueryParam
                    ["tasks", "completed", "by_due_date"]
                    (Just $ toQueryParam completedTasksQueryParam)
                    Nothing
        resp <- liftIO $ apiGet (Proxy @CompletedTasksQueryParamAPI) config apiRequest
        case resp of
            Right res -> pure $ items res
            Left err -> lift $ except (Left err)

    getCompletedTasksByCompletionDate :: CompletedTasksQueryParam -> TodoistIO [TaskId]
    getCompletedTasksByCompletionDate completedTasksQueryParam = TodoistIO $ do
        config <- ask
        let apiRequest =
                mkTodoistRequest @CompletedTasksQueryParam
                    ["tasks", "completed", "by_completion_date"]
                    (Just $ toQueryParam completedTasksQueryParam)
                    Nothing
        resp <- liftIO $ apiGet (Proxy @CompletedTasksQueryParamAPI) config apiRequest
        case resp of
            Right res -> pure $ items res
            Left err -> lift $ except (Left err)
