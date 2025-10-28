{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- everything related to the interface between the function and low http request code
module Web.Todoist.Runner.TodoistIO
    ( TodoistConfig (..)
    , TodoistIO (..)
    ) where

import Web.Todoist.Patch ( TaskPatch, TaskCreate, ProjectCreate )
import Web.Todoist.Domain.Project
    ( TodoistProjectM(..), Collaborator, ProjectId(..) )
import Web.Todoist.Internal.Config (TodoistConfig (..))
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Internal.HTTP (apiDelete, apiGet, apiPost, apiPost', apiPost'')
import Web.Todoist.Internal.Request (mkTodoistRequest)
import Web.Todoist.Internal.Types (TodoistReturn (results))
import Web.Todoist.Domain.Task
    ( CompletedTasksQueryParam,
      TaskFilter,
      TaskParam,
      TodoistTaskM(..),
      CompletedTasksQueryParamAPI(items),
      AddTaskQuick,
      MoveTask,
      NewTask,
      Task,
      TaskId(..) )
import Web.Todoist.QueryParam ( QueryParam(toQueryParam) )

import Control.Applicative (Applicative, pure)
import Control.Monad (Functor, Monad)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except ( ExceptT, except )
import Control.Monad.Trans.Reader ( ReaderT(ReaderT), ask )
import Data.Either ( Either(Left, Right) )
import Data.Function (($))
import Data.Maybe (Maybe (..))
import Data.Proxy ( Proxy(Proxy) )
import Data.Void (Void)
import System.IO (IO)

newtype TodoistIO a
    = TodoistIO {unTodoist :: ReaderT TodoistConfig (ExceptT TodoistError IO) a}
    deriving newtype (Functor, Applicative, Monad)

instance TodoistProjectM TodoistIO where
    getProject :: ProjectId -> TodoistIO ProjectId
    getProject ProjectId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", _id] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @ProjectId) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    getAllProjects :: TodoistIO [ProjectId]
    getAllProjects = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects"] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn ProjectId)) config apiRequest

        case resp of
            Right res -> pure $ results res
            Left err -> lift $ except (Left err)

    getProjectCollaborators :: ProjectId -> TodoistIO [Collaborator]
    getProjectCollaborators ProjectId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", _id, "collaborators"] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn Collaborator)) config apiRequest
        case resp of
            Right res -> pure $ results res
            Left err -> lift $ except (Left err)

    deleteProject :: ProjectId -> TodoistIO ()
    deleteProject ProjectId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", _id] Nothing Nothing
        resp <- liftIO $ apiDelete config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    addProject :: ProjectCreate -> TodoistIO ProjectId
    addProject project = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @ProjectCreate ["projects"] Nothing (Just project)
        resp <- liftIO $ apiPost (Proxy @ProjectId) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    archiveProject :: ProjectId -> TodoistIO ProjectId
    archiveProject ProjectId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", _id, "archive"] Nothing Nothing
        resp <- liftIO $ apiPost (Proxy @ProjectId) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    unarchiveProject :: ProjectId -> TodoistIO ProjectId
    unarchiveProject ProjectId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", _id, "unarchive"] Nothing Nothing
        resp <- liftIO $ apiPost (Proxy @ProjectId) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

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
        let apiRequest = mkTodoistRequest @TaskCreate ["tasks"] Nothing (Just taskCreate)
        resp <- liftIO $ apiPost (Proxy @NewTask) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    updateTask :: TaskId -> TaskPatch -> TodoistIO NewTask
    updateTask TaskId {..} taskPatch = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @TaskPatch ["tasks", _id] Nothing (Just taskPatch)
        resp <- liftIO $ apiPost (Proxy @NewTask) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    closeTask :: TaskId -> TodoistIO ()
    closeTask TaskId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", _id, "close"] Nothing Nothing
        resp <- liftIO $ apiPost' config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    uncloseTask :: TaskId -> TodoistIO ()
    uncloseTask TaskId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", _id, "reopen"] Nothing Nothing
        resp <- liftIO $ apiPost' config apiRequest
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
        let apiRequest = mkTodoistRequest @MoveTask ["tasks", _id, "move"] Nothing (Just moveTaskBody)
        resp <- liftIO $ apiPost (Proxy @TaskId) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    addTaskQuick :: AddTaskQuick -> TodoistIO ()
    addTaskQuick atqBody = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @AddTaskQuick ["tasks", "quick"] Nothing (Just atqBody)
        resp <- liftIO $ apiPost'' config apiRequest
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
