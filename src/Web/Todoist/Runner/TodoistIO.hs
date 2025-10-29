{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- everything related to the interface between the function and low http request code
module Web.Todoist.Runner.TodoistIO
    ( TodoistConfig (..)
    , TodoistIO (..)
    , projectResponseToProject
    ) where

import Web.Todoist.Domain.Project
    ( Collaborator
    , Project (..)
    , ProjectId (..)
    , TodoistProjectM (..)
    , parseViewStyle
    )
import Web.Todoist.Domain.Task
    ( AddTaskQuick
    , CompletedTasksQueryParam
    , CompletedTasksQueryParamAPI (items)
    , MoveTask
    , NewTask
    , Task
    , TaskFilter
    , TaskId (..)
    , TaskParam
    , TodoistTaskM (..)
    )
import Web.Todoist.Internal.Config (TodoistConfig (..))
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Internal.HTTP (PostResponse (..), apiDelete, apiGet, apiPost)
import Web.Todoist.Internal.Request (mkTodoistRequest)
import Web.Todoist.Internal.Types
    ( CreatedAt (..)
    , ProjectResponse (..)
    , TodoistReturn (results)
    , UpdatedAt (..)
    )
import Web.Todoist.Patch (ProjectCreate, TaskCreate, TaskPatch)
import Web.Todoist.QueryParam (QueryParam (toQueryParam))

import Control.Applicative (Applicative, pure, (<$>))
import Control.Monad (Functor, Monad)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except)
import Control.Monad.Trans.Reader (ReaderT (ReaderT), ask)
import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Maybe (Maybe (..))
import Data.Proxy (Proxy (Proxy))
import Data.Void (Void)
import System.IO (IO)

newtype TodoistIO a
    = TodoistIO {unTodoist :: ReaderT TodoistConfig (ExceptT TodoistError IO) a}
    deriving newtype (Functor, Applicative, Monad)

instance TodoistProjectM TodoistIO where
    getProject :: ProjectId -> TodoistIO Project
    getProject ProjectId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", _id] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @ProjectResponse) config apiRequest
        case resp of
            Right res -> pure $ projectResponseToProject res
            Left err -> lift $ except (Left err)

    getAllProjects :: TodoistIO [Project]
    getAllProjects = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects"] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn ProjectResponse)) config apiRequest

        case resp of
            Right res -> pure $ projectResponseToProject <$> results res
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
        let apiRequest = mkTodoistRequest @ProjectCreate ["projects"] Nothing Nothing
        resp <- liftIO $ apiPost (Just project) (JsonResponse (Proxy @ProjectId)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    archiveProject :: ProjectId -> TodoistIO ProjectId
    archiveProject ProjectId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", _id, "archive"] Nothing Nothing
        resp <- liftIO $ apiPost (Nothing @Void) (JsonResponse (Proxy @ProjectId)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    unarchiveProject :: ProjectId -> TodoistIO ProjectId
    unarchiveProject ProjectId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", _id, "unarchive"] Nothing Nothing
        resp <- liftIO $ apiPost (Nothing @Void) (JsonResponse (Proxy @ProjectId)) config apiRequest
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

-- | Convert a ProjectResponse (HTTP API type) to a Project (domain type)
projectResponseToProject :: ProjectResponse -> Project
projectResponseToProject ProjectResponse {..} =
    let (CreatedAt createdAt) = p_created_at
        (UpdatedAt updatedAt) = p_updated_at
     in Project
            { _id = p_id
            , _name = p_name
            , _description = p_description
            , _order = p_child_order
            , _color = p_color
            , _is_collapsed = p_is_collapsed
            , _is_shared = p_is_shared
            , _is_favorite = p_is_favorite
            , _is_archived = p_is_archived
            , _can_assign_tasks = p_can_assign_tasks
            , _view_style = parseViewStyle p_view_style
            , _created_at = createdAt
            , _updated_at = updatedAt
            }
