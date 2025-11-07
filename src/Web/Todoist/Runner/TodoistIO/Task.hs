{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | TodoistTaskM instance for the TodoistIO monad
module Web.Todoist.Runner.TodoistIO.Task () where

import Web.Todoist.Domain.Task
    ( AddTaskQuick
    , CompletedTasksQueryParam
    , CompletedTasksQueryParamAPI (items)
    , Deadline (..)
    , Due (..)
    , Duration (..)
    , DurationUnit (..)
    , MoveTask
    , NewTask (..)
    , Task (..)
    , TaskCompletedItem (..)
    , TaskCreate
    , TaskFilter (..)
    , TaskParam (..)
    , TaskPatch
    , TodoistTaskM (..)
    )
import Web.Todoist.Domain.Types (TaskId (..))
import Web.Todoist.Internal.Config (TodoistConfig)
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Internal.HTTP (PostResponse (..), apiDelete, apiGet, apiPost)
import Web.Todoist.Internal.Request (mkTodoistRequest)
import Web.Todoist.Internal.Types
    ( DeadlineResponse (..)
    , DueResponse (..)
    , DurationResponse (..)
    , NewTaskResponse (..)
    , TaskResponse (..)
    , TodoistReturn (next_cursor, results)
    )
import Web.Todoist.QueryParam (QueryParam (toQueryParam))

import Control.Applicative (pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import System.IO (IO)

-- Import TodoistIO type from Core module to avoid circular dependencies
import Web.Todoist.Runner.TodoistIO.Core (TodoistIO (..))

-- | Parse duration unit text to DurationUnit enum
parseDurationUnit :: Text -> DurationUnit
parseDurationUnit txt = case T.toLower txt of
    "minute" -> Minute
    "day" -> Day
    _ -> Day -- Default to Day for unknown units

-- | Convert DurationResponse to Duration
durationResponseToDuration :: DurationResponse -> Duration
durationResponseToDuration DurationResponse {..} =
    Duration
        { _amount = p_amount
        , _unit = parseDurationUnit p_unit
        }

-- | Convert DeadlineResponse to Deadline
deadlineResponseToDeadline :: DeadlineResponse -> Deadline
deadlineResponseToDeadline DeadlineResponse {..} =
    Deadline
        { _date = p_date
        , _lang = p_lang
        }

-- | Convert DueResponse to Due
dueResponseToDue :: DueResponse -> Due
dueResponseToDue DueResponse {..} =
    Due
        { _date = p_date
        , _string = p_string
        , _lang = p_lang
        , _is_recurring = p_is_recurring
        , _timezone = p_timezone
        }

-- | Convert TaskResponse to Task
taskResponseToTask :: TaskResponse -> Task
taskResponseToTask TaskResponse {..} =
    Task
        { _id = p_id
        , _content = p_content
        , _description = p_description
        , _project_id = p_project_id
        , _section_id = p_section_id
        , _parent_id = p_parent_id
        , _labels = p_labels
        , _priority = p_priority
        , _due = fmap dueResponseToDue p_due
        , _deadline = fmap deadlineResponseToDeadline p_deadline
        , _duration = fmap durationResponseToDuration p_duration
        , _is_collapsed = p_is_collapsed
        , _order = p_child_order
        , _assignee_id = p_responsible_uid
        , _assigner_id = p_assigned_by_uid
        , _completed_at = p_completed_at
        , _creator_id = p_user_id
        , _created_at = fromMaybe "" p_added_at
        , _updated_at = fromMaybe "" p_updated_at
        }

-- | Convert NewTaskResponse to NewTask
newTaskResponseToNewTask :: NewTaskResponse -> NewTask
newTaskResponseToNewTask NewTaskResponse {..} =
    NewTask
        { _user_id = p_user_id
        , _id = p_id
        , _project_id = p_project_id
        , _section_id = p_section_id
        , _parent_id = p_parent_id
        , _added_by_uid = p_added_by_uid
        , _assigned_by_uid = p_assigned_by_uid
        , _responsible_uid = p_responsible_uid
        , _labels = p_labels
        , _checked = p_checked
        , _is_deleted = p_is_deleted
        , _added_at = p_added_at
        , _completed_at = p_completed_at
        , _updated_at = p_updated_at
        , _priority = p_priority
        , _child_order = p_child_order
        , _content = p_content
        , _description = p_description
        , _note_count = p_note_count
        , _day_order = p_day_order
        , _is_collapsed = p_is_collapsed
        }

instance TodoistTaskM TodoistIO where
    getTasks :: TaskParam -> TodoistIO [Task]
    getTasks initialParams = TodoistIO $ do
        config <- ask
        let loop :: Maybe Text -> [Task] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Task]
            loop cursorVal acc = do
                let TaskParam {project_id, section_id, parent_id, task_ids} = initialParams
                    params = TaskParam {project_id, section_id, parent_id, task_ids, cursor = cursorVal, limit = Nothing}
                    apiRequest = mkTodoistRequest @Void ["tasks"] (Just $ toQueryParam params) Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn TaskResponse)) config apiRequest
                case resp of
                    Right res -> do
                        let newAcc = acc <> fmap taskResponseToTask (results res)
                        case next_cursor res of
                            Nothing -> pure newAcc
                            Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    getTask :: TaskId -> TodoistIO Task
    getTask TaskId {getTaskId = taskIdText} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", taskIdText] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @TaskResponse) config apiRequest
        case resp of
            Right res -> pure (taskResponseToTask res)
            Left err -> lift $ except (Left err)

    addTask :: TaskCreate -> TodoistIO NewTask
    addTask taskCreate = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @TaskCreate ["tasks"] Nothing Nothing
        resp <- liftIO $ apiPost (Just taskCreate) (JsonResponse (Proxy @NewTaskResponse)) config apiRequest
        case resp of
            Right res -> pure (newTaskResponseToNewTask res)
            Left err -> lift $ except (Left err)

    updateTask :: TaskId -> TaskPatch -> TodoistIO NewTask
    updateTask TaskId {getTaskId = taskIdText} taskPatch = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @TaskPatch ["tasks", taskIdText] Nothing Nothing
        resp <- liftIO $ apiPost (Just taskPatch) (JsonResponse (Proxy @NewTaskResponse)) config apiRequest
        case resp of
            Right res -> pure (newTaskResponseToNewTask res)
            Left err -> lift $ except (Left err)

    closeTask :: TaskId -> TodoistIO ()
    closeTask TaskId {getTaskId = taskIdText} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", taskIdText, "close"] Nothing Nothing
        resp <- liftIO $ apiPost (Nothing @Void) IgnoreResponse config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    uncloseTask :: TaskId -> TodoistIO ()
    uncloseTask TaskId {getTaskId = taskIdText} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", taskIdText, "reopen"] Nothing Nothing
        resp <- liftIO $ apiPost (Nothing @Void) IgnoreResponse config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    deleteTask :: TaskId -> TodoistIO ()
    deleteTask TaskId {getTaskId = taskIdText} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", taskIdText] Nothing Nothing
        resp <- liftIO $ apiDelete config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    getTasksByFilter :: TaskFilter -> TodoistIO [TaskId]
    getTasksByFilter initialFilter = TodoistIO $ do
        config <- ask
        let loop :: Maybe Text -> [TaskId] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [TaskId]
            loop cursorVal acc = do
                let TaskFilter {query, lang, limit} = initialFilter
                    filter' = TaskFilter {query, lang, cursor = cursorVal, limit}
                    apiRequest = mkTodoistRequest @Void ["tasks", "filter"] (Just $ toQueryParam filter') Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn TaskResponse)) config apiRequest
                case resp of
                    Right res -> do
                        let taskIds = fmap (\TaskResponse {p_id} -> TaskId {getTaskId = p_id}) (results res)
                        let newAcc = acc <> taskIds
                        case next_cursor res of
                            Nothing -> pure newAcc
                            Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    moveTask :: TaskId -> MoveTask -> TodoistIO TaskId
    moveTask TaskId {getTaskId = taskIdText} moveTaskBody = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @MoveTask ["tasks", taskIdText, "move"] Nothing Nothing
        resp <- liftIO $ apiPost (Just moveTaskBody) (JsonResponse (Proxy @TaskResponse)) config apiRequest
        case resp of
            Right TaskResponse {p_id} -> pure $ TaskId {getTaskId = p_id}
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
            Right res -> pure $ fmap (\(TaskCompletedItem tid) -> TaskId {getTaskId = tid}) (items res)
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
            Right res -> pure $ fmap (\(TaskCompletedItem tid) -> TaskId {getTaskId = tid}) (items res)
            Left err -> lift $ except (Left err)

    getTasksPaginated :: TaskParam -> TodoistIO ([TaskId], Maybe Text)
    getTasksPaginated taskparams = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks"] (Just $ toQueryParam taskparams) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn TaskResponse)) config apiRequest
        case resp of
            Right res -> do
                let taskIds = fmap (\TaskResponse {p_id} -> TaskId {getTaskId = p_id}) (results res)
                pure (taskIds, fmap T.pack (next_cursor res))
            Left err -> lift $ except (Left err)

    getTasksByFilterPaginated :: TaskFilter -> TodoistIO ([TaskId], Maybe Text)
    getTasksByFilterPaginated taskFilter = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", "filter"] (Just $ toQueryParam taskFilter) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn TaskResponse)) config apiRequest
        case resp of
            Right res -> do
                let taskIds = fmap (\TaskResponse {p_id} -> TaskId {getTaskId = p_id}) (results res)
                pure (taskIds, fmap T.pack (next_cursor res))
            Left err -> lift $ except (Left err)

    getTasksWithLimit :: TaskParam -> Int -> TodoistIO [TaskId]
    getTasksWithLimit baseParams pageLimit = TodoistIO $ do
        let loop :: Maybe Text -> [TaskId] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [TaskId]
            loop cursorVal acc = do
                let TaskParam {project_id, section_id, parent_id, task_ids} = baseParams
                    params =
                        TaskParam {project_id, section_id, parent_id, task_ids, cursor = cursorVal, limit = Just pageLimit}
                (tasks, nextCursor) <- unTodoist $ getTasksPaginated params
                let newAcc = acc <> tasks
                case nextCursor of
                    Nothing -> pure newAcc
                    Just c -> loop (Just c) newAcc
        loop Nothing []

    getTasksByFilterWithLimit :: TaskFilter -> Int -> TodoistIO [TaskId]
    getTasksByFilterWithLimit baseFilter pageLimit = TodoistIO $ do
        let loop :: Maybe Text -> [TaskId] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [TaskId]
            loop cursorVal acc = do
                let TaskFilter {query, lang} = baseFilter
                    filter' = TaskFilter {query, lang, cursor = cursorVal, limit = Just pageLimit}
                (tasks, nextCursor) <- unTodoist $ getTasksByFilterPaginated filter'
                let newAcc = acc <> tasks
                case nextCursor of
                    Nothing -> pure newAcc
                    Just c -> loop (Just c) newAcc
        loop Nothing []
