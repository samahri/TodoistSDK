{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | TodoistIO interpreter implementations for all domain type classes.
-- This module contains all the orphan instances that implement the domain
-- operations using actual HTTP requests to the Todoist API.
module Web.Todoist.Runner.IO.Interpreters
    ( -- * Conversion Functions (for testing)
      projectResponseToProject
    , commentResponseToComment
    ) where

import Control.Applicative (pure, (<$>))
import Control.Monad (fmap)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (traverse)
import Data.Void (Void)
import System.IO (IO)

-- Domain imports
import Web.Todoist.Domain.Comment
    ( Comment (..)
    , CommentCreate
    , CommentId (..)
    , CommentParam (..)
    , CommentUpdate
    , TodoistCommentM (..)
    )
import Web.Todoist.Domain.Label
    ( Label (..)
    , LabelCreate
    , LabelId (..)
    , LabelParam (..)
    , LabelUpdate
    , SharedLabelParam (..)
    , SharedLabelRemove
    , SharedLabelRename
    , TodoistLabelM (..)
    )
import Web.Todoist.Domain.Project
    ( CanAssignTasks (..)
    , Collaborator
    , IsArchived (..)
    , IsShared (..)
    , PaginationParam (..)
    , Project (..)
    , ProjectCreate
    , ProjectUpdate
    , TodoistProjectM (..)
    )
import Web.Todoist.Domain.Section
    ( Section (..)
    , SectionCreate
    , SectionId (..)
    , SectionParam (..)
    , SectionUpdate
    , TodoistSectionM (..)
    )
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
import Web.Todoist.Domain.Types
    ( Color (..)
    , Content (..)
    , Description (..)
    , IsFavorite (..)
    , IsCollapsed (..)
    , Name (..)
    , Order (..)
    , ParentId (..)
    , ProjectId (..)
    , TaskId (..)
    , Uid (..)
    , parseViewStyle
    )

-- Internal imports
import Web.Todoist.Internal.Config (TodoistConfig)
import Web.Todoist.Internal.Error (TodoistError (HttpError))
import Web.Todoist.Internal.HTTP (PostResponse (..), apiDelete, apiGet, apiPost)
import Web.Todoist.Internal.Request (mkTodoistRequest)
import Web.Todoist.Internal.Types
    ( CommentResponse (..)
    , CreatedAt (..)
    , DeadlineResponse (..)
    , DueResponse (..)
    , DurationResponse (..)
    , LabelResponse (..)
    , NewTaskResponse (..)
    , ProjectPermissions
    , ProjectResponse (..)
    , SectionResponse (..)
    , TaskResponse (..)
    , TodoistReturn (next_cursor, results)
    , UpdatedAt (..)
    )

-- Runner imports
import Web.Todoist.Util.QueryParam (QueryParam (toQueryParam))
import Web.Todoist.Runner.IO.Core (TodoistIO (..))

-- ============================================================================
-- Conversion Functions (Internal - Not Exported)
-- ============================================================================

-- | Convert ProjectResponse to Project
projectResponseToProject :: ProjectResponse -> Project
projectResponseToProject ProjectResponse {..} =
    let (CreatedAt createdAt) = p_created_at
        (UpdatedAt updatedAt) = p_updated_at
     in Project
            { _id = ProjectId p_id
            , _name = Name p_name
            , _description = Description p_description
            , _order = Order p_child_order
            , _color = Color p_color
            , _is_collapsed = IsCollapsed p_is_collapsed
            , _is_shared = IsShared p_is_shared
            , _is_favorite = IsFavorite p_is_favorite
            , _is_archived = IsArchived p_is_archived
            , _can_assign_tasks = CanAssignTasks p_can_assign_tasks
            , _view_style = parseViewStyle p_view_style
            , _created_at = createdAt
            , _updated_at = updatedAt
            }

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
        { _id = TaskId p_id
        , _content = Content p_content
        , _description = Description p_description
        , _project_id = ProjectId p_project_id
        , _section_id = fmap (\sid -> SectionId {_id = sid}) p_section_id
        , _parent_id = fmap ParentId p_parent_id
        , _labels = p_labels
        , _priority = p_priority
        , _due = fmap dueResponseToDue p_due
        , _deadline = fmap deadlineResponseToDeadline p_deadline
        , _duration = fmap durationResponseToDuration p_duration
        , _is_collapsed = IsCollapsed p_is_collapsed
        , _order = Order p_child_order
        , _assignee_id = fmap Uid p_responsible_uid
        , _assigner_id = fmap Uid p_assigned_by_uid
        , _completed_at = p_completed_at
        , _creator_id = Uid p_user_id
        , _created_at = fromMaybe "" p_added_at
        , _updated_at = fromMaybe "" p_updated_at
        }

-- | Convert NewTaskResponse to NewTask
newTaskResponseToNewTask :: NewTaskResponse -> NewTask
newTaskResponseToNewTask NewTaskResponse {..} =
    NewTask
        { _user_id = p_user_id
        , _id = TaskId p_id
        , _project_id = ProjectId p_project_id
        , _section_id = fmap (\sid -> SectionId {_id = sid}) p_section_id
        , _parent_id = fmap ParentId p_parent_id
        , _added_by_uid = fmap Uid p_added_by_uid
        , _assigned_by_uid = fmap Uid p_assigned_by_uid
        , _responsible_uid = fmap Uid p_responsible_uid
        , _labels = p_labels
        , _checked = p_checked
        , _is_deleted = p_is_deleted
        , _added_at = p_added_at
        , _completed_at = p_completed_at
        , _updated_at = p_updated_at
        , _priority = p_priority
        , _child_order = Order p_child_order
        , _content = Content p_content
        , _description = Description p_description
        , _note_count = p_note_count
        , _day_order = Order p_day_order
        , _is_collapsed = IsCollapsed p_is_collapsed
        }

-- | Convert CommentResponse to Comment
-- Validates that at least one of task_id or project_id is present
commentResponseToComment :: CommentResponse -> Either TodoistError Comment
commentResponseToComment CommentResponse {..} =
    case (p_item_id, p_project_id) of
        (Nothing, Nothing) -> Left $ HttpError "Comment must have either task_id or project_id"
        _ ->
            Right $
                Comment
                    { _id = CommentId p_id
                    , _content = Content p_content
                    , _poster_id = fmap Uid p_posted_uid
                    , _posted_at = fmap Uid p_posted_at
                    , _task_id = fmap TaskId p_item_id
                    , _project_id = fmap ProjectId p_project_id
                    , _attachment = p_file_attachment
                    }

-- | Convert SectionResponse to Section
sectionResponseToSection :: SectionResponse -> Section
sectionResponseToSection SectionResponse {..} =
    Section
        { _id = SectionId {_id = p_id}
        , _name = Name p_name
        , _project_id = ProjectId p_project_id
        , _is_collapsed = IsCollapsed p_is_collapsed
        , _order = Order p_section_order
        }

-- | Convert LabelResponse to Label
labelResponseToLabel :: LabelResponse -> Label
labelResponseToLabel LabelResponse {..} =
    Label
        { _id = LabelId p_id
        , _name = Name p_name
        , _color = Color p_color
        , _order = fmap Order p_order
        , _is_favorite = IsFavorite p_is_favorite
        }

-- ============================================================================
-- TodoistProjectM Instance
-- ============================================================================

instance TodoistProjectM TodoistIO where
    getProject :: ProjectId -> TodoistIO Project
    getProject ProjectId {getProjectId = projectIdText} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", projectIdText] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @ProjectResponse) config apiRequest
        case resp of
            Right res -> pure $ projectResponseToProject res
            Left err -> lift $ except (Left err)

    getAllProjects :: TodoistIO [Project]
    getAllProjects = TodoistIO $ do
        config <- ask
        let loop :: Maybe Text -> [Project] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Project]
            loop cursor acc = do
                let params = PaginationParam {cursor = cursor, limit = Nothing}
                    apiRequest = mkTodoistRequest @Void ["projects"] (Just $ toQueryParam params) Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn ProjectResponse)) config apiRequest
                case resp of
                    Right res -> do
                        let newAcc = acc <> (projectResponseToProject <$> results res)
                        case next_cursor res of
                            Nothing -> pure newAcc
                            Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    getProjectCollaborators :: ProjectId -> TodoistIO [Collaborator]
    getProjectCollaborators ProjectId {getProjectId = projectIdText} = TodoistIO $ do
        config <- ask
        let loop ::
                Maybe Text -> [Collaborator] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Collaborator]
            loop cursor acc = do
                let params = PaginationParam {cursor = cursor, limit = Nothing}
                    apiRequest =
                        mkTodoistRequest @Void
                            ["projects", projectIdText, "collaborators"]
                            (Just $ toQueryParam params)
                            Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn Collaborator)) config apiRequest
                case resp of
                    Right res -> do
                        let newAcc = acc <> results res
                        case next_cursor res of
                            Nothing -> pure newAcc
                            Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    deleteProject :: ProjectId -> TodoistIO ()
    deleteProject ProjectId {getProjectId = projectIdText} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", projectIdText] Nothing Nothing
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
    archiveProject ProjectId {getProjectId = projectIdText} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", projectIdText, "archive"] Nothing Nothing
        resp <- liftIO $ apiPost (Nothing @Void) (JsonResponse (Proxy @ProjectId)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    unarchiveProject :: ProjectId -> TodoistIO ProjectId
    unarchiveProject ProjectId {getProjectId = projectIdText} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", projectIdText, "unarchive"] Nothing Nothing
        resp <- liftIO $ apiPost (Nothing @Void) (JsonResponse (Proxy @ProjectId)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    getProjectPermissions :: TodoistIO ProjectPermissions
    getProjectPermissions = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", "permissions"] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @ProjectPermissions) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    updateProject :: ProjectUpdate -> ProjectId -> TodoistIO Project
    updateProject projectUpdate ProjectId {getProjectId = projectIdText} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @ProjectUpdate ["projects", projectIdText] Nothing Nothing
        resp <-
            liftIO $ apiPost (Just projectUpdate) (JsonResponse (Proxy @ProjectResponse)) config apiRequest
        case resp of
            Right res -> pure $ projectResponseToProject res
            Left err -> lift $ except (Left err)

    getAllProjectsPaginated :: PaginationParam -> TodoistIO ([Project], Maybe Text)
    getAllProjectsPaginated params = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects"] (Just $ toQueryParam params) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn ProjectResponse)) config apiRequest
        case resp of
            Right res ->
                pure (projectResponseToProject <$> results res, fmap T.pack (next_cursor res))
            Left err -> lift $ except (Left err)

    getProjectCollaboratorsPaginated ::
        PaginationParam -> ProjectId -> TodoistIO ([Collaborator], Maybe Text)
    getProjectCollaboratorsPaginated params ProjectId {getProjectId = projectIdText} = TodoistIO $ do
        config <- ask
        let apiRequest =
                mkTodoistRequest @Void
                    ["projects", projectIdText, "collaborators"]
                    (Just $ toQueryParam params)
                    Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn Collaborator)) config apiRequest
        case resp of
            Right res -> pure (results res, fmap T.pack (next_cursor res))
            Left err -> lift $ except (Left err)

    getAllProjectsWithLimit :: Int -> TodoistIO [Project]
    getAllProjectsWithLimit pageLimit = TodoistIO $ do
        let loop :: Maybe Text -> [Project] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Project]
            loop cursor acc = do
                let params = PaginationParam {cursor = cursor, limit = Just pageLimit}
                (projects, nextCursor) <- unTodoist $ getAllProjectsPaginated params
                let newAcc = acc <> projects
                case nextCursor of
                    Nothing -> pure newAcc
                    Just c -> loop (Just c) newAcc
        loop Nothing []

    getProjectCollaboratorsWithLimit :: Int -> ProjectId -> TodoistIO [Collaborator]
    getProjectCollaboratorsWithLimit pageLimit projectId = TodoistIO $ do
        let loop ::
                Maybe Text -> [Collaborator] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Collaborator]
            loop cursor acc = do
                let params = PaginationParam {cursor = cursor, limit = Just pageLimit}
                (collaborators, nextCursor) <- unTodoist $ getProjectCollaboratorsPaginated params projectId
                let newAcc = acc <> collaborators
                case nextCursor of
                    Nothing -> pure newAcc
                    Just c -> loop (Just c) newAcc
        loop Nothing []

-- ============================================================================
-- TodoistTaskM Instance
-- ============================================================================

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

    updateTask :: TaskPatch -> TaskId -> TodoistIO NewTask
    updateTask taskPatch TaskId {getTaskId = taskIdText} = TodoistIO $ do
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

    moveTask :: MoveTask -> TaskId -> TodoistIO TaskId
    moveTask moveTaskBody TaskId {getTaskId = taskIdText} = TodoistIO $ do
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

    getTasksWithLimit :: Int -> TaskParam -> TodoistIO [TaskId]
    getTasksWithLimit pageLimit baseParams = TodoistIO $ do
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

-- ============================================================================
-- TodoistCommentM Instance
-- ============================================================================

instance TodoistCommentM TodoistIO where
    addComment :: CommentCreate -> TodoistIO Comment
    addComment comment = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @CommentCreate ["comments"] Nothing Nothing
        resp <- liftIO $ apiPost (Just comment) (JsonResponse (Proxy @CommentResponse)) config apiRequest
        case resp of
            Right res -> do
                let commentResults = commentResponseToComment res
                case commentResults of
                    Left err -> lift $ except (Left err)
                    Right cmnt -> pure cmnt
            Left err -> lift $ except (Left err)

    getComments :: CommentParam -> TodoistIO [Comment]
    getComments initialParams = TodoistIO $ do
        config <- ask
        let loop :: Maybe Text -> [Comment] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Comment]
            loop cursorVal acc = do
                let CommentParam {project_id, task_id, public_key} = initialParams
                    params = CommentParam {project_id, task_id, cursor = cursorVal, limit = Nothing, public_key}
                    apiRequest = mkTodoistRequest @Void ["comments"] (Just $ toQueryParam params) Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn CommentResponse)) config apiRequest
                case resp of
                    Right res -> do
                        let commentResults = traverse commentResponseToComment (results res)
                        case commentResults of
                            Left err -> lift $ except (Left err)
                            Right comments -> do
                                let newAcc = acc <> comments
                                case next_cursor res of
                                    Nothing -> pure newAcc
                                    Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    getCommentsPaginated :: CommentParam -> TodoistIO ([Comment], Maybe Text)
    getCommentsPaginated params = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["comments"] (Just $ toQueryParam params) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn CommentResponse)) config apiRequest
        case resp of
            Right res -> do
                let commentResults = traverse commentResponseToComment (results res)
                    nextCursor = fmap T.pack (next_cursor res)
                case commentResults of
                    Left err -> lift $ except (Left err)
                    Right comments -> pure (comments, nextCursor)
            Left err -> lift $ except (Left err)

    getComment :: CommentId -> TodoistIO Comment
    getComment CommentId {getCommentId = commentIdText} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["comments", commentIdText] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @CommentResponse) config apiRequest
        case resp of
            Right res ->
                case commentResponseToComment res of
                    Left err -> lift $ except (Left err)
                    Right comment -> pure comment
            Left err -> lift $ except (Left err)

    updateComment :: CommentUpdate -> CommentId -> TodoistIO Comment
    updateComment update CommentId {getCommentId = commentIdText} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @CommentUpdate ["comments", commentIdText] Nothing Nothing
        resp <- liftIO $ apiPost (Just update) (JsonResponse (Proxy @CommentResponse)) config apiRequest
        case resp of
            Right res ->
                case commentResponseToComment res of
                    Left err -> lift $ except (Left err)
                    Right comment -> pure comment
            Left err -> lift $ except (Left err)

    deleteComment :: CommentId -> TodoistIO ()
    deleteComment CommentId {getCommentId = commentIdText} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["comments", commentIdText] Nothing Nothing
        resp <- liftIO $ apiDelete config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

-- ============================================================================
-- TodoistSectionM Instance
-- ============================================================================

instance TodoistSectionM TodoistIO where
    getSections :: SectionParam -> TodoistIO [Section]
    getSections initialParams = TodoistIO $ do
        config <- ask
        let loop :: Maybe Text -> [Section] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Section]
            loop cursorVal acc = do
                let SectionParam {project_id} = initialParams
                    params = SectionParam {project_id, cursor = cursorVal, limit = Nothing}
                    apiRequest = mkTodoistRequest @Void ["sections"] (Just $ toQueryParam params) Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn SectionResponse)) config apiRequest
                case resp of
                    Right res -> do
                        let newAcc = acc <> fmap sectionResponseToSection (results res)
                        case next_cursor res of
                            Nothing -> pure newAcc
                            Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    getSection :: SectionId -> TodoistIO Section
    getSection SectionId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["sections", _id] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @SectionResponse) config apiRequest
        case resp of
            Right res -> pure $ sectionResponseToSection res
            Left err -> lift $ except (Left err)

    addSection :: SectionCreate -> TodoistIO SectionId
    addSection section = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @SectionCreate ["sections"] Nothing Nothing
        resp <- liftIO $ apiPost (Just section) (JsonResponse (Proxy @SectionId)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    updateSection :: SectionUpdate -> SectionId -> TodoistIO Section
    updateSection sectionUpdate SectionId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @SectionUpdate ["sections", _id] Nothing Nothing
        resp <-
            liftIO $ apiPost (Just sectionUpdate) (JsonResponse (Proxy @SectionResponse)) config apiRequest
        case resp of
            Right res -> pure $ sectionResponseToSection res
            Left err -> lift $ except (Left err)

    deleteSection :: SectionId -> TodoistIO ()
    deleteSection SectionId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["sections", _id] Nothing Nothing
        resp <- liftIO $ apiDelete config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    getSectionsPaginated :: SectionParam -> TodoistIO ([Section], Maybe Text)
    getSectionsPaginated sectionParams = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["sections"] (Just $ toQueryParam sectionParams) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn SectionResponse)) config apiRequest
        case resp of
            Right res -> pure (fmap sectionResponseToSection (results res), fmap T.pack (next_cursor res))
            Left err -> lift $ except (Left err)

-- ============================================================================
-- TodoistLabelM Instance
-- ============================================================================

instance TodoistLabelM TodoistIO where
    getLabels :: LabelParam -> TodoistIO [Label]
    getLabels _ = TodoistIO $ do
        config <- ask
        let loop :: Maybe Text -> [Label] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Label]
            loop cursorVal acc = do
                let params = LabelParam {cursor = cursorVal, limit = Nothing}
                    apiRequest = mkTodoistRequest @Void ["labels"] (Just $ toQueryParam params) Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn LabelResponse)) config apiRequest
                case resp of
                    Right res -> do
                        let newAcc = acc <> fmap labelResponseToLabel (results res)
                        case next_cursor res of
                            Nothing -> pure newAcc
                            Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    getLabel :: LabelId -> TodoistIO Label
    getLabel LabelId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["labels", getLabelId] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @LabelResponse) config apiRequest
        case resp of
            Right res -> pure $ labelResponseToLabel res
            Left err -> lift $ except (Left err)

    addLabel :: LabelCreate -> TodoistIO LabelId
    addLabel label = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @LabelCreate ["labels"] Nothing Nothing
        resp <- liftIO $ apiPost (Just label) (JsonResponse (Proxy @LabelId)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    updateLabel :: LabelUpdate -> LabelId -> TodoistIO Label
    updateLabel labelUpdate LabelId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @LabelUpdate ["labels", getLabelId] Nothing Nothing
        resp <-
            liftIO $ apiPost (Just labelUpdate) (JsonResponse (Proxy @LabelResponse)) config apiRequest
        case resp of
            Right res -> pure $ labelResponseToLabel res
            Left err -> lift $ except (Left err)

    deleteLabel :: LabelId -> TodoistIO ()
    deleteLabel LabelId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["labels", getLabelId] Nothing Nothing
        resp <- liftIO $ apiDelete config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    getLabelsPaginated :: LabelParam -> TodoistIO ([Label], Maybe Text)
    getLabelsPaginated labelParams = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["labels"] (Just $ toQueryParam labelParams) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn LabelResponse)) config apiRequest
        case resp of
            Right res -> pure (fmap labelResponseToLabel (results res), fmap T.pack (next_cursor res))
            Left err -> lift $ except (Left err)

    getSharedLabels :: SharedLabelParam -> TodoistIO [Text]
    getSharedLabels (SharedLabelParam initialOmit _ _) = TodoistIO $ do
        config <- ask
        let loop :: Maybe Text -> [Text] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Text]
            loop cursorVal acc = do
                let params = SharedLabelParam {omit_personal = initialOmit, cursor = cursorVal, limit = Nothing}
                    apiRequest =
                        mkTodoistRequest @Void
                            ["labels", "shared"]
                            (Just $ toQueryParam (params :: SharedLabelParam))
                            Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn Text)) config apiRequest
                case resp of
                    Right res -> do
                        let newAcc = acc <> results res
                        case next_cursor res of
                            Nothing -> pure newAcc
                            Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    getSharedLabelsPaginated :: SharedLabelParam -> TodoistIO ([Text], Maybe Text)
    getSharedLabelsPaginated sharedParams = TodoistIO $ do
        config <- ask
        let apiRequest =
                mkTodoistRequest @Void
                    ["labels", "shared"]
                    (Just $ toQueryParam (sharedParams :: SharedLabelParam))
                    Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn Text)) config apiRequest
        case resp of
            Right res -> pure (results res, fmap T.pack (next_cursor res))
            Left err -> lift $ except (Left err)

    removeSharedLabels :: SharedLabelRemove -> TodoistIO ()
    removeSharedLabels removeReq = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @SharedLabelRemove ["labels", "shared", "remove"] Nothing Nothing
        resp <- liftIO $ apiPost (Just removeReq) IgnoreResponse config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    renameSharedLabels :: SharedLabelRename -> TodoistIO ()
    renameSharedLabels renameReq = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @SharedLabelRename ["labels", "shared", "rename"] Nothing Nothing
        resp <- liftIO $ apiPost (Just renameReq) IgnoreResponse config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)
