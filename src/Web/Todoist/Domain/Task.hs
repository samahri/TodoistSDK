{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Web.Todoist.Domain.Task
    ( TodoistTaskM (..)
    , TaskParam (..)
    , Task (..)
    , Due (..)
    , Deadline (..)
    , Duration (..)
    , DurationUnit (..)
    , NewTask (..) -- todo: remove child exports
    , MoveTask (..)
    , AddTaskQuick
    , emptyMoveTask
    , addTaskQuickText
    , CompletedTasksQueryParamAPI (..)
    , TaskCompletedItem (..)
    , TaskFilter (..)
    , CompletedTasksQueryParam (..)
    , addTaskQuickWithQuery
    , TaskCreate
    , TaskPatch
    , newTask
    , newMoveTask
    , emptyTaskPatch
    , setProjectId
    , taskFilterWithQuery
    ) where

import Web.Todoist.Util.Builder
    ( HasAssigneeId (..)
    , HasContent (..)
    , HasDeadlineDate (..)
    , HasDescription (..)
    , HasDueDate (..)
    , HasDueDatetime (..)
    , HasDueLang (..)
    , HasDueString (..)
    , HasDuration (..)
    , HasDurationUnit (..)
    , HasLabels (..)
    , HasOrder (..)
    , HasParentId (..)
    , HasPriority (..)
    , HasProjectId (..)
    , HasSectionId (..)
    , Initial
    , seed
    )
import Web.Todoist.Internal.Types (Params)
import Web.Todoist.Util.QueryParam (QueryParam (..))

import Control.Monad (Monad)
import Data.Aeson
    ( FromJSON (parseJSON)
    , Options (..)
    , ToJSON (toJSON)
    , Value
    , defaultOptions
    , genericParseJSON
    , genericToJSON
    , omitNothingFields
    )
import Data.Aeson.Types (Parser)
import Data.Bool (Bool (..))
import Data.Eq (Eq)
import Data.Int (Int)
import qualified Data.List as L
import Data.Maybe (Maybe (..), maybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text
import GHC.Generics (Generic)
import Text.Show (Show)
import Web.Todoist.Domain.Section (SectionId (..))
import Web.Todoist.Domain.Types
    ( Content (..)
    , Description (..)
    , IsCollapsed
    , Order (..)
    , ParentId (..)
    , ProjectId (..)
    , TaskId
    , Uid
    )

-- | Duration unit for tasks
data DurationUnit = Minute | Day
    deriving (Show, Eq, Generic)

instance FromJSON DurationUnit where
    parseJSON :: Value -> Parser DurationUnit
    parseJSON = genericParseJSON defaultOptions

instance ToJSON DurationUnit where
    toJSON :: DurationUnit -> Value
    toJSON Minute = "minute"
    toJSON Day = "day"

-- | Duration information for a task
data Duration = Duration
    { _amount :: Int
    , _unit :: DurationUnit
    }
    deriving (Show, Generic)

instance FromJSON Duration where
    parseJSON :: Value -> Parser Duration
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance ToJSON Duration where
    toJSON :: Duration -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1}

-- | Deadline information for a task
data Deadline = Deadline
    { _date :: Text
    , _lang :: Text
    }
    deriving (Show, Generic)

instance FromJSON Deadline where
    parseJSON :: Value -> Parser Deadline
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance ToJSON Deadline where
    toJSON :: Deadline -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1}

-- | Due date information for a task
data Due = Due
    { _date :: Text
    , _string :: Text
    , _lang :: Text
    , _is_recurring :: Bool
    , _timezone :: Maybe Text
    }
    deriving (Show, Generic)

instance FromJSON Due where
    parseJSON :: Value -> Parser Due
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance ToJSON Due where
    toJSON :: Due -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1}

data Task = Task
    { _id :: TaskId
    , _content :: Content
    , _description :: Description
    , _project_id :: ProjectId
    , _section_id :: Maybe SectionId
    , _parent_id :: Maybe ParentId
    , _labels :: [Text]
    , _priority :: Int
    , _due :: Maybe Due
    , _deadline :: Maybe Deadline
    , _duration :: Maybe Duration
    , _is_collapsed :: IsCollapsed
    , _order :: Order
    , _assignee_id :: Maybe Uid
    , _assigner_id :: Maybe Uid
    , _completed_at :: Maybe Text
    , _creator_id :: Uid
    , _created_at :: Text
    , _updated_at :: Text
    }
    deriving (Show, Generic)

instance FromJSON Task where
    parseJSON :: Value -> Parser Task
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance ToJSON Task where
    toJSON :: Task -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1}

data NewTask = NewTask
    { _user_id :: Text
    , _id :: TaskId
    , _project_id :: ProjectId
    , _section_id :: Maybe SectionId
    , _parent_id :: Maybe ParentId
    , _added_by_uid :: Maybe Uid
    , _assigned_by_uid :: Maybe Uid
    , _responsible_uid :: Maybe Uid
    , _labels :: [Text]
    , _checked :: Bool
    , _is_deleted :: Bool
    , _added_at :: Maybe Text
    , _completed_at :: Maybe Text
    , _updated_at :: Maybe Text
    , _priority :: Int
    , _child_order :: Order
    , _content :: Content
    , _description :: Description
    , _note_count :: Int
    , _day_order :: Order
    , _is_collapsed :: IsCollapsed
    }
    deriving (Show, Generic)

instance FromJSON NewTask where
    parseJSON :: Value -> Parser NewTask
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance ToJSON NewTask where
    toJSON :: NewTask -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1}

data MoveTask = MoveTask
    { _project_id :: Maybe ProjectId
    , _section_id :: Maybe SectionId
    , _parent_id :: Maybe ParentId
    }
    deriving (Show, Generic)

instance ToJSON MoveTask where
    toJSON :: MoveTask -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance FromJSON MoveTask where
    parseJSON :: Value -> Parser MoveTask
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = L.drop 1}

newMoveTask :: Initial MoveTask
newMoveTask =
    seed
        MoveTask
            { _project_id = Nothing
            , _section_id = Nothing
            , _parent_id = Nothing
            }

instance HasProjectId MoveTask where
    hasProjectId :: Text -> MoveTask -> MoveTask
    hasProjectId projId MoveTask {..} = MoveTask {_project_id = Just (ProjectId projId), ..}

instance HasSectionId MoveTask where
    hasSectionId :: Text -> MoveTask -> MoveTask
    hasSectionId secId MoveTask {..} = MoveTask {_section_id = Just (SectionId {_id = secId}), ..}

instance HasParentId MoveTask where
    hasParentId :: Text -> MoveTask -> MoveTask
    hasParentId parId MoveTask {..} = MoveTask {_parent_id = Just (ParentId parId), ..}

emptyMoveTask :: MoveTask
emptyMoveTask =
    MoveTask
        { _project_id = Nothing
        , _section_id = Nothing
        , _parent_id = Nothing
        }

data AddTaskQuick = AddTaskQuick
    { _text :: Text
    , _note :: Maybe Text
    , _reminder :: Maybe Text
    , _auto_reminder :: Bool
    , _meta :: Bool
    }
    deriving (Show, Generic, FromJSON, ToJSON)

addTaskQuickText :: Text -> AddTaskQuick
addTaskQuickText text =
    AddTaskQuick
        { _text = text
        , _note = Nothing
        , _reminder = Nothing
        , _auto_reminder = False
        , _meta = False
        }

data TaskCreate = TaskCreate
    { _content :: Content
    , _description :: Maybe Description
    , _project_id :: Maybe ProjectId
    , _section_id :: Maybe SectionId
    , _parent_id :: Maybe ParentId
    , _order :: Maybe Order
    , _labels :: Maybe [Text]
    , _priority :: Maybe Int
    , _assignee_id :: Maybe Int
    , _due_string :: Maybe Text
    , _due_date :: Maybe Text
    , _due_datetime :: Maybe Text
    , _due_lang :: Maybe Text
    , _duration :: Maybe Int
    , _duration_unit :: Maybe Text
    , _deadline_date :: Maybe Text
    }
    deriving (Show, Generic)

instance ToJSON TaskCreate where
    toJSON :: TaskCreate -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1, omitNothingFields = True}

newTask :: Text -> Initial TaskCreate
newTask content =
    seed
        TaskCreate
            { _content = Content content
            , _description = Nothing
            , _project_id = Nothing
            , _section_id = Nothing
            , _parent_id = Nothing
            , _order = Nothing
            , _labels = Nothing
            , _priority = Nothing
            , _assignee_id = Nothing
            , _due_string = Nothing
            , _due_date = Nothing
            , _due_datetime = Nothing
            , _due_lang = Nothing
            , _duration = Nothing
            , _duration_unit = Nothing
            , _deadline_date = Nothing
            }

instance HasDescription TaskCreate where
    hasDescription :: Text -> TaskCreate -> TaskCreate
    hasDescription desc TaskCreate {..} = TaskCreate {_description = Just (Description desc), ..}

instance HasContent TaskCreate where
    hasContent :: Text -> TaskCreate -> TaskCreate
    hasContent content TaskCreate {..} = TaskCreate {_content = Content content, ..}

instance HasSectionId TaskCreate where
    hasSectionId :: Text -> TaskCreate -> TaskCreate
    hasSectionId sid TaskCreate {..} = TaskCreate {_section_id = Just (SectionId {_id = sid}), ..}

instance HasParentId TaskCreate where
    hasParentId :: Text -> TaskCreate -> TaskCreate
    hasParentId pid TaskCreate {..} = TaskCreate {_parent_id = Just (ParentId pid), ..}

instance HasOrder TaskCreate where
    hasOrder :: Int -> TaskCreate -> TaskCreate
    hasOrder order TaskCreate {..} = TaskCreate {_order = Just (Order order), ..}

instance HasLabels TaskCreate where
    hasLabels :: [Text] -> TaskCreate -> TaskCreate
    hasLabels labels TaskCreate {..} = TaskCreate {_labels = Just labels, ..}

instance HasPriority TaskCreate where
    hasPriority :: Int -> TaskCreate -> TaskCreate
    hasPriority priority TaskCreate {..} = TaskCreate {_priority = Just priority, ..}

instance HasAssigneeId TaskCreate where
    hasAssigneeId :: Int -> TaskCreate -> TaskCreate
    hasAssigneeId aid TaskCreate {..} = TaskCreate {_assignee_id = Just aid, ..}

instance HasDueString TaskCreate where
    hasDueString :: Text -> TaskCreate -> TaskCreate
    hasDueString dueStr TaskCreate {..} = TaskCreate {_due_string = Just dueStr, ..}

instance HasDueDate TaskCreate where
    hasDueDate :: Text -> TaskCreate -> TaskCreate
    hasDueDate dueDate TaskCreate {..} = TaskCreate {_due_date = Just dueDate, ..}

instance HasDueDatetime TaskCreate where
    hasDueDatetime :: Text -> TaskCreate -> TaskCreate
    hasDueDatetime dueDatetime TaskCreate {..} = TaskCreate {_due_datetime = Just dueDatetime, ..}

instance HasDueLang TaskCreate where
    hasDueLang :: Text -> TaskCreate -> TaskCreate
    hasDueLang dueLang TaskCreate {..} = TaskCreate {_due_lang = Just dueLang, ..}

instance HasDuration TaskCreate where
    hasDuration :: Int -> TaskCreate -> TaskCreate
    hasDuration duration TaskCreate {..} = TaskCreate {_duration = Just duration, ..}

instance HasDurationUnit TaskCreate where
    hasDurationUnit :: Text -> TaskCreate -> TaskCreate
    hasDurationUnit durationUnit TaskCreate {..} = TaskCreate {_duration_unit = Just durationUnit, ..}

instance HasDeadlineDate TaskCreate where
    hasDeadlineDate :: Text -> TaskCreate -> TaskCreate
    hasDeadlineDate deadlineDate TaskCreate {..} = TaskCreate {_deadline_date = Just deadlineDate, ..}

instance HasProjectId TaskCreate where
    hasProjectId :: Text -> TaskCreate -> TaskCreate
    hasProjectId projId TaskCreate {..} = TaskCreate {_project_id = Just (ProjectId projId), ..}

data TaskPatch = TaskPatch
    { _content :: Maybe Content
    , _description :: Maybe Description
    , _labels :: Maybe [Text]
    , _priority :: Maybe Int
    , _due_string :: Maybe Text
    , _due_date :: Maybe Text
    , _due_datetime :: Maybe Text
    , _due_lang :: Maybe Text
    , _assignee_id :: Maybe Int
    , _duration :: Maybe Int
    , _duration_unit :: Maybe Text
    , _deadline_date :: Maybe Text
    , _deadline_lang :: Maybe Text
    }
    deriving (Show, Generic)

emptyTaskPatch :: Initial TaskPatch
emptyTaskPatch =
    seed
        TaskPatch
            { _content = Nothing
            , _description = Nothing
            , _labels = Nothing
            , _priority = Nothing
            , _due_string = Nothing
            , _due_date = Nothing
            , _due_datetime = Nothing
            , _due_lang = Nothing
            , _assignee_id = Nothing
            , _duration = Nothing
            , _duration_unit = Nothing
            , _deadline_date = Nothing
            , _deadline_lang = Nothing
            }

instance ToJSON TaskPatch where
    toJSON :: TaskPatch -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1, omitNothingFields = True}

instance HasDescription TaskPatch where
    hasDescription :: Text -> TaskPatch -> TaskPatch
    hasDescription desc TaskPatch {..} = TaskPatch {_description = Just (Description desc), ..}

instance HasContent TaskPatch where
    hasContent :: Text -> TaskPatch -> TaskPatch
    hasContent content TaskPatch {..} = TaskPatch {_content = Just (Content content), ..}

instance HasLabels TaskPatch where
    hasLabels :: [Text] -> TaskPatch -> TaskPatch
    hasLabels labels TaskPatch {..} = TaskPatch {_labels = Just labels, ..}

instance HasPriority TaskPatch where
    hasPriority :: Int -> TaskPatch -> TaskPatch
    hasPriority priority TaskPatch {..} = TaskPatch {_priority = Just priority, ..}

instance HasDueString TaskPatch where
    hasDueString :: Text -> TaskPatch -> TaskPatch
    hasDueString dueStr TaskPatch {..} = TaskPatch {_due_string = Just dueStr, ..}

instance HasDueDate TaskPatch where
    hasDueDate :: Text -> TaskPatch -> TaskPatch
    hasDueDate dueDate TaskPatch {..} = TaskPatch {_due_date = Just dueDate, ..}

instance HasDueDatetime TaskPatch where
    hasDueDatetime :: Text -> TaskPatch -> TaskPatch
    hasDueDatetime dueDatetime TaskPatch {..} = TaskPatch {_due_datetime = Just dueDatetime, ..}

instance HasDueLang TaskPatch where
    hasDueLang :: Text -> TaskPatch -> TaskPatch
    hasDueLang dueLang TaskPatch {..} = TaskPatch {_due_lang = Just dueLang, ..}

instance HasAssigneeId TaskPatch where
    hasAssigneeId :: Int -> TaskPatch -> TaskPatch
    hasAssigneeId aid TaskPatch {..} = TaskPatch {_assignee_id = Just aid, ..}

instance HasDuration TaskPatch where
    hasDuration :: Int -> TaskPatch -> TaskPatch
    hasDuration duration TaskPatch {..} = TaskPatch {_duration = Just duration, ..}

instance HasDurationUnit TaskPatch where
    hasDurationUnit :: Text -> TaskPatch -> TaskPatch
    hasDurationUnit durationUnit TaskPatch {..} = TaskPatch {_duration_unit = Just durationUnit, ..}

instance HasDeadlineDate TaskPatch where
    hasDeadlineDate :: Text -> TaskPatch -> TaskPatch
    hasDeadlineDate deadlineDate TaskPatch {..} = TaskPatch {_deadline_date = Just deadlineDate, ..}

{- | Internal type for parsing completed tasks API response
The API returns full task objects, not just IDs
-}
newtype CompletedTasksQueryParamAPI = CompletedTasksQueryParamAPI
    { items :: [TaskCompletedItem]
    }
    deriving (Show, Generic, FromJSON)

{- | Minimal task representation for completed tasks response
Only contains the id field we need from the completed tasks API
-}
newtype TaskCompletedItem = TaskCompletedItem
    { id :: Text
    }
    deriving (Show, Generic, FromJSON)

class (Monad m) => TodoistTaskM m where
    -- | Get tasks (automatically fetches all pages)
    getTasks :: TaskParam -> m [Task]

    getTask :: TaskId -> m Task

    addTask :: TaskCreate -> m NewTask -- todo: should return Task; TaskCreate should be named NewTask

    updateTask :: TaskId -> TaskPatch -> m NewTask -- todo: should return Task

    closeTask :: TaskId -> m ()

    uncloseTask :: TaskId -> m ()

    deleteTask :: TaskId -> m ()

    -- | Get tasks by filter (automatically fetches all pages)
    getTasksByFilter :: TaskFilter -> m [TaskId]

    moveTask :: TaskId -> MoveTask -> m TaskId

    addTaskQuick :: AddTaskQuick -> m ()

    getCompletedTasksByDueDate :: CompletedTasksQueryParam -> m [TaskId]

    getCompletedTasksByCompletionDate :: CompletedTasksQueryParam -> m [TaskId]

    {- | Get tasks with manual pagination control
    Returns a tuple of (results, next_cursor) for the requested page
    -}
    getTasksPaginated :: TaskParam -> m ([TaskId], Maybe Text)

    {- | Get tasks by filter with manual pagination control
    Returns a tuple of (results, next_cursor) for the requested page
    Note: TaskFilter already has cursor and limit fields
    -}
    getTasksByFilterPaginated :: TaskFilter -> m ([TaskId], Maybe Text)

    -- | Get all tasks with custom page size (fetches all pages automatically)
    getTasksWithLimit :: TaskParam -> Int -> m [TaskId]

    -- | Get all tasks by filter with custom page size (fetches all pages automatically)
    getTasksByFilterWithLimit :: TaskFilter -> Int -> m [TaskId]

-- | Query parameters for filtering tasks
data TaskParam = TaskParam
    { project_id :: Maybe Text
    , section_id :: Maybe Text
    , parent_id :: Maybe Text
    , task_ids :: [Text]
    , cursor :: Maybe Text
    , limit :: Maybe Int
    }
    deriving (Show)

instance QueryParam TaskParam where
    toQueryParam :: TaskParam -> Params
    toQueryParam TaskParam {..} =
        maybe [] (\projId -> [("project_id", projId)]) project_id
            <> maybe [] (\secId -> [("section_id", secId)]) section_id
            <> maybe [] (\parId -> [("parent_id", parId)]) parent_id
            <> L.map ("task_id",) task_ids
            <> maybe [] (\c -> [("cursor", c)]) cursor
            <> maybe [] (\l -> [("limit", Data.Text.show l)]) limit

-- | Query parameters for filtering tasks by text query
data TaskFilter = TaskFilter
    { query :: Text
    , lang :: Maybe Text
    , cursor :: Maybe Text
    , limit :: Maybe Int
    }
    deriving (Show)

instance QueryParam TaskFilter where
    toQueryParam :: TaskFilter -> Params
    toQueryParam TaskFilter {..} =
        [("query", query)]
            <> maybe [] (\p -> [("lang", p)]) lang
            <> maybe [] (\p -> [("cursor", p)]) cursor
            <> maybe [] (\p -> [("limit", Data.Text.show p)]) limit

taskFilterWithQuery :: Text -> TaskFilter
taskFilterWithQuery query =
    TaskFilter
        { query
        , lang = Nothing
        , cursor = Nothing
        , limit = Nothing
        }

-- | Query parameters for getting completed tasks
data CompletedTasksQueryParam = CompletedTasksQueryParam
    { since :: Text
    , until :: Text
    , workspace_id :: Maybe Text
    , project_id :: Maybe Text
    , section_id :: Maybe Text
    , parent_id :: Maybe Text
    , filter_query :: Maybe Text
    , filter_lang :: Maybe Text
    , cursor :: Maybe Text
    , limit :: Int
    }
    deriving (Show)

instance QueryParam CompletedTasksQueryParam where
    toQueryParam :: CompletedTasksQueryParam -> Params
    toQueryParam CompletedTasksQueryParam {..} =
        [("since", since)]
            <> [("until", until)]
            <> maybe [] (\p -> [("workspace_id", p)]) workspace_id
            <> maybe [] (\p -> [("project_id", p)]) project_id
            <> maybe [] (\p -> [("section_id", p)]) section_id
            <> maybe [] (\p -> [("parent_id", p)]) parent_id
            <> maybe [] (\p -> [("filter_query", p)]) filter_query
            <> maybe [] (\p -> [("filter_lang", p)]) filter_lang
            <> maybe [] (\p -> [("cursor", p)]) cursor
            <> [("limit", Data.Text.show limit)]

addTaskQuickWithQuery :: Text -> Text -> CompletedTasksQueryParam
addTaskQuickWithQuery since until =
    CompletedTasksQueryParam
        { since
        , until
        , workspace_id = Nothing
        , project_id = Nothing
        , section_id = Nothing
        , parent_id = Nothing
        , filter_query = Nothing
        , filter_lang = Nothing
        , cursor = Nothing
        , limit = 50
        }

setProjectId :: Text -> TaskParam -> TaskParam
setProjectId pid TaskParam {..} = TaskParam {project_id = Just pid, ..}
