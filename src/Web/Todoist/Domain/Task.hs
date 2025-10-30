{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- TODO: Create Web.Todoist.Types module for shared data types
-- TODO: Create Web.Todoist.Error module for TodoistError and related types
module Web.Todoist.Domain.Task
    ( TodoistTaskM (..)
    , TaskId (..)
    , TaskParam (..)
    , Task (..)
    , NewTask
    , MoveTask
    , AddTaskQuick
    , emptyTask
    , emptyMoveTask
    , addTaskQuickText
    , CompletedTasksQueryParamAPI (..)
    , TaskFilter (..)
    , CompletedTasksQueryParam (..)
    , addTaskQuickWithQuery
    , TaskCreate
    , TaskPatch
    , newTask
    -- , setDescription
    , emptyTaskPatch
    ) where

import Web.Todoist.Builder.Has (HasDescription (..))
import Web.Todoist.Internal.Json (jsonOpts)
import Web.Todoist.Internal.Types (Params)
import Web.Todoist.QueryParam (QueryParam (..))

import Control.Monad (Monad)
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON (toJSON)
    , Value
    , genericParseJSON
    , genericToJSON
    )
import Data.Aeson.Types (Parser)
import Data.Bool (Bool (False))
import Data.Int (Int)
import qualified Data.List as L
import Data.Maybe (Maybe (..), maybe)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Text (Text, show)
import GHC.Generics (Generic)
import Text.Show (Show)

-- TODO: use Text
-- TODO: NAMING - Remove p_ prefix from record fields, use proper field names
newtype TaskId = TaskId
    { _id :: Text
    }
    deriving (Show, Generic)

instance FromJSON TaskId where
    parseJSON :: Value -> Parser TaskId
    parseJSON = genericParseJSON jsonOpts

instance ToJSON TaskId where
    toJSON :: TaskId -> Value
    toJSON = genericToJSON jsonOpts

-- TODO: TYPE_SAFETY - Replace String with Text for consistency
data Task = Task
    { _tid :: TaskId
    , _projectId :: String
    }
    deriving (Show, Generic, FromJSON)

instance ToJSON Task where
    toJSON :: Task -> Value
    toJSON = genericToJSON jsonOpts

-- TODO: NAMING - Remove p_ prefix from all record fields
-- TODO: DOCUMENTATION - Add Haddock documentation for all exported types
data NewTask = NewTask
    { p_user_id :: Text
    , p_id :: Text
    , p_project_id :: Text
    , p_section_id :: Maybe Text
    , p_parent_id :: Maybe Text
    , p_added_by_uid :: Maybe Text
    , p_assigned_by_uid :: Maybe Text
    , p_responsible_uid :: Maybe Text
    , p_labels :: [Text]
    , p_checked :: Bool
    , p_is_deleted :: Bool
    , p_added_at :: Maybe Text
    , p_completed_at :: Maybe Text
    , p_updated_at :: Maybe Text
    , p_priority :: Int
    , p_child_order :: Int
    , p_content :: Text
    , p_description :: Text
    , p_note_count :: Int
    , p_day_order :: Int
    , p_is_collapsed :: Bool
    }
    deriving (Show, Generic)

emptyTask :: NewTask
emptyTask =
    NewTask
        { p_user_id = ""
        , p_id = ""
        , p_project_id = ""
        , p_section_id = Nothing
        , p_parent_id = Nothing
        , p_added_by_uid = Nothing
        , p_assigned_by_uid = Nothing
        , p_responsible_uid = Nothing
        , p_labels = []
        , p_checked = False
        , p_is_deleted = False
        , p_added_at = Nothing
        , p_completed_at = Nothing
        , p_updated_at = Nothing
        , p_priority = 0
        , p_child_order = 0
        , p_content = ""
        , p_description = ""
        , p_note_count = 0
        , p_day_order = 0
        , p_is_collapsed = False
        }

instance FromJSON NewTask where
    parseJSON :: Value -> Parser NewTask
    parseJSON = genericParseJSON jsonOpts

instance ToJSON NewTask where
    toJSON :: NewTask -> Value
    toJSON = genericToJSON jsonOpts

data MoveTask = MoveTask
    { p_project_id :: Maybe Text
    , p_section_id :: Maybe Text
    , p_parent_id :: Maybe Text
    }
    deriving (Show, Generic)

instance ToJSON MoveTask where
    toJSON :: MoveTask -> Value
    toJSON = genericToJSON jsonOpts

instance FromJSON MoveTask where
    parseJSON :: Value -> Parser MoveTask
    parseJSON = genericParseJSON jsonOpts

emptyMoveTask :: MoveTask
emptyMoveTask =
    MoveTask
        { p_project_id = Nothing
        , p_section_id = Nothing
        , p_parent_id = Nothing
        }

data AddTaskQuick = AddTaskQuick
    { text :: Text
    , note :: Maybe Text
    , reminder :: Maybe Text
    , auto_reminder :: Bool
    , meta :: Bool
    }
    deriving (Show, Generic, FromJSON, ToJSON)

addTaskQuickText :: Text -> AddTaskQuick
addTaskQuickText text =
    AddTaskQuick
        { text
        , note = Nothing
        , reminder = Nothing
        , auto_reminder = False
        , meta = False
        }

data TaskCreate = TaskCreate
    { p_content :: String
    , p_description :: Maybe Text
    , p_project_id :: Maybe String
    }
    deriving (Show, Generic)

instance ToJSON TaskCreate where
    toJSON :: TaskCreate -> Value
    toJSON = genericToJSON jsonOpts

newTask :: String -> TaskCreate
newTask content =
    TaskCreate
        { p_content = content
        , p_description = Nothing
        , p_project_id = Nothing
        }

instance HasDescription TaskCreate where
    hasDescription :: Text -> TaskCreate -> TaskCreate
    hasDescription desc TaskCreate {..} = TaskCreate {p_description = Just desc, ..}

data TaskPatch = TaskPatch
    { p_content :: Maybe Text
    , p_description :: Maybe Text
    , p_labels :: Maybe Text
    , p_priority :: Maybe Text
    , p_due_string :: Maybe Text
    , p_due_date :: Maybe Text
    , p_due_datetime :: Maybe Text
    , p_due_lang :: Maybe Text
    , p_assignee_id :: Maybe Text
    , p_duration :: Maybe Text
    , p_duration_unit :: Maybe Text
    , p_deadline_date :: Maybe Text
    , p_deadline_lang :: Maybe Text
    }
    deriving (Show, Generic)

emptyTaskPatch :: TaskPatch
emptyTaskPatch =
    TaskPatch
        { p_content = Nothing
        , p_description = Nothing
        , p_labels = Nothing
        , p_priority = Nothing
        , p_due_string = Nothing
        , p_due_date = Nothing
        , p_due_datetime = Nothing
        , p_due_lang = Nothing
        , p_assignee_id = Nothing
        , p_duration = Nothing
        , p_duration_unit = Nothing
        , p_deadline_date = Nothing
        , p_deadline_lang = Nothing
        }

instance ToJSON TaskPatch where
    toJSON :: TaskPatch -> Value
    toJSON = genericToJSON jsonOpts

instance HasDescription TaskPatch where
    hasDescription :: Text -> TaskPatch -> TaskPatch
    hasDescription desc TaskPatch {..} = TaskPatch {p_description = Just desc, ..}

newtype CompletedTasksQueryParamAPI = CompletedTasksQueryParamAPI
    { items :: [TaskId]
    }
    deriving (Show, Generic, FromJSON)

class (Monad m) => TodoistTaskM m where
    -- TODO: DOCUMENTATION - Write algebraic laws
    getTasks :: TaskParam -> m [TaskId]

    getTask :: TaskId -> m Task

    addTask :: TaskCreate -> m NewTask

    updateTask :: TaskId -> TaskPatch -> m NewTask

    closeTask :: TaskId -> m ()

    uncloseTask :: TaskId -> m ()

    deleteTask :: TaskId -> m ()

    getTasksByFilter :: TaskFilter -> m [TaskId]

    moveTask :: TaskId -> MoveTask -> m TaskId

    addTaskQuick :: AddTaskQuick -> m ()

    getCompletedTasksByDueDate :: CompletedTasksQueryParam -> m [TaskId]

    getCompletedTasksByCompletionDate :: CompletedTasksQueryParam -> m [TaskId]

-- | Query parameters for filtering tasks
data TaskParam = TaskParam
    { project_id :: Maybe Text
    , section_id :: Maybe Text
    , parent_id :: Maybe Text
    , task_ids :: [Text]
    }
    deriving (Show)

instance QueryParam TaskParam where
    toQueryParam :: TaskParam -> Params
    toQueryParam TaskParam {..} =
        maybe [] (\projId -> [("project_id", projId)]) project_id
            <> maybe [] (\secId -> [("section_id", secId)]) section_id
            <> maybe [] (\parId -> [("parent_id", parId)]) parent_id
            <> L.map ("task_id",) task_ids

emptyParams :: TaskParam
emptyParams =
    TaskParam
        { project_id = Nothing
        , section_id = Nothing
        , parent_id = Nothing
        , task_ids = []
        }

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
            <> maybe [] (\p -> [("limit", show p)]) limit

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
            <> [("limit", show limit)]

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
