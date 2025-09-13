{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web.Todoist.Task (
  TodoistTaskM(..),
  TaskId(..),
  TaskParam,
  Task(..),
  NewTask,
  MoveTask,
  AddTaskQuick,
  emptyTask,
  emptyMoveTask,
  addTaskQuickText,
  CompletedTasksQueryParamAPI(..)
) where

import Web.Todoist.Patch 
import Web.Todoist.Param 

import Data.Maybe
import Data.Text
import Data.String (String)
import Text.Show (Show)
import Control.Monad (Monad)
import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types
import Data.Bool
import Data.Int (Int)
import qualified Data.List as L

-- TODO: use Text
newtype TaskId = TaskId {
  p_id :: Text
} deriving (Show, Generic)

instance FromJSON TaskId where
  parseJSON :: Value -> Parser TaskId
  parseJSON = genericParseJSON jsonOpts

instance ToJSON TaskId where
  toJSON :: TaskId -> Value
  toJSON = genericToJSON jsonOpts

data Task = Task {
  p_id :: TaskId,
  p_projectId :: String
} deriving (Show, Generic, FromJSON)

instance ToJSON Task where
  toJSON :: Task -> Value
  toJSON = genericToJSON jsonOpts

data NewTask = NewTask {
  p_user_id         :: Text
  , p_id              :: Text
  , p_project_id      :: Text
  , p_section_id      :: Maybe Text
  , p_parent_id       :: Maybe Text
  , p_added_by_uid    :: Maybe Text
  , p_assigned_by_uid :: Maybe Text
  , p_responsible_uid :: Maybe Text
  , p_labels          :: [Text]
  , p_checked         :: Bool
  , p_is_deleted      :: Bool
  , p_added_at        :: Maybe Text
  , p_completed_at    :: Maybe Text
  , p_updated_at      :: Maybe Text
  , p_priority        :: Int
  , p_child_order     :: Int
  , p_content         :: Text
  , p_description     :: Text
  , p_note_count      :: Int
  , p_day_order       :: Int
  , p_is_collapsed    :: Bool
} deriving (Show, Generic)

emptyTask :: NewTask
emptyTask = NewTask {
        p_user_id         = ""
      , p_id              = ""
      , p_project_id      = ""
      , p_section_id      = Nothing
      , p_parent_id       = Nothing
      , p_added_by_uid    = Nothing
      , p_assigned_by_uid = Nothing
      , p_responsible_uid = Nothing
      , p_labels          = []
      , p_checked         = False
      , p_is_deleted      = False
      , p_added_at        = Nothing
      , p_completed_at    = Nothing
      , p_updated_at      = Nothing
      , p_priority        = 0
      , p_child_order     = 0
      , p_content         = ""
      , p_description     = ""
      , p_note_count      = 0
      , p_day_order       = 0
      , p_is_collapsed    = False
    }

instance FromJSON NewTask where
  parseJSON :: Value -> Parser NewTask
  parseJSON = genericParseJSON jsonOpts

instance ToJSON NewTask where
  toJSON :: NewTask -> Value
  toJSON = genericToJSON jsonOpts

data MoveTask = MoveTask {
    p_project_id      :: Maybe Text
  , p_section_id      :: Maybe Text
  , p_parent_id       :: Maybe Text
} deriving (Show, Generic)

instance ToJSON MoveTask where
  toJSON :: MoveTask -> Value
  toJSON = genericToJSON jsonOpts

instance FromJSON MoveTask where
  parseJSON :: Value -> Parser MoveTask
  parseJSON = genericParseJSON jsonOpts

emptyMoveTask :: MoveTask
emptyMoveTask = MoveTask {
    p_project_id      = Nothing
  , p_section_id      = Nothing
  , p_parent_id       = Nothing
} 

data AddTaskQuick = AddTaskQuick {
  text :: Text,
  note :: Maybe Text,
  reminder :: Maybe Text,
  auto_reminder :: Bool,
  meta :: Bool
} deriving (Show, Generic, FromJSON, ToJSON)

addTaskQuickText :: Text -> AddTaskQuick
addTaskQuickText text = AddTaskQuick {
  text,
  note = Nothing,
  reminder = Nothing,
  auto_reminder = False,
  meta = False
}

newtype CompletedTasksQueryParamAPI = CompletedTasksQueryParam {
  items :: [TaskId]
} deriving (Show, Generic, FromJSON)

class Monad m => TodoistTaskM m where
  -- todo write algebraic laws
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

jsonOpts :: Options
jsonOpts = defaultOptions { fieldLabelModifier = L.drop 2, omitNothingFields = True }