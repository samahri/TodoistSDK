{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Web.Todoist.Domain.Task
Description : Task API types and operations for Todoist REST API
Copyright   : (c) 2025 Sam S. Almahri
License     : MIT
Maintainer  : sam.salmahri@gmail.com

This module provides types and operations for working with Todoist tasks.
Tasks are the core items in Todoist, supporting due dates, priorities, labels,
assignees, and hierarchical sub-tasks.

= Usage Example

@
import Web.Todoist.Domain.Task
import Web.Todoist.Util.Builder
import Web.Todoist.Runner

main :: IO ()
main = do
    let config = newTodoistConfig "your-api-token"

    -- Create a new task
    let task = runBuilder (newTaskBuilder "Buy groceries")
               (withDescription "Milk, eggs, bread" <> withPriority 2)
    taskId <- todoist config (createTask task)

    -- Get all tasks with builder pattern
    let params = runBuilder taskParamBuilder (withProjectId "project-123" <> withLimit 50)
    tasks <- todoist config (getTasks params)

    -- Complete a task
    todoist config (closeTask taskId)
@

For more details, see: <https://developer.todoist.com/rest/v2/#tasks>
-}
module Web.Todoist.Domain.Task
    ( TodoistTaskM (..)
    , TaskParam (..)
    , Task (..)
    , Due (..)
    , Deadline (..)
    , Duration (..)
    , DurationUnit (..)
    , NewTask (..)
    , MoveTask
    , AddTaskQuick
    , addTaskQuickText
    , CompletedTasksQueryParamAPI (..)
    , TaskCompletedItem (..)
    , TaskFilter (..)
    , CompletedTasksQueryParam (..)
    , TaskCreate
    , TaskUpdate
    , newTaskBuilder
    , moveTaskBuilder
    , updateTaskBuilder
    , taskParamBuilder
    , filterTaskBuilder
    , completedTasksQueryParamBuilder
      -- * Lenses
      -- ** Duration
    , amount
    , unit
      -- ** Deadline
    , deadlineDate
    , deadlineLang
      -- ** Due
    , dueDate
    , dueString
    , dueLang
    , dueIsRecurring
    , dueTimezone
      -- ** Task
    , taskId
    , taskContent
    , taskDescription
    , taskProjectId
    , taskSectionId
    , taskParentId
    , taskLabels
    , taskPriority
    , taskDue
    , taskDeadline
    , taskDuration
    , taskIsCollapsed
    , taskOrder
    , taskAssigneeId
    , taskAssignerId
    , taskCompletedAt
    , taskCreatorId
    , taskCreatedAt
    , taskUpdatedAt
      -- ** NewTask
    , newTaskUserId
    , newTaskId
    , newTaskProjectId
    , newTaskSectionId
    , newTaskParentId
    , newTaskAddedByUid
    , newTaskAssignedByUid
    , newTaskResponsibleUid
    , newTaskLabels
    , newTaskChecked
    , newTaskIsDeleted
    , newTaskAddedAt
    , newTaskCompletedAt
    , newTaskUpdatedAt
    , newTaskPriority
    , newTaskChildOrder
    , newTaskContent
    , newTaskDescription
    , newTaskNoteCount
    , newTaskDayOrder
    , newTaskIsCollapsed
      -- ** MoveTask
    , moveTaskProjectId
    , moveTaskSectionId
    , moveTaskParentId
      -- ** AddTaskQuick
    , addTaskQuickTextLens
    , addTaskQuickNote
    , addTaskQuickReminder
    , addTaskQuickAutoReminder
    , addTaskQuickMeta
    ) where

import Web.Todoist.Internal.Types (Params)
import Web.Todoist.Util.Builder
    ( HasAssigneeId (..)
    , HasContent (..)
    , HasCursor (..)
    , HasDeadlineDate (..)
    , HasDescription (..)
    , HasDueDate (..)
    , HasDueDatetime (..)
    , HasDueLang (..)
    , HasDueString (..)
    , HasDuration (..)
    , HasDurationUnit (..)
    , HasFilterLang (..)
    , HasFilterQuery (..)
    , HasLabels (..)
    , HasLang (..)
    , HasLimit (..)
    , HasOrder (..)
    , HasParentId (..)
    , HasPriority (..)
    , HasProjectId (..)
    , HasSectionId (..)
    , HasTaskIds (..)
    , Initial
    , seed
    )
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
import Lens.Micro (to)
import Text.Show (Show)
import Web.Todoist.Lens (Getter)
import Web.Todoist.Domain.Section (SectionId (..))
import Web.Todoist.Domain.Types
    ( Content (..)
    , Description (..)
    , IsCollapsed (..)
    , Order (..)
    , ParentId (..)
    , ProjectId (..)
    , TaskId (..)
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

-- | Lenses for Duration
amount :: Getter Duration Int
amount = to _amount
{-# INLINE amount #-}

unit :: Getter Duration DurationUnit
unit = to _unit
{-# INLINE unit #-}

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

-- | Lenses for Deadline
deadlineDate :: Getter Deadline Text
deadlineDate = to (\(Deadline {_date = x}) -> x)
{-# INLINE deadlineDate #-}

deadlineLang :: Getter Deadline Text
deadlineLang = to (\(Deadline {_lang = x}) -> x)
{-# INLINE deadlineLang #-}

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

-- | Lenses for Due
dueDate :: Getter Due Text
dueDate = to (\(Due {_date = x}) -> x)
{-# INLINE dueDate #-}

dueString :: Getter Due Text
dueString = to (\(Due {_string = x}) -> x)
{-# INLINE dueString #-}

dueLang :: Getter Due Text
dueLang = to (\(Due {_lang = x}) -> x)
{-# INLINE dueLang #-}

dueIsRecurring :: Getter Due Bool
dueIsRecurring = to (\(Due {_is_recurring = x}) -> x)
{-# INLINE dueIsRecurring #-}

dueTimezone :: Getter Due (Maybe Text)
dueTimezone = to (\(Due {_timezone = x}) -> x)
{-# INLINE dueTimezone #-}

{- | Task domain type representing a Todoist task

Contains all task metadata including content, project/section assignment,
due dates, priority, labels, and completion status. Tasks can have sub-tasks
using parent_id and support rich due date information with recurring patterns.
-}
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

-- | Lenses for Task
taskId :: Getter Task TaskId
taskId = to (\(Task {_id = x}) -> x)
{-# INLINE taskId #-}

taskContent :: Getter Task Content
taskContent = to (\(Task {_content = x}) -> x)
{-# INLINE taskContent #-}

taskDescription :: Getter Task Description
taskDescription = to (\(Task {_description = x}) -> x)
{-# INLINE taskDescription #-}

taskProjectId :: Getter Task ProjectId
taskProjectId = to (\(Task {_project_id = x}) -> x)
{-# INLINE taskProjectId #-}

taskSectionId :: Getter Task (Maybe SectionId)
taskSectionId = to (\(Task {_section_id = x}) -> x)
{-# INLINE taskSectionId #-}

taskParentId :: Getter Task (Maybe ParentId)
taskParentId = to (\(Task {_parent_id = x}) -> x)
{-# INLINE taskParentId #-}

taskLabels :: Getter Task [Text]
taskLabels = to (\(Task {_labels = x}) -> x)
{-# INLINE taskLabels #-}

taskPriority :: Getter Task Int
taskPriority = to (\(Task {_priority = x}) -> x)
{-# INLINE taskPriority #-}

taskDue :: Getter Task (Maybe Due)
taskDue = to (\(Task {_due = x}) -> x)
{-# INLINE taskDue #-}

taskDeadline :: Getter Task (Maybe Deadline)
taskDeadline = to (\(Task {_deadline = x}) -> x)
{-# INLINE taskDeadline #-}

taskDuration :: Getter Task (Maybe Duration)
taskDuration = to (\(Task {_duration = x}) -> x)
{-# INLINE taskDuration #-}

taskIsCollapsed :: Getter Task IsCollapsed
taskIsCollapsed = to (\(Task {_is_collapsed = x}) -> x)
{-# INLINE taskIsCollapsed #-}

taskOrder :: Getter Task Order
taskOrder = to (\(Task {_order = x}) -> x)
{-# INLINE taskOrder #-}

taskAssigneeId :: Getter Task (Maybe Uid)
taskAssigneeId = to (\(Task {_assignee_id = x}) -> x)
{-# INLINE taskAssigneeId #-}

taskAssignerId :: Getter Task (Maybe Uid)
taskAssignerId = to (\(Task {_assigner_id = x}) -> x)
{-# INLINE taskAssignerId #-}

taskCompletedAt :: Getter Task (Maybe Text)
taskCompletedAt = to (\(Task {_completed_at = x}) -> x)
{-# INLINE taskCompletedAt #-}

taskCreatorId :: Getter Task Uid
taskCreatorId = to (\(Task {_creator_id = x}) -> x)
{-# INLINE taskCreatorId #-}

taskCreatedAt :: Getter Task Text
taskCreatedAt = to (\(Task {_created_at = x}) -> x)
{-# INLINE taskCreatedAt #-}

taskUpdatedAt :: Getter Task Text
taskUpdatedAt = to (\(Task {_updated_at = x}) -> x)
{-# INLINE taskUpdatedAt #-}

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

-- | Lenses for NewTask
newTaskUserId :: Getter NewTask Text
newTaskUserId = to (\(NewTask {_user_id = x}) -> x)
{-# INLINE newTaskUserId #-}

newTaskId :: Getter NewTask TaskId
newTaskId = to (\(NewTask {_id = x}) -> x)
{-# INLINE newTaskId #-}

newTaskProjectId :: Getter NewTask ProjectId
newTaskProjectId = to (\(NewTask {_project_id = x}) -> x)
{-# INLINE newTaskProjectId #-}

newTaskSectionId :: Getter NewTask (Maybe SectionId)
newTaskSectionId = to (\(NewTask {_section_id = x}) -> x)
{-# INLINE newTaskSectionId #-}

newTaskParentId :: Getter NewTask (Maybe ParentId)
newTaskParentId = to (\(NewTask {_parent_id = x}) -> x)
{-# INLINE newTaskParentId #-}

newTaskAddedByUid :: Getter NewTask (Maybe Uid)
newTaskAddedByUid = to (\(NewTask {_added_by_uid = x}) -> x)
{-# INLINE newTaskAddedByUid #-}

newTaskAssignedByUid :: Getter NewTask (Maybe Uid)
newTaskAssignedByUid = to (\(NewTask {_assigned_by_uid = x}) -> x)
{-# INLINE newTaskAssignedByUid #-}

newTaskResponsibleUid :: Getter NewTask (Maybe Uid)
newTaskResponsibleUid = to (\(NewTask {_responsible_uid = x}) -> x)
{-# INLINE newTaskResponsibleUid #-}

newTaskLabels :: Getter NewTask [Text]
newTaskLabels = to (\(NewTask {_labels = x}) -> x)
{-# INLINE newTaskLabels #-}

newTaskChecked :: Getter NewTask Bool
newTaskChecked = to (\(NewTask {_checked = x}) -> x)
{-# INLINE newTaskChecked #-}

newTaskIsDeleted :: Getter NewTask Bool
newTaskIsDeleted = to (\(NewTask {_is_deleted = x}) -> x)
{-# INLINE newTaskIsDeleted #-}

newTaskAddedAt :: Getter NewTask (Maybe Text)
newTaskAddedAt = to (\(NewTask {_added_at = x}) -> x)
{-# INLINE newTaskAddedAt #-}

newTaskCompletedAt :: Getter NewTask (Maybe Text)
newTaskCompletedAt = to (\(NewTask {_completed_at = x}) -> x)
{-# INLINE newTaskCompletedAt #-}

newTaskUpdatedAt :: Getter NewTask (Maybe Text)
newTaskUpdatedAt = to (\(NewTask {_updated_at = x}) -> x)
{-# INLINE newTaskUpdatedAt #-}

newTaskPriority :: Getter NewTask Int
newTaskPriority = to (\(NewTask {_priority = x}) -> x)
{-# INLINE newTaskPriority #-}

newTaskChildOrder :: Getter NewTask Order
newTaskChildOrder = to (\(NewTask {_child_order = x}) -> x)
{-# INLINE newTaskChildOrder #-}

newTaskContent :: Getter NewTask Content
newTaskContent = to (\(NewTask {_content = x}) -> x)
{-# INLINE newTaskContent #-}

newTaskDescription :: Getter NewTask Description
newTaskDescription = to (\(NewTask {_description = x}) -> x)
{-# INLINE newTaskDescription #-}

newTaskNoteCount :: Getter NewTask Int
newTaskNoteCount = to (\(NewTask {_note_count = x}) -> x)
{-# INLINE newTaskNoteCount #-}

newTaskDayOrder :: Getter NewTask Order
newTaskDayOrder = to (\(NewTask {_day_order = x}) -> x)
{-# INLINE newTaskDayOrder #-}

newTaskIsCollapsed :: Getter NewTask IsCollapsed
newTaskIsCollapsed = to (\(NewTask {_is_collapsed = x}) -> x)
{-# INLINE newTaskIsCollapsed #-}

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

-- | Lenses for MoveTask
moveTaskProjectId :: Getter MoveTask (Maybe ProjectId)
moveTaskProjectId = to (\(MoveTask {_project_id = x}) -> x)
{-# INLINE moveTaskProjectId #-}

moveTaskSectionId :: Getter MoveTask (Maybe SectionId)
moveTaskSectionId = to (\(MoveTask {_section_id = x}) -> x)
{-# INLINE moveTaskSectionId #-}

moveTaskParentId :: Getter MoveTask (Maybe ParentId)
moveTaskParentId = to (\(MoveTask {_parent_id = x}) -> x)
{-# INLINE moveTaskParentId #-}

-- | Create empty MoveTask for use with builder pattern
moveTaskBuilder :: Initial MoveTask
moveTaskBuilder =
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

data AddTaskQuick = AddTaskQuick
    { _text :: Text
    , _note :: Maybe Text
    , _reminder :: Maybe Text
    , _auto_reminder :: Bool
    , _meta :: Bool
    }
    deriving (Show, Generic, FromJSON, ToJSON)

-- | Lenses for AddTaskQuick
addTaskQuickTextLens :: Getter AddTaskQuick Text
addTaskQuickTextLens = to (\(AddTaskQuick {_text = x}) -> x)
{-# INLINE addTaskQuickTextLens #-}

addTaskQuickNote :: Getter AddTaskQuick (Maybe Text)
addTaskQuickNote = to (\(AddTaskQuick {_note = x}) -> x)
{-# INLINE addTaskQuickNote #-}

addTaskQuickReminder :: Getter AddTaskQuick (Maybe Text)
addTaskQuickReminder = to (\(AddTaskQuick {_reminder = x}) -> x)
{-# INLINE addTaskQuickReminder #-}

addTaskQuickAutoReminder :: Getter AddTaskQuick Bool
addTaskQuickAutoReminder = to (\(AddTaskQuick {_auto_reminder = x}) -> x)
{-# INLINE addTaskQuickAutoReminder #-}

addTaskQuickMeta :: Getter AddTaskQuick Bool
addTaskQuickMeta = to (\(AddTaskQuick {_meta = x}) -> x)
{-# INLINE addTaskQuickMeta #-}

addTaskQuickText :: Text -> AddTaskQuick
addTaskQuickText text =
    AddTaskQuick
        { _text = text
        , _note = Nothing
        , _reminder = Nothing
        , _auto_reminder = False
        , _meta = False
        }

{- | Request body for creating a new task

Only content is required. Use 'newTaskBuilder' with the builder pattern for
ergonomic construction of tasks with optional fields.
-}
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

-- | Create new TaskCreate with required content parameter
newTaskBuilder :: Text -> Initial TaskCreate
newTaskBuilder content =
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

{- | Request body for updating an existing task (partial updates)

All fields are optional (using Maybe). Only provided fields will be updated.
Use 'updateTaskBuilder' with the builder pattern for updates.
-}
data TaskUpdate = TaskUpdate
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

updateTaskBuilder :: Initial TaskUpdate
updateTaskBuilder =
    seed
        TaskUpdate
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

instance ToJSON TaskUpdate where
    toJSON :: TaskUpdate -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1, omitNothingFields = True}

instance HasDescription TaskUpdate where
    hasDescription :: Text -> TaskUpdate -> TaskUpdate
    hasDescription desc TaskUpdate {..} = TaskUpdate {_description = Just (Description desc), ..}

instance HasContent TaskUpdate where
    hasContent :: Text -> TaskUpdate -> TaskUpdate
    hasContent content TaskUpdate {..} = TaskUpdate {_content = Just (Content content), ..}

instance HasLabels TaskUpdate where
    hasLabels :: [Text] -> TaskUpdate -> TaskUpdate
    hasLabels labels TaskUpdate {..} = TaskUpdate {_labels = Just labels, ..}

instance HasPriority TaskUpdate where
    hasPriority :: Int -> TaskUpdate -> TaskUpdate
    hasPriority priority TaskUpdate {..} = TaskUpdate {_priority = Just priority, ..}

instance HasDueString TaskUpdate where
    hasDueString :: Text -> TaskUpdate -> TaskUpdate
    hasDueString dueStr TaskUpdate {..} = TaskUpdate {_due_string = Just dueStr, ..}

instance HasDueDate TaskUpdate where
    hasDueDate :: Text -> TaskUpdate -> TaskUpdate
    hasDueDate dueDate TaskUpdate {..} = TaskUpdate {_due_date = Just dueDate, ..}

instance HasDueDatetime TaskUpdate where
    hasDueDatetime :: Text -> TaskUpdate -> TaskUpdate
    hasDueDatetime dueDatetime TaskUpdate {..} = TaskUpdate {_due_datetime = Just dueDatetime, ..}

instance HasDueLang TaskUpdate where
    hasDueLang :: Text -> TaskUpdate -> TaskUpdate
    hasDueLang dueLang TaskUpdate {..} = TaskUpdate {_due_lang = Just dueLang, ..}

instance HasAssigneeId TaskUpdate where
    hasAssigneeId :: Int -> TaskUpdate -> TaskUpdate
    hasAssigneeId aid TaskUpdate {..} = TaskUpdate {_assignee_id = Just aid, ..}

instance HasDuration TaskUpdate where
    hasDuration :: Int -> TaskUpdate -> TaskUpdate
    hasDuration duration TaskUpdate {..} = TaskUpdate {_duration = Just duration, ..}

instance HasDurationUnit TaskUpdate where
    hasDurationUnit :: Text -> TaskUpdate -> TaskUpdate
    hasDurationUnit durationUnit TaskUpdate {..} = TaskUpdate {_duration_unit = Just durationUnit, ..}

instance HasDeadlineDate TaskUpdate where
    hasDeadlineDate :: Text -> TaskUpdate -> TaskUpdate
    hasDeadlineDate deadlineDate TaskUpdate {..} = TaskUpdate {_deadline_date = Just deadlineDate, ..}

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

    -- | Get a single task by ID
    getTask :: TaskId -> m Task

    -- | Create a new task
    createTask :: TaskCreate -> m NewTask -- todo: should return Task; TaskCreate should be named NewTask

    -- | Update an existing task with partial changes
    updateTask :: TaskUpdate -> TaskId -> m NewTask -- todo: should return Task

    -- | Mark a task as completed
    closeTask :: TaskId -> m ()

    -- | Reopen a previously completed task
    uncloseTask :: TaskId -> m ()

    -- | Permanently delete a task
    deleteTask :: TaskId -> m ()

    -- | Get tasks by filter (automatically fetches all pages)
    getTasksByFilter :: TaskFilter -> m [TaskId]

    -- | Move a task to a different project or section
    moveTask :: MoveTask -> TaskId -> m TaskId

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
    getTasksWithLimit :: Int -> TaskParam -> m [TaskId]

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

-- | Create new TaskFilter with required query parameter
filterTaskBuilder :: Text -> Initial TaskFilter
filterTaskBuilder query =
    seed
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

-- | Create new CompletedTasksQueryParam with required since/until parameters
completedTasksQueryParamBuilder :: Text -> Text -> Initial CompletedTasksQueryParam
completedTasksQueryParamBuilder since until =
    seed
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

-- | Create empty TaskParam for use with builder pattern
taskParamBuilder :: Initial TaskParam
taskParamBuilder =
    seed
        TaskParam
            { project_id = Nothing
            , section_id = Nothing
            , parent_id = Nothing
            , task_ids = []
            , cursor = Nothing
            , limit = Nothing
            }

-- HasX instances for TaskParam
instance HasProjectId TaskParam where
    hasProjectId :: Text -> TaskParam -> TaskParam
    hasProjectId pid TaskParam {..} = TaskParam {project_id = Just pid, ..}

instance HasSectionId TaskParam where
    hasSectionId :: Text -> TaskParam -> TaskParam
    hasSectionId sid TaskParam {..} = TaskParam {section_id = Just sid, ..}

instance HasParentId TaskParam where
    hasParentId :: Text -> TaskParam -> TaskParam
    hasParentId pid TaskParam {..} = TaskParam {parent_id = Just pid, ..}

instance HasTaskIds TaskParam where
    hasTaskIds :: [Text] -> TaskParam -> TaskParam
    hasTaskIds tids TaskParam {..} = TaskParam {task_ids = tids, ..}

instance HasCursor TaskParam where
    hasCursor :: Text -> TaskParam -> TaskParam
    hasCursor c TaskParam {..} = TaskParam {cursor = Just c, ..}

instance HasLimit TaskParam where
    hasLimit :: Int -> TaskParam -> TaskParam
    hasLimit l TaskParam {..} = TaskParam {limit = Just l, ..}

-- HasX instances for TaskFilter
instance HasLang TaskFilter where
    hasLang :: Text -> TaskFilter -> TaskFilter
    hasLang lng TaskFilter {..} = TaskFilter {lang = Just lng, ..}

instance HasCursor TaskFilter where
    hasCursor :: Text -> TaskFilter -> TaskFilter
    hasCursor c TaskFilter {..} = TaskFilter {cursor = Just c, ..}

instance HasLimit TaskFilter where
    hasLimit :: Int -> TaskFilter -> TaskFilter
    hasLimit l TaskFilter {..} = TaskFilter {limit = Just l, ..}

-- HasX instances for CompletedTasksQueryParam
instance HasProjectId CompletedTasksQueryParam where
    hasProjectId :: Text -> CompletedTasksQueryParam -> CompletedTasksQueryParam
    hasProjectId pid CompletedTasksQueryParam {..} = CompletedTasksQueryParam {project_id = Just pid, ..}

instance HasSectionId CompletedTasksQueryParam where
    hasSectionId :: Text -> CompletedTasksQueryParam -> CompletedTasksQueryParam
    hasSectionId sid CompletedTasksQueryParam {..} = CompletedTasksQueryParam {section_id = Just sid, ..}

instance HasParentId CompletedTasksQueryParam where
    hasParentId :: Text -> CompletedTasksQueryParam -> CompletedTasksQueryParam
    hasParentId pid CompletedTasksQueryParam {..} = CompletedTasksQueryParam {parent_id = Just pid, ..}

instance HasFilterQuery CompletedTasksQueryParam where
    hasFilterQuery :: Text -> CompletedTasksQueryParam -> CompletedTasksQueryParam
    hasFilterQuery fq CompletedTasksQueryParam {..} = CompletedTasksQueryParam {filter_query = Just fq, ..}

instance HasFilterLang CompletedTasksQueryParam where
    hasFilterLang :: Text -> CompletedTasksQueryParam -> CompletedTasksQueryParam
    hasFilterLang fl CompletedTasksQueryParam {..} = CompletedTasksQueryParam {filter_lang = Just fl, ..}

instance HasCursor CompletedTasksQueryParam where
    hasCursor :: Text -> CompletedTasksQueryParam -> CompletedTasksQueryParam
    hasCursor c CompletedTasksQueryParam {..} = CompletedTasksQueryParam {cursor = Just c, ..}

instance HasLimit CompletedTasksQueryParam where
    hasLimit :: Int -> CompletedTasksQueryParam -> CompletedTasksQueryParam
    hasLimit l CompletedTasksQueryParam {..} = CompletedTasksQueryParam {limit = l, ..}
