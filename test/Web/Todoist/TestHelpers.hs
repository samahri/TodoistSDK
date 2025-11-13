{-# LANGUAGE DisambiguateRecordFields #-}

module Web.Todoist.TestHelpers
    ( -- Project-related exports
      sampleProjectResponse
    , sampleProjectResponseJson
    , sampleProjectId
    , sampleProjectIdJson
    , sampleProject
    , sampleProjects
    , sampleProjectsJson
    , sampleCollaborator
    , sampleCollaborators
    , sampleCollaboratorsJson
    , sampleProjectCreate
    , sampleProjectCreateJson
    , sampleProjectUpdate
    , sampleProjectUpdateJson
    , samplePartialProjectUpdate
    , samplePartialProjectUpdateJson
    , sampleAction
    , sampleRoleActions
    , sampleProjectPermissions
    , sampleProjectPermissionsJson
    -- Task-related exports
    , sampleDurationResponse
    , sampleDuration
    , sampleDeadlineResponse
    , sampleDeadline
    , sampleDueResponse
    , sampleDue
    , sampleTaskResponse
    , sampleTaskResponseJson
    , sampleTask
    , sampleTaskId
    , sampleNewTaskResponse
    , sampleNewTaskResponseJson
    , sampleNewTask
    , sampleTasksJson
    -- Comment-related exports
    , sampleCommentId
    , sampleCommentResponse
    , sampleCommentResponseJson
    , sampleComment
    , sampleCommentResponseWithAttachment
    , sampleCommentResponseWithAttachmentJson
    , sampleCommentCreate
    -- Section-related exports
    , sampleSectionId
    , sampleSectionResponse
    , sampleSectionResponseJson
    , sampleSection
    , sampleSectionCreate
    , sampleSectionUpdate
    , sampleSectionsJson
    -- Label-related exports
    , sampleLabelResponse
    , sampleLabel
    , sampleLabelResponseJson
    , sampleLabelsJson
    , sampleSharedLabelsJson
    ) where

import Web.Todoist.Builder (runBuilder, setDescription)
import Web.Todoist.Domain.Comment
    ( Comment (..)
    , CommentCreate
    , CommentId (..)
    , Content (..)
    , newComment
    )
import Web.Todoist.Domain.Label
    ( Label (..)
    , LabelId (..)
    )
import Web.Todoist.Domain.Project
    ( Collaborator (..)
    , Project (..)
    , ProjectCreate
    , ProjectUpdate (..)
    , newProject
    )
import Web.Todoist.Domain.Section
    ( Section (..)
    , SectionCreate
    , SectionId (..)
    , SectionUpdate (..)
    , newSection
    )
import Web.Todoist.Domain.Task
    ( Deadline (..)
    , Due (..)
    , Duration (..)
    , DurationUnit (..)
    , NewTask (..)
    , Task (..)
    )
import Web.Todoist.Domain.Types (ProjectId (..), TaskId (..), Uid (..), ViewStyle (..), Name (..), Color (..), IsFavorite (..), Order (..))
import Web.Todoist.Internal.Types
    ( Action (..)
    , CollaboratorRole (..)
    , CommentResponse (..)
    , CreatedAt (..)
    , CreatorUid (..)
    , DeadlineResponse (..)
    , DueResponse (..)
    , DurationResponse (..)
    , FileAttachment (..)
    , LabelResponse (..)
    , NewTaskResponse (..)
    , ParentId (..)
    , ProjectPermissions (..)
    , ProjectResponse (..)
    , Role (..)
    , RoleActions (..)
    , SectionResponse (..)
    , TaskResponse (..)
    , UpdatedAt (..)
    )

import Data.Bool (Bool (..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Function (($))
import Data.Maybe (Maybe (..))
import Data.Monoid (mempty)

-- | Sample ProjectId for testing
sampleProjectId :: ProjectId
sampleProjectId = ProjectId {getProjectId = "2203306141"}

-- | Sample ProjectResponse with all fields populated
sampleProjectResponse :: ProjectResponse
sampleProjectResponse =
    ProjectResponse
        { p_id = "2203306141"
        , p_can_assign_tasks = False
        , p_child_order = 1
        , p_color = "blue"
        , p_creator_uid = CreatorUid (Just "12345678")
        , p_created_at = CreatedAt (Just "2023-06-15T10:30:00Z")
        , p_is_archived = False
        , p_is_deleted = False
        , p_is_favorite = True
        , p_is_frozen = False
        , p_name = "Test Project"
        , p_updated_at = UpdatedAt (Just "2023-06-20T14:45:00Z")
        , p_view_style = "list"
        , p_default_order = 0
        , p_description = "A test project for unit testing"
        , p_public_key = "test-public-key"
        , p_access = Nothing
        , p_role = Role Nothing
        , p_parent_id = ParentId Nothing
        , p_inbox_project = False
        , p_is_collapsed = False
        , p_is_shared = False
        }

-- | Sample Project (domain model) corresponding to sampleProjectResponse
sampleProject :: Project
sampleProject =
    Project
        { _id = "2203306141"
        , _name = "Test Project"
        , _description = "A test project for unit testing"
        , _order = 1
        , _color = "blue"
        , _is_collapsed = False
        , _is_shared = False
        , _is_favorite = True
        , _is_archived = False
        , _can_assign_tasks = False
        , _view_style = List
        , _created_at = Just "2023-06-15T10:30:00Z"
        , _updated_at = Just "2023-06-20T14:45:00Z"
        }

-- | JSON representation of a valid ProjectResponse
sampleProjectResponseJson :: ByteString
sampleProjectResponseJson =
    BSL.pack
        "{\
        \\"id\":\"2203306141\",\
        \\"can_assign_tasks\":false,\
        \\"child_order\":1,\
        \\"color\":\"blue\",\
        \\"creator_uid\":{\"creator_uid\":\"12345678\"},\
        \\"created_at\":{\"created_at\":\"2023-06-15T10:30:00Z\"},\
        \\"is_archived\":false,\
        \\"is_deleted\":false,\
        \\"is_favorite\":true,\
        \\"is_frozen\":false,\
        \\"name\":\"Test Project\",\
        \\"updated_at\":{\"updated_at\":\"2023-06-20T14:45:00Z\"},\
        \\"view_style\":\"list\",\
        \\"default_order\":0,\
        \\"description\":\"A test project for unit testing\",\
        \\"public_key\":\"test-public-key\",\
        \\"access\":null,\
        \\"role\":{\"role\":null},\
        \\"parent_id\":{\"parent_id\":null},\
        \\"inbox_project\":false,\
        \\"is_collapsed\":false,\
        \\"is_shared\":false\
        \}"

-- | Sample ProjectId JSON response (fieldLabelModifier drops 1 char from _id -> id)
sampleProjectIdJson :: ByteString
sampleProjectIdJson = BSL.pack "{\"id\":\"2203306141\"}"

-- | Sample list of projects for getAllProjects
sampleProjects :: [Project]
sampleProjects = [sampleProject, sampleProject {_id = "2203306142", _name = "Second Project"}]

-- | JSON for TodoistReturn [ProjectResponse] (getAllProjects response)
sampleProjectsJson :: ByteString
sampleProjectsJson =
    BSL.pack
        "{\
        \\"results\":[\
        \{\"id\":\"2203306141\",\"can_assign_tasks\":false,\"child_order\":1,\"color\":\"blue\",\
        \\"creator_uid\":{\"creator_uid\":\"12345678\"},\"created_at\":\"2023-06-15T10:30:00Z\",\
        \\"is_archived\":false,\"is_deleted\":false,\"is_favorite\":true,\"is_frozen\":false,\
        \\"name\":\"Test Project\",\"updated_at\":\"2023-06-20T14:45:00Z\",\
        \\"view_style\":\"list\",\"default_order\":0,\"description\":\"A test project for unit testing\",\
        \\"public_key\":\"test-public-key\",\"access\":null,\"role\":{\"role\":null},\
        \\"parent_id\":{\"parent_id\":null},\"inbox_project\":false,\"is_collapsed\":false,\"is_shared\":false},\
        \{\"id\":\"2203306142\",\"can_assign_tasks\":false,\"child_order\":2,\"color\":\"blue\",\
        \\"creator_uid\":{\"creator_uid\":\"12345678\"},\"created_at\":\"2023-06-15T10:30:00Z\",\
        \\"is_archived\":false,\"is_deleted\":false,\"is_favorite\":true,\"is_frozen\":false,\
        \\"name\":\"Second Project\",\"updated_at\":\"2023-06-20T14:45:00Z\",\
        \\"view_style\":\"list\",\"default_order\":0,\"description\":\"A test project for unit testing\",\
        \\"public_key\":\"test-public-key\",\"access\":null,\"role\":{\"role\":null},\
        \\"parent_id\":{\"parent_id\":null},\"inbox_project\":false,\"is_collapsed\":false,\"is_shared\":false}\
        \],\
        \\"next_cursor\":null\
        \}"

-- | Sample Collaborator
sampleCollaborator :: Collaborator
sampleCollaborator =
    Collaborator
        { _id = "user123"
        , _name = "John Doe"
        , _email = "john@example.com"
        }

-- | Sample list of collaborators
sampleCollaborators :: [Collaborator]
sampleCollaborators =
    [ sampleCollaborator
    , sampleCollaborator {_id = "user456", _name = "Jane Smith", _email = "jane@example.com"}
    ]

-- | JSON for TodoistReturn [Collaborator]
sampleCollaboratorsJson :: ByteString
sampleCollaboratorsJson =
    BSL.pack
        "{\
        \\"results\":[\
        \{\"id\":\"user123\",\"name\":\"John Doe\",\"email\":\"john@example.com\"},\
        \{\"id\":\"user456\",\"name\":\"Jane Smith\",\"email\":\"jane@example.com\"}\
        \],\
        \\"next_cursor\":null\
        \}"

-- | Sample ProjectCreate
sampleProjectCreate :: ProjectCreate
sampleProjectCreate = runBuilder (newProject "New Project") (setDescription "A new project to be created")

-- | JSON representation of ProjectCreate (for serialization test)
sampleProjectCreateJson :: ByteString
sampleProjectCreateJson =
    BSL.pack
        "{\
        \\"name\":\"New Project\",\
        \\"description\":\"A new project to be created\"\
        \}"

-- | Sample ProjectUpdate for testing
sampleProjectUpdate :: ProjectUpdate
sampleProjectUpdate =
    ProjectUpdate
        { _name = Just "Updated Project Name"
        , _description = Just "Updated description"
        , _color = Just "red"
        , _is_favorite = Just True
        , _view_style = Just List
        }

-- | JSON representation of ProjectUpdate
sampleProjectUpdateJson :: ByteString
sampleProjectUpdateJson =
    BSL.pack
        "{\
        \\"name\":\"Updated Project Name\",\
        \\"description\":\"Updated description\",\
        \\"color\":\"red\",\
        \\"is_favorite\":true,\
        \\"view_style\":\"list\"\
        \}"

-- | Sample ProjectUpdate with only some fields (partial update)
samplePartialProjectUpdate :: ProjectUpdate
samplePartialProjectUpdate =
    ProjectUpdate
        { _name = Just "New Name"
        , _description = Nothing
        , _color = Nothing
        , _is_favorite = Just True
        , _view_style = Nothing
        }

-- | JSON representation of partial ProjectUpdate
samplePartialProjectUpdateJson :: ByteString
samplePartialProjectUpdateJson =
    BSL.pack
        "{\
        \\"name\":\"New Name\",\
        \\"is_favorite\":true\
        \}"

-- | Sample Action for permissions testing
sampleAction :: Action
sampleAction = Action {p_name = "create_task"}

-- | Sample RoleActions for permissions testing
sampleRoleActions :: RoleActions
sampleRoleActions =
    RoleActions
        { p_name = Creator
        , p_actions = [sampleAction, Action {p_name = "delete_project"}]
        }

-- | Sample ProjectPermissions for testing
sampleProjectPermissions :: ProjectPermissions
sampleProjectPermissions =
    ProjectPermissions
        { p_project_collaborator_actions = [sampleRoleActions]
        , p_workspace_collaborator_actions = [sampleRoleActions]
        }

-- | JSON representation of ProjectPermissions
sampleProjectPermissionsJson :: ByteString
sampleProjectPermissionsJson =
    BSL.pack
        "{\
        \\"project_collaborator_actions\":[{\
        \\"name\":\"CREATOR\",\
        \\"actions\":[{\"name\":\"create_task\"},{\"name\":\"delete_project\"}]\
        \}],\
        \\"workspace_collaborator_actions\":[{\
        \\"name\":\"CREATOR\",\
        \\"actions\":[{\"name\":\"create_task\"},{\"name\":\"delete_project\"}]\
        \}]\
        \}"

-- ============================================================================
-- Task-related test data
-- ============================================================================

-- | Sample DurationResponse for testing
sampleDurationResponse :: DurationResponse
sampleDurationResponse =
    DurationResponse
        { p_amount = 30
        , p_unit = "minute"
        }

-- | Sample Duration (domain model)
sampleDuration :: Duration
sampleDuration =
    Duration
        { _amount = 30
        , _unit = Minute
        }

-- | Sample DeadlineResponse for testing
sampleDeadlineResponse :: DeadlineResponse
sampleDeadlineResponse =
    DeadlineResponse
        { p_date = "2025-12-31"
        , p_lang = "en"
        }

-- | Sample Deadline (domain model)
sampleDeadline :: Deadline
sampleDeadline =
    Deadline
        { _date = "2025-12-31"
        , _lang = "en"
        }

-- | Sample DueResponse for testing
sampleDueResponse :: DueResponse
sampleDueResponse =
    DueResponse
        { p_date = "2025-11-15"
        , p_string = "Nov 15"
        , p_lang = "en"
        , p_is_recurring = False
        , p_timezone = Just "America/New_York"
        }

-- | Sample Due (domain model)
sampleDue :: Due
sampleDue =
    Due
        { _date = "2025-11-15"
        , _string = "Nov 15"
        , _lang = "en"
        , _is_recurring = False
        , _timezone = Just "America/New_York"
        }

-- | Sample TaskId for testing
sampleTaskId :: TaskId
sampleTaskId = TaskId {getTaskId = "7654321098"}

-- | Sample TaskResponse with all fields populated
sampleTaskResponse :: TaskResponse
sampleTaskResponse =
    TaskResponse
        { p_user_id = "56092663"
        , p_id = "7654321098"
        , p_project_id = "2203306141"
        , p_section_id = Just "section123"
        , p_parent_id = Just "parent456"
        , p_added_by_uid = Just "56092663"
        , p_assigned_by_uid = Just "56092663"
        , p_responsible_uid = Just "assignee789"
        , p_labels = ["urgent", "work"]
        , p_deadline = Just sampleDeadlineResponse
        , p_duration = Just sampleDurationResponse
        , p_checked = False
        , p_is_deleted = False
        , p_added_at = Just "2025-11-01T10:00:00Z"
        , p_completed_at = Nothing
        , p_updated_at = Just "2025-11-03T14:30:00Z"
        , p_due = Just sampleDueResponse
        , p_priority = 3
        , p_child_order = 1
        , p_content = "Test Task Content"
        , p_description = "This is a test task description"
        , p_note_count = 2
        , p_day_order = 5
        , p_is_collapsed = False
        }

-- | Sample Task (domain model) corresponding to sampleTaskResponse
sampleTask :: Task
sampleTask =
    Task
        { _id = "7654321098"
        , _content = "Test Task Content"
        , _description = "This is a test task description"
        , _project_id = "2203306141"
        , _section_id = Just "section123"
        , _parent_id = Just "parent456"
        , _labels = ["urgent", "work"]
        , _priority = 3
        , _due = Just sampleDue
        , _deadline = Just sampleDeadline
        , _duration = Just sampleDuration
        , _is_collapsed = False
        , _order = 1
        , _assignee_id = Just "assignee789"
        , _assigner_id = Just "56092663"
        , _completed_at = Nothing
        , _creator_id = "56092663"
        , _created_at = "2025-11-01T10:00:00Z"
        , _updated_at = "2025-11-03T14:30:00Z"
        }

-- | JSON representation of a valid TaskResponse
sampleTaskResponseJson :: ByteString
sampleTaskResponseJson =
    BSL.pack
        "{\
        \\"user_id\":\"56092663\",\
        \\"id\":\"7654321098\",\
        \\"project_id\":\"2203306141\",\
        \\"section_id\":\"section123\",\
        \\"parent_id\":\"parent456\",\
        \\"added_by_uid\":\"56092663\",\
        \\"assigned_by_uid\":\"56092663\",\
        \\"responsible_uid\":\"assignee789\",\
        \\"labels\":[\"urgent\",\"work\"],\
        \\"deadline\":{\"date\":\"2025-12-31\",\"lang\":\"en\"},\
        \\"duration\":{\"amount\":30,\"unit\":\"minute\"},\
        \\"checked\":false,\
        \\"is_deleted\":false,\
        \\"added_at\":\"2025-11-01T10:00:00Z\",\
        \\"completed_at\":null,\
        \\"updated_at\":\"2025-11-03T14:30:00Z\",\
        \\"due\":{\"date\":\"2025-11-15\",\"string\":\"Nov 15\",\"lang\":\"en\",\"is_recurring\":false,\"timezone\":\"America/New_York\"},\
        \\"priority\":3,\
        \\"child_order\":1,\
        \\"content\":\"Test Task Content\",\
        \\"description\":\"This is a test task description\",\
        \\"note_count\":2,\
        \\"day_order\":5,\
        \\"is_collapsed\":false\
        \}"

-- | Sample NewTaskResponse for testing
sampleNewTaskResponse :: NewTaskResponse
sampleNewTaskResponse =
    NewTaskResponse
        { p_user_id = "56092663"
        , p_id = "9876543210"
        , p_project_id = "2203306141"
        , p_section_id = Nothing
        , p_parent_id = Nothing
        , p_added_by_uid = Just "56092663"
        , p_assigned_by_uid = Nothing
        , p_responsible_uid = Nothing
        , p_labels = ["new"]
        , p_checked = False
        , p_is_deleted = False
        , p_added_at = Just "2025-11-04T09:00:00Z"
        , p_completed_at = Nothing
        , p_updated_at = Just "2025-11-04T09:00:00Z"
        , p_priority = 1
        , p_child_order = 0
        , p_content = "New Task"
        , p_description = "A newly created task"
        , p_note_count = 0
        , p_day_order = 1
        , p_is_collapsed = False
        }

-- | Sample NewTask (domain model) corresponding to sampleNewTaskResponse
sampleNewTask :: NewTask
sampleNewTask =
    NewTask
        { _user_id = "56092663"
        , _id = "9876543210"
        , _project_id = "2203306141"
        , _section_id = Nothing
        , _parent_id = Nothing
        , _added_by_uid = Just "56092663"
        , _assigned_by_uid = Nothing
        , _responsible_uid = Nothing
        , _labels = ["new"]
        , _checked = False
        , _is_deleted = False
        , _added_at = Just "2025-11-04T09:00:00Z"
        , _completed_at = Nothing
        , _updated_at = Just "2025-11-04T09:00:00Z"
        , _priority = 1
        , _child_order = 0
        , _content = "New Task"
        , _description = "A newly created task"
        , _note_count = 0
        , _day_order = 1
        , _is_collapsed = False
        }

-- | JSON representation of a valid NewTaskResponse
sampleNewTaskResponseJson :: ByteString
sampleNewTaskResponseJson =
    BSL.pack
        "{\
        \\"user_id\":\"56092663\",\
        \\"id\":\"9876543210\",\
        \\"project_id\":\"2203306141\",\
        \\"section_id\":null,\
        \\"parent_id\":null,\
        \\"added_by_uid\":\"56092663\",\
        \\"assigned_by_uid\":null,\
        \\"responsible_uid\":null,\
        \\"labels\":[\"new\"],\
        \\"checked\":false,\
        \\"is_deleted\":false,\
        \\"added_at\":\"2025-11-04T09:00:00Z\",\
        \\"completed_at\":null,\
        \\"updated_at\":\"2025-11-04T09:00:00Z\",\
        \\"priority\":1,\
        \\"child_order\":0,\
        \\"content\":\"New Task\",\
        \\"description\":\"A newly created task\",\
        \\"note_count\":0,\
        \\"day_order\":1,\
        \\"is_collapsed\":false\
        \}"

-- | JSON for TodoistReturn [TaskResponse] (getTasks response)
sampleTasksJson :: ByteString
sampleTasksJson =
    BSL.pack
        "{\
        \\"results\":[\
        \{\"user_id\":\"56092663\",\"id\":\"7654321098\",\"project_id\":\"2203306141\",\
        \\"section_id\":\"section123\",\"parent_id\":\"parent456\",\"added_by_uid\":\"56092663\",\
        \\"assigned_by_uid\":\"56092663\",\"responsible_uid\":\"assignee789\",\"labels\":[\"urgent\",\"work\"],\
        \\"deadline\":{\"date\":\"2025-12-31\",\"lang\":\"en\"},\"duration\":{\"amount\":30,\"unit\":\"minute\"},\
        \\"checked\":false,\"is_deleted\":false,\"added_at\":\"2025-11-01T10:00:00Z\",\"completed_at\":null,\
        \\"updated_at\":\"2025-11-03T14:30:00Z\",\"due\":{\"date\":\"2025-11-15\",\"string\":\"Nov 15\",\"lang\":\"en\",\"is_recurring\":false,\"timezone\":\"America/New_York\"},\
        \\"priority\":3,\"child_order\":1,\"content\":\"Test Task Content\",\"description\":\"This is a test task description\",\
        \\"note_count\":2,\"day_order\":5,\"is_collapsed\":false}\
        \],\
        \\"next_cursor\":null\
        \}"

-- ===== Comment Fixtures =====

-- | Sample Comment ID
sampleCommentId :: CommentId
sampleCommentId = CommentId {getCommentId = "3012345678"}

-- | Sample CommentResponse
sampleCommentResponse :: CommentResponse
sampleCommentResponse =
    CommentResponse
        { p_id = "3012345678"
        , p_content = "This is a test comment"
        , p_posted_uid = Just "2671355"
        , p_posted_at = Just "2023-10-15T14:30:00Z"
        , p_item_id = Nothing
        , p_project_id = Just "2203306141"
        , p_file_attachment = Nothing
        , p_uids_to_notify = Just ["2671355"]
        , p_is_deleted = False
        , p_reactions = Nothing
        }

-- | Sample CommentResponse JSON
sampleCommentResponseJson :: ByteString
sampleCommentResponseJson =
    BSL.pack
        "{\
        \\"id\":\"3012345678\",\
        \\"content\":\"This is a test comment\",\
        \\"posted_uid\":\"2671355\",\
        \\"posted_at\":\"2023-10-15T14:30:00Z\",\
        \\"item_id\":null,\
        \\"project_id\":\"2203306141\",\
        \\"file_attachment\":null,\
        \\"uids_to_notify\":[\"2671355\"],\
        \\"is_deleted\":false,\
        \\"reactions\":null\
        \}"

-- | Sample Comment
sampleComment :: Comment
sampleComment =
    Comment
        { _id = CommentId "3012345678"
        , _content = Content "This is a test comment"
        , _poster_id = Just (Uid "2671355")
        , _posted_at = Just (Uid "2023-10-15T14:30:00Z")
        , _task_id = Nothing
        , _project_id = Just (ProjectId "2203306141")
        , _attachment = Nothing
        }

-- | Sample CommentResponse with attachment
sampleCommentResponseWithAttachment :: CommentResponse
sampleCommentResponseWithAttachment =
    CommentResponse
        { p_id = "3012345679"
        , p_content = "Comment with attachment"
        , p_posted_uid = Just "2671355"
        , p_posted_at = Just "2023-10-15T15:00:00Z"
        , p_item_id = Just "2995104339"
        , p_project_id = Nothing
        , p_file_attachment =
            Just $
                FileAttachment
                    { p_file_name = "document.pdf"
                    , p_file_type = "application/pdf"
                    , p_file_url = "https://example.com/document.pdf"
                    , p_resource_type = "file"
                    }
        , p_uids_to_notify = Nothing
        , p_is_deleted = False
        , p_reactions = Nothing
        }

-- | Sample CommentResponse with attachment JSON
sampleCommentResponseWithAttachmentJson :: ByteString
sampleCommentResponseWithAttachmentJson =
    BSL.pack
        "{\
        \\"id\":\"3012345679\",\
        \\"content\":\"Comment with attachment\",\
        \\"posted_uid\":\"2671355\",\
        \\"posted_at\":\"2023-10-15T15:00:00Z\",\
        \\"item_id\":\"2995104339\",\
        \\"project_id\":null,\
        \\"file_attachment\":{\
        \\"file_name\":\"document.pdf\",\
        \\"file_type\":\"application/pdf\",\
        \\"file_url\":\"https://example.com/document.pdf\",\
        \\"resource_type\":\"file\"\
        \},\
        \\"uids_to_notify\":null,\
        \\"is_deleted\":false,\
        \\"reactions\":null\
        \}"

-- | Sample CommentCreate
sampleCommentCreate :: CommentCreate
sampleCommentCreate =
    runBuilder
        (newComment "New comment")
        mempty

-- ===== Section Fixtures =====

-- | Sample Section ID
sampleSectionId :: SectionId
sampleSectionId = SectionId {_id = "section123"}

-- | Sample SectionResponse
sampleSectionResponse :: SectionResponse
sampleSectionResponse =
    SectionResponse
        { p_id = "section123"
        , p_user_id = "user456"
        , p_project_id = "project789"
        , p_added_at = "2024-01-01T12:00:00Z"
        , p_updated_at = Just "2024-01-02T14:30:00Z"
        , p_archived_at = Nothing
        , p_name = "Test Section"
        , p_section_order = 1
        , p_is_archived = False
        , p_is_deleted = False
        , p_is_collapsed = False
        }

-- | Sample SectionResponse JSON
sampleSectionResponseJson :: ByteString
sampleSectionResponseJson =
    BSL.pack
        "{\
        \\"id\":\"section123\",\
        \\"user_id\":\"user456\",\
        \\"project_id\":\"project789\",\
        \\"added_at\":\"2024-01-01T12:00:00Z\",\
        \\"updated_at\":\"2024-01-02T14:30:00Z\",\
        \\"archived_at\":null,\
        \\"name\":\"Test Section\",\
        \\"section_order\":1,\
        \\"is_archived\":false,\
        \\"is_deleted\":false,\
        \\"is_collapsed\":false\
        \}"

-- | Sample Section (domain model)
sampleSection :: Section
sampleSection =
    Section
        { _id = "section123"
        , _name = "Test Section"
        , _project_id = "project789"
        , _is_collapsed = False
        , _order = 1
        }

-- | Sample SectionCreate
sampleSectionCreate :: SectionCreate
sampleSectionCreate = runBuilder (newSection "New Section" "project789") mempty

-- | Sample SectionUpdate
sampleSectionUpdate :: SectionUpdate
sampleSectionUpdate =
    SectionUpdate
        { _name = Just "Updated Section"
        }

-- | JSON for TodoistReturn [SectionResponse] (getSections response)
sampleSectionsJson :: ByteString
sampleSectionsJson =
    BSL.pack
        "{\
        \\"results\":[{\
        \\"id\":\"section123\",\
        \\"user_id\":\"user456\",\
        \\"project_id\":\"project789\",\
        \\"added_at\":\"2024-01-01T12:00:00Z\",\
        \\"updated_at\":\"2024-01-02T14:30:00Z\",\
        \\"archived_at\":null,\
        \\"name\":\"Test Section\",\
        \\"section_order\":1,\
        \\"is_archived\":false,\
        \\"is_deleted\":false,\
        \\"is_collapsed\":false\
        \}],\
        \\"next_cursor\":null\
        \}"

-- ==================== Label-related Helpers ====================

-- | Sample LabelResponse for testing
sampleLabelResponse :: LabelResponse
sampleLabelResponse =
    LabelResponse
        { p_id = "label123"
        , p_name = "Test Label"
        , p_color = "charcoal"
        , p_order = Just 1
        , p_is_favorite = False
        }

-- | Sample Label domain type for testing
sampleLabel :: Label
sampleLabel =
    Label
        { _id = LabelId "label123"
        , _name = Name "Test Label"
        , _color = Color "charcoal"
        , _order = Just (Order 1)
        , _is_favorite = IsFavorite False
        }

-- | JSON for single LabelResponse
sampleLabelResponseJson :: ByteString
sampleLabelResponseJson =
    "{\"id\":\"label123\",\"name\":\"Test Label\",\"color\":\"charcoal\",\"order\":1,\"is_favorite\":false}"

-- | JSON for paginated labels response
sampleLabelsJson :: ByteString
sampleLabelsJson =
    "{\"results\":[{\"id\":\"label123\",\"name\":\"Test Label\",\"color\":\"charcoal\",\"order\":1,\"is_favorite\":false}],\"next_cursor\":null}"

-- | JSON for shared labels response
sampleSharedLabelsJson :: ByteString
sampleSharedLabelsJson =
    "{\"results\":[\"Label1\",\"Label2\",\"Label3\"],\"next_cursor\":null}"
