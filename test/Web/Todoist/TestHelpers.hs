{-# LANGUAGE DisambiguateRecordFields #-}

module Web.Todoist.TestHelpers
    ( sampleProjectResponse
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
    ) where

import Web.Todoist.Builder.Has (HasDescription (hasDescription))
import Web.Todoist.Domain.Project
    ( Collaborator (..)
    , Project (..)
    , ProjectCreate
    , ProjectId (..)
    , newProject
    )
import Web.Todoist.Domain.Types (ViewStyle (..))
import Web.Todoist.Internal.Types
    ( CreatedAt (..)
    , CreatorUid (..)
    , ParentId (..)
    , ProjectResponse (..)
    , Role (..)
    , UpdatedAt (..)
    )

import Data.Bool (Bool (..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Function (($))
import Data.Maybe (Maybe (..))

-- | Sample ProjectId for testing
sampleProjectId :: ProjectId
sampleProjectId = ProjectId {_id = "2203306141"}

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
sampleProjectCreate = hasDescription "A new project to be created" $ newProject "New Project"

-- | JSON representation of ProjectCreate (for serialization test)
sampleProjectCreateJson :: ByteString
sampleProjectCreateJson =
    BSL.pack
        "{\
        \\"name\":\"New Project\",\
        \\"description\":\"A new project to be created\"\
        \}"
