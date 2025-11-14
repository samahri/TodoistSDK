{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : Web.Todoist.Internal.Types
Description : Internal HTTP response types for Todoist REST API
Copyright   : (c) 2025 Sam S. Almahri
License     : MIT
Maintainer  : sam.salmahri@gmail.com

Internal module defining HTTP response types that mirror the Todoist REST API
JSON responses. These types use the @p_@ field prefix convention and are
converted to domain types for public use.

This module is part of the internal HTTP layer. End users should use the
domain types from "Web.Todoist.Domain.*" modules instead.

= Field Naming Convention

HTTP response types use @p_@ prefix (e.g., @p_id@, @p_name@) which is dropped
by 'jsonOpts' during serialization. This allows clean JSON field names while
avoiding conflicts with domain types that use @_@ prefix.
-}
module Web.Todoist.Internal.Types
    ( TodoistReturn (..)
    , ProjectResponse (..)
    , CreatedAt (..)
    , UpdatedAt (..)
    , CreatorUid (..)
    , Role (..)
    , ParentId (..)
    , ProjectAccessView (..)
    , ProjectVisibility (..)
    , CollaboratorRole (..)
    , Action (..)
    , RoleActions (..)
    , ProjectPermissions (..)
    , TaskResponse (..)
    , DurationResponse (..)
    , DeadlineResponse (..)
    , DueResponse (..)
    , NewTaskResponse (..)
    , CommentResponse (..)
    , SectionResponse (..)
    , LabelResponse (..)
    , FileAttachment (..)
    , Reactions (..)
    , Params
    , Endpoint
    ) where

import Web.Todoist.Internal.Json (jsonOpts)

import Control.Applicative (pure)
import Control.Monad.Fail (fail)
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON (toJSON)
    , Value (String)
    , genericParseJSON
    , genericToJSON
    , withText
    )
import Data.Aeson.Types (Parser)
import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Show (Show)

-- | Wrapper type for paginated API responses
data TodoistReturn a = TodoistReturn
    { results :: [a]
    , next_cursor :: Maybe String
    }
    deriving (Show, Generic, ToJSON, FromJSON)

-- | Type alias for query parameters (key-value pairs)
type Params = [(Text, Text)]

-- | Type alias for API endpoint path segments
type Endpoint = [Text]

{- | HTTP API response type for GET project endpoint
This represents the complete JSON response returned by the Todoist REST API
when retrieving a project. Field names use p_ prefix which is dropped by jsonOpts.
-}
data ProjectResponse = ProjectResponse
    { p_id :: Text
    , p_can_assign_tasks :: Bool
    , p_child_order :: Int
    , p_color :: Text
    , p_creator_uid :: CreatorUid
    , p_created_at :: CreatedAt
    , p_is_archived :: Bool
    , p_is_deleted :: Bool
    , p_is_favorite :: Bool
    , p_is_frozen :: Bool
    , p_name :: Text
    , p_updated_at :: UpdatedAt -- TODO: use Maybe Text
    , p_view_style :: Text
    , p_default_order :: Int
    , p_description :: Text
    , p_public_key :: Text
    , p_access :: Maybe ProjectAccessView
    , p_role :: Role
    , p_parent_id :: ParentId
    , p_inbox_project :: Bool
    , p_is_collapsed :: Bool
    , p_is_shared :: Bool
    }
    deriving (Show, Generic)

instance FromJSON ProjectResponse where
    parseJSON :: Value -> Parser ProjectResponse
    parseJSON = genericParseJSON jsonOpts

instance ToJSON ProjectResponse where
    toJSON :: ProjectResponse -> Value
    toJSON = genericToJSON jsonOpts

newtype CreatorUid = CreatorUid
    { p_creator_uid :: Maybe Text
    }
    deriving (Show, Generic)

instance FromJSON CreatorUid where
    parseJSON :: Value -> Parser CreatorUid
    parseJSON = \case
        String txt -> pure $ CreatorUid (Just txt)
        _ -> pure $ CreatorUid Nothing

instance ToJSON CreatorUid where
    toJSON :: CreatorUid -> Value
    toJSON (CreatorUid Nothing) = String ""
    toJSON (CreatorUid (Just txt)) = String txt

newtype CreatedAt = CreatedAt
    { p_created_at :: Maybe Text
    }
    deriving (Show, Generic)

instance FromJSON CreatedAt where
    parseJSON :: Value -> Parser CreatedAt
    parseJSON = \case
        String txt -> pure $ CreatedAt (Just txt)
        _ -> pure $ CreatedAt Nothing

instance ToJSON CreatedAt where
    toJSON :: CreatedAt -> Value
    toJSON (CreatedAt Nothing) = String ""
    toJSON (CreatedAt (Just txt)) = String txt

newtype UpdatedAt = UpdatedAt
    { p_updated_at :: Maybe Text
    }
    deriving (Show, Generic, Eq)

instance FromJSON UpdatedAt where
    parseJSON :: Value -> Parser UpdatedAt
    parseJSON = \case
        String txt -> pure $ UpdatedAt (Just txt)
        _ -> pure $ UpdatedAt Nothing

instance ToJSON UpdatedAt where
    toJSON :: UpdatedAt -> Value
    toJSON (UpdatedAt Nothing) = String ""
    toJSON (UpdatedAt (Just txt)) = String txt

newtype Role = Role
    { p_role :: Maybe Text
    }
    deriving (Show, Generic)

instance FromJSON Role where
    parseJSON :: Value -> Parser Role
    parseJSON = \case
        String txt -> pure $ Role (Just txt)
        _ -> pure $ Role Nothing

instance ToJSON Role where
    toJSON :: Role -> Value
    toJSON (Role Nothing) = String ""
    toJSON (Role (Just txt)) = String txt

newtype ParentId = ParentId
    { p_parent_id :: Maybe Text
    }
    deriving (Show, Generic)

instance FromJSON ParentId where
    parseJSON :: Value -> Parser ParentId
    parseJSON = \case
        String txt -> pure $ ParentId (Just txt)
        _ -> pure $ ParentId Nothing

instance ToJSON ParentId where
    toJSON :: ParentId -> Value
    toJSON (ParentId Nothing) = String ""
    toJSON (ParentId (Just txt)) = String txt

data ProjectAccessView = ProjectAccessView
    { p_visibility :: ProjectVisibility
    , p_configuration :: Value
    }
    deriving (Show, Generic)

instance FromJSON ProjectAccessView where
    parseJSON :: Value -> Parser ProjectAccessView
    parseJSON = genericParseJSON jsonOpts

instance ToJSON ProjectAccessView where
    toJSON :: ProjectAccessView -> Value
    toJSON = genericToJSON jsonOpts

data ProjectVisibility = Restricted | Team | Public deriving (Show, Generic)

instance FromJSON ProjectVisibility where
    parseJSON :: Value -> Parser ProjectVisibility
    parseJSON = withText "ProjectVisibility" $ \case
        "restricted" -> pure Restricted
        "team" -> pure Team
        "public" -> pure Public
        _ -> fail "Unknown ProjectVisibility value"

instance ToJSON ProjectVisibility where
    toJSON :: ProjectVisibility -> Value
    toJSON Restricted = String "restricted"
    toJSON Team = String "team"
    toJSON Public = String "public"

{- | Collaborator role enum. Represents different permission levels
for project collaborators.
-}
data CollaboratorRole = Creator | Admin | Member | ReadWrite | ReadOnly
    deriving (Show, Eq, Generic)

instance FromJSON CollaboratorRole where
    parseJSON :: Value -> Parser CollaboratorRole
    parseJSON = withText "CollaboratorRole" $ \case
        "CREATOR" -> pure Creator
        "ADMIN" -> pure Admin
        "MEMBER" -> pure Member
        "READ_WRITE" -> pure ReadWrite
        "READ_ONLY" -> pure ReadOnly
        other -> fail $ "Unknown collaborator role: " <> T.unpack other

instance ToJSON CollaboratorRole where
    toJSON :: CollaboratorRole -> Value
    toJSON Creator = String "CREATOR"
    toJSON Admin = String "ADMIN"
    toJSON Member = String "MEMBER"
    toJSON ReadWrite = String "READ_WRITE"
    toJSON ReadOnly = String "READ_ONLY"

{- | An action that can be performed by a collaborator role.
Action names are kept as Text for maximum flexibility as the API
may add new action types without warning.
-}
newtype Action = Action
    { p_name :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON Action where
    parseJSON :: Value -> Parser Action
    parseJSON = genericParseJSON jsonOpts

instance ToJSON Action where
    toJSON :: Action -> Value
    toJSON = genericToJSON jsonOpts

-- | A role with its associated list of allowed actions.
data RoleActions = RoleActions
    { p_name :: CollaboratorRole
    -- ^ The collaborator role (e.g., Creator)
    , p_actions :: [Action]
    -- ^ Actions this role can perform
    }
    deriving (Show, Eq, Generic)

instance FromJSON RoleActions where
    parseJSON :: Value -> Parser RoleActions
    parseJSON = genericParseJSON jsonOpts

instance ToJSON RoleActions where
    toJSON :: RoleActions -> Value
    toJSON = genericToJSON jsonOpts

{- | Top-level permissions response from GET /api/v1/projects/permissions.
Contains role-action mappings for both project and workspace collaborators.
-}
data ProjectPermissions = ProjectPermissions
    { p_project_collaborator_actions :: [RoleActions]
    , p_workspace_collaborator_actions :: [RoleActions]
    }
    deriving (Show, Eq, Generic)

instance FromJSON ProjectPermissions where
    parseJSON :: Value -> Parser ProjectPermissions
    parseJSON = genericParseJSON jsonOpts

instance ToJSON ProjectPermissions where
    toJSON :: ProjectPermissions -> Value
    toJSON = genericToJSON jsonOpts

-- | Duration information for a task (API response version)
data DurationResponse = DurationResponse
    { p_amount :: Int
    , p_unit :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON DurationResponse where
    parseJSON :: Value -> Parser DurationResponse
    parseJSON = genericParseJSON jsonOpts

instance ToJSON DurationResponse where
    toJSON :: DurationResponse -> Value
    toJSON = genericToJSON jsonOpts

-- | Deadline information for a task (API response version)
data DeadlineResponse = DeadlineResponse
    { p_date :: Text
    , p_lang :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON DeadlineResponse where
    parseJSON :: Value -> Parser DeadlineResponse
    parseJSON = genericParseJSON jsonOpts

instance ToJSON DeadlineResponse where
    toJSON :: DeadlineResponse -> Value
    toJSON = genericToJSON jsonOpts

-- | Due date information for a task (API response version)
data DueResponse = DueResponse
    { p_date :: Text
    , p_string :: Text
    , p_lang :: Text
    , p_is_recurring :: Bool
    , p_timezone :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON DueResponse where
    parseJSON :: Value -> Parser DueResponse
    parseJSON = genericParseJSON jsonOpts

instance ToJSON DueResponse where
    toJSON :: DueResponse -> Value
    toJSON = genericToJSON jsonOpts

{- | HTTP API response type for task endpoints
This represents the complete JSON response returned by the Todoist REST API
when retrieving or manipulating tasks. Field names use p_ prefix which is dropped by jsonOpts.
-}
data TaskResponse = TaskResponse
    { p_user_id :: Text
    , p_id :: Text
    , p_project_id :: Text
    , p_section_id :: Maybe Text
    , p_parent_id :: Maybe Text
    , p_added_by_uid :: Maybe Text
    , p_assigned_by_uid :: Maybe Text
    , p_responsible_uid :: Maybe Text
    , p_labels :: [Text]
    , p_deadline :: Maybe DeadlineResponse
    , p_duration :: Maybe DurationResponse
    , p_checked :: Bool
    , p_is_deleted :: Bool
    , p_added_at :: Maybe Text
    , p_completed_at :: Maybe Text
    , p_updated_at :: Maybe Text
    , p_due :: Maybe DueResponse
    , p_priority :: Int
    , p_child_order :: Int
    , p_content :: Text
    , p_description :: Text
    , p_note_count :: Int
    , p_day_order :: Int
    , p_is_collapsed :: Bool
    }
    deriving (Show, Eq, Generic)

instance FromJSON TaskResponse where
    parseJSON :: Value -> Parser TaskResponse
    parseJSON = genericParseJSON jsonOpts

instance ToJSON TaskResponse where
    toJSON :: TaskResponse -> Value
    toJSON = genericToJSON jsonOpts

data NewTaskResponse = NewTaskResponse
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

instance FromJSON NewTaskResponse where
    parseJSON :: Value -> Parser NewTaskResponse
    parseJSON = genericParseJSON jsonOpts

instance ToJSON NewTaskResponse where
    toJSON :: NewTaskResponse -> Value
    toJSON = genericToJSON jsonOpts

-- | File attachment in comment response
data FileAttachment = FileAttachment
    { p_file_name :: Text
    , p_file_type :: Text
    , p_file_url :: Text
    , p_resource_type :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON FileAttachment where
    parseJSON :: Value -> Parser FileAttachment
    parseJSON = genericParseJSON jsonOpts

instance ToJSON FileAttachment where
    toJSON :: FileAttachment -> Value
    toJSON = genericToJSON jsonOpts

-- | Reactions data (opaque object for now)
newtype Reactions = Reactions {p_reactions :: Value}
    deriving (Show, Eq, Generic)

instance FromJSON Reactions where
    parseJSON :: Value -> Parser Reactions
    parseJSON v = pure $ Reactions {p_reactions = v}

instance ToJSON Reactions where
    toJSON :: Reactions -> Value
    toJSON (Reactions r) = r

-- | HTTP response type for Comment API (contains all fields from API)
data CommentResponse = CommentResponse
    { p_id :: Text
    , p_content :: Text
    , p_posted_uid :: Maybe Text
    , p_posted_at :: Maybe Text
    , p_item_id :: Maybe Text -- Maps to task_id in domain
    , p_project_id :: Maybe Text
    , p_file_attachment :: Maybe FileAttachment -- Maps to attachment in domain
    , p_uids_to_notify :: Maybe [Text]
    , p_is_deleted :: Bool
    , p_reactions :: Maybe Reactions
    }
    deriving (Show, Generic)

instance FromJSON CommentResponse where
    parseJSON :: Value -> Parser CommentResponse
    parseJSON = genericParseJSON jsonOpts

instance ToJSON CommentResponse where
    toJSON :: CommentResponse -> Value
    toJSON = genericToJSON jsonOpts

{- | HTTP response type for Section endpoints
Uses p_ prefix which gets dropped by jsonOpts (drops 2 chars)
-}
data SectionResponse = SectionResponse
    { p_id :: Text
    , p_user_id :: Text
    , p_project_id :: Text
    , p_added_at :: Text
    , p_updated_at :: Maybe Text
    , p_archived_at :: Maybe Text
    , p_name :: Text
    , p_section_order :: Int
    , p_is_archived :: Bool
    , p_is_deleted :: Bool
    , p_is_collapsed :: Bool
    }
    deriving (Show, Generic)

instance FromJSON SectionResponse where
    parseJSON :: Value -> Parser SectionResponse
    parseJSON = genericParseJSON jsonOpts

instance ToJSON SectionResponse where
    toJSON :: SectionResponse -> Value
    toJSON = genericToJSON jsonOpts

-- | HTTP API response type for GET label endpoint
data LabelResponse = LabelResponse
    { p_id :: Text
    , p_name :: Text
    , p_color :: Text
    , p_order :: Maybe Int
    , p_is_favorite :: Bool
    }
    deriving (Show, Generic)

instance FromJSON LabelResponse where
    parseJSON :: Value -> Parser LabelResponse
    parseJSON = genericParseJSON jsonOpts

instance ToJSON LabelResponse where
    toJSON :: LabelResponse -> Value
    toJSON = genericToJSON jsonOpts
