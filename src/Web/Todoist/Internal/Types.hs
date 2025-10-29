{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

-- | Internal types used across the HTTP layer
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
import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.String (String)
import Data.Text (Text)
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
    , p_updated_at :: UpdatedAt
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
    parseJSON = genericParseJSON jsonOpts

instance ToJSON CreatorUid where
    toJSON :: CreatorUid -> Value
    toJSON = genericToJSON jsonOpts

newtype CreatedAt = CreatedAt
    { p_created_at :: Maybe Text
    }
    deriving (Show, Generic)

instance FromJSON CreatedAt where
    parseJSON :: Value -> Parser CreatedAt
    parseJSON = genericParseJSON jsonOpts

instance ToJSON CreatedAt where
    toJSON :: CreatedAt -> Value
    toJSON = genericToJSON jsonOpts

newtype UpdatedAt = UpdatedAt
    { p_updated_at :: Maybe Text
    }
    deriving (Show, Generic)

instance FromJSON UpdatedAt where
    parseJSON :: Value -> Parser UpdatedAt
    parseJSON = genericParseJSON jsonOpts

instance ToJSON UpdatedAt where
    toJSON :: UpdatedAt -> Value
    toJSON = genericToJSON jsonOpts

newtype Role = Role
    { p_role :: Maybe Text
    }
    deriving (Show, Generic)

instance FromJSON Role where
    parseJSON :: Value -> Parser Role
    parseJSON = genericParseJSON jsonOpts

instance ToJSON Role where
    toJSON :: Role -> Value
    toJSON = genericToJSON jsonOpts

newtype ParentId = ParentId
    { p_parent_id :: Maybe Text
    }
    deriving (Show, Generic)

instance FromJSON ParentId where
    parseJSON :: Value -> Parser ParentId
    parseJSON = genericParseJSON jsonOpts

instance ToJSON ParentId where
    toJSON :: ParentId -> Value
    toJSON = genericToJSON jsonOpts

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
