{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

{- |
Module      : Web.Todoist.Domain.Types
Description : Shared domain types and type classes
Copyright   : (c) 2025 Sam S. Almahri
License     : MIT
Maintainer  : sam.salmahri@gmail.com

This module provides shared types used across the Todoist SDK, including
newtype wrappers for IDs and common fields, as well as core enums and data types.

Most users will import "Web.Todoist" which re-exports these types rather than
importing this module directly.
-}
module Web.Todoist.Domain.Types
    ( Attachment
    , ViewStyle (..)
    , ProjectId (..)
    , TaskId (..)
    , Uid (..)
    , Name (..)
    , Color (..)
    , Order (..)
    , IsFavorite (..)
    , Description (..)
    , IsCollapsed (..)
    , Content (..)
    , ParentId (..)
    , parseViewStyle
    ) where

import Control.Monad (return)
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value
    , defaultOptions
    , fieldLabelModifier
    , genericParseJSON
    , genericToJSON
    , withObject
    , (.:)
    )
import Data.Aeson.Types (Parser)
import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Base (undefined)
import GHC.Generics (Generic)
import Text.Show (Show)

{- | Visual display style for projects

Projects can be displayed as a list, board (Kanban-style), or calendar view.
-}
data ViewStyle = List | Board | Calendar deriving (Show, Eq)

instance ToJSON ViewStyle where
    toJSON :: ViewStyle -> Value
    toJSON List = toJSON ("list" :: Text)
    toJSON Board = toJSON ("board" :: Text)
    toJSON Calendar = toJSON ("calendar" :: Text)

instance FromJSON ViewStyle where
    parseJSON :: Value -> Parser ViewStyle
    parseJSON v = do
        str <- parseJSON v
        return $ parseViewStyle str

-- | Parse a Text string into ViewStyle, defaulting to undefined for unrecognized values
parseViewStyle :: Text -> ViewStyle
parseViewStyle txt = case T.toLower txt of
    "list" -> List
    "board" -> Board
    "calendar" -> Calendar
    _ -> undefined -- Default to undefined for unrecognized values

-- | Attachment metadata for comment
data Attachment = Attachment
    { _resource_type :: Text
    , _file_name :: Text
    , _file_type :: Text
    , _file_url :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON Attachment where
    toJSON :: Attachment -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance FromJSON Attachment where
    parseJSON :: Value -> Parser Attachment
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = L.drop 1}

-- | Unique identifier for a Todoist project
newtype ProjectId = ProjectId
    { getProjectId :: Text
    }
    deriving (Show, Eq, Generic)

-- Custom JSON instances for ProjectId - parse from {"id": "..."} object
instance FromJSON ProjectId where
    parseJSON :: Value -> Parser ProjectId
    parseJSON = withObject "ProjectId" $ \obj ->
        ProjectId <$> obj .: "id"

instance ToJSON ProjectId where
    toJSON :: ProjectId -> Value
    toJSON (ProjectId txt) = toJSON txt

-- | Unique identifier for a Todoist task
newtype TaskId = TaskId
    { getTaskId :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON TaskId where
    parseJSON :: Value -> Parser TaskId
    parseJSON v = TaskId <$> parseJSON v

instance ToJSON TaskId where
    toJSON :: TaskId -> Value
    toJSON (TaskId txt) = toJSON txt

-- | User ID in Todoist system
newtype Uid = Uid {getUid :: Text} deriving (Show, Eq, Generic)

instance FromJSON Uid where
    parseJSON :: Value -> Parser Uid
    parseJSON v = Uid <$> parseJSON v

instance ToJSON Uid where
    toJSON :: Uid -> Value
    toJSON (Uid txt) = toJSON txt

-- | Display name for projects, tasks, or other entities
newtype Name = Name {getName :: Text} deriving (Show, Eq, Generic)

instance FromJSON Name where
    parseJSON :: Value -> Parser Name
    parseJSON v = Name <$> parseJSON v

instance ToJSON Name where
    toJSON :: Name -> Value
    toJSON (Name txt) = toJSON txt

-- | Color identifier for visual categorization (e.g., \"red\", \"blue\", \"green\")
newtype Color = Color {getColor :: Text} deriving (Show, Eq, Generic)

instance FromJSON Color where
    parseJSON :: Value -> Parser Color
    parseJSON v = Color <$> parseJSON v

instance ToJSON Color where
    toJSON :: Color -> Value
    toJSON (Color txt) = toJSON txt

-- | Sort order for items (lower numbers appear first)
newtype Order = Order {getOrder :: Int} deriving (Show, Eq, Generic)

instance FromJSON Order where
    parseJSON :: Value -> Parser Order
    parseJSON v = Order <$> parseJSON v

instance ToJSON Order where
    toJSON :: Order -> Value
    toJSON (Order txt) = toJSON txt

-- | Flag indicating whether an item is marked as favorite
newtype IsFavorite = IsFavorite {getIsFavorite :: Bool} deriving (Show, Eq, Generic)

instance FromJSON IsFavorite where
    parseJSON :: Value -> Parser IsFavorite
    parseJSON v = IsFavorite <$> parseJSON v

instance ToJSON IsFavorite where
    toJSON :: IsFavorite -> Value
    toJSON (IsFavorite txt) = toJSON txt

-- | Markdown-formatted description text
newtype Description = Description {getDescription :: Text} deriving (Show, Eq, Generic)

instance FromJSON Description where
    parseJSON :: Value -> Parser Description
    parseJSON v = Description <$> parseJSON v

instance ToJSON Description where
    toJSON :: Description -> Value
    toJSON (Description txt) = toJSON txt

-- | Flag indicating whether a section or project is collapsed in the UI
newtype IsCollapsed = IsCollapsed {getIsCollapsed :: Bool} deriving (Show, Eq, Generic)

instance FromJSON IsCollapsed where
    parseJSON :: Value -> Parser IsCollapsed
    parseJSON v = IsCollapsed <$> parseJSON v

instance ToJSON IsCollapsed where
    toJSON :: IsCollapsed -> Value
    toJSON (IsCollapsed txt) = toJSON txt

-- | Task or comment content text
newtype Content = Content {getContent :: Text} deriving (Show, Eq, Generic)

instance ToJSON Content where
    toJSON :: Content -> Value
    toJSON (Content txt) = toJSON txt

instance FromJSON Content where
    parseJSON :: Value -> Parser Content
    parseJSON v = Content <$> parseJSON v

-- | Parent project or task identifier for hierarchical relationships
newtype ParentId = ParentId
    { getParentId :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON ParentId where
    parseJSON :: Value -> Parser ParentId
    parseJSON v = ParentId <$> parseJSON v

instance ToJSON ParentId where
    toJSON :: ParentId -> Value
    toJSON (ParentId txt) = toJSON txt
