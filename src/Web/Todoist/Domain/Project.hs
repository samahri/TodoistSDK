{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: Create Web.Todoist.Types module for shared data types (ProjectId, TaskId, etc.)
module Web.Todoist.Domain.Project
    ( TodoistProjectM (..)
    , Project (..)
    , ProjectId (..)
    , Collaborator (..)
    , ProjectCreate (..)
    , newProject
    -- defaultProject
    ) where

import Web.Todoist.Builder (Builder, seed)
import Web.Todoist.Builder.Has
    ( HasDescription (..)
    , HasParentId (..)
    , HasViewStyle (..)
    , HasWorkspaceId (..)
    )
import Web.Todoist.Domain.Types (ViewStyle (..))

import Control.Monad (Monad)
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON (toJSON)
    , Value
    , defaultOptions
    , fieldLabelModifier
    , genericParseJSON
    , genericToJSON
    )
import Data.Aeson.Types (Parser)
import Data.Bool (Bool (False))
import Data.Eq (Eq)
import Data.Int (Int)
import qualified Data.List as L
import Data.Maybe (Maybe (Just, Nothing))
import Data.String (String)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show (Show)

-- TODO: DOCUMENTATION - Add Haddock documentation for all exported types
newtype ProjectId = ProjectId
    { _id :: Text
    }
    deriving (Show, Eq, Generic)

-- Custom JSON instances for ProjectId to use "id" field directly
instance FromJSON ProjectId where
    parseJSON :: Value -> Parser ProjectId
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance ToJSON ProjectId where
    toJSON :: ProjectId -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1}

data Project = Project
    { _id :: Text
    , _name :: Text
    , _description :: Text
    , _order :: Int
    , _color :: Text
    , _is_collapsed :: Bool
    , _is_shared :: Bool
    , _is_favorite :: Bool
    , _is_archived :: Bool
    , _can_assign_tasks :: Bool
    , _view_style :: ViewStyle
    , _created_at :: Maybe Text
    , _updated_at :: Maybe Text
    }
    deriving (Show, Eq)

data ProjectCreate = ProjectCreate
    { _name :: Text
    , _description :: Maybe Text
    , _parentId :: Maybe Text
    , -- , _color :: Text or Int Default: {"name":"charcoal","hex":"#808080","database_index":47}
      _is_favorite :: Bool
    , _view_style :: Maybe ViewStyle
    , _workspace_id :: Maybe Int
    }
    deriving (Show, Generic)

instance ToJSON ProjectCreate where
    toJSON :: ProjectCreate -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance FromJSON ProjectCreate where
    parseJSON :: Value -> Parser ProjectCreate
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance HasDescription ProjectCreate where
    hasDescription :: Text -> ProjectCreate -> ProjectCreate
    hasDescription desc ProjectCreate {..} = ProjectCreate {_description = Just desc, ..}

instance HasParentId ProjectCreate where
    hasParentId :: Text -> ProjectCreate -> ProjectCreate
    hasParentId pid ProjectCreate {..} = ProjectCreate {_parentId = Just pid, ..}

instance HasViewStyle ProjectCreate where
    hasViewStyle :: ViewStyle -> ProjectCreate -> ProjectCreate
    hasViewStyle style ProjectCreate {..} = ProjectCreate {_view_style = Just style, ..}

instance HasWorkspaceId ProjectCreate where
    hasWorkspaceId :: Int -> ProjectCreate -> ProjectCreate
    hasWorkspaceId wid ProjectCreate {..} = ProjectCreate {_workspace_id = Just wid, ..}

-- projects
newProject :: Text -> Builder ProjectCreate
newProject name =
    seed
        ProjectCreate
            { _name = name
            , _description = Nothing
            , _parentId = Nothing
            , _is_favorite = False
            , _view_style = Nothing
            , _workspace_id = Nothing
            }

data Collaborator = Collaborator
    { _id :: Text
    , _name :: Text
    , _email :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON Collaborator where
    toJSON :: Collaborator -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance FromJSON Collaborator where
    parseJSON :: Value -> Parser Collaborator
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = L.drop 1}

data ParentId = ParentIdStr String | ParentIdInt Int deriving (Show, Generic, FromJSON, ToJSON)

-- TODO: API_DESIGN - Separate domain types from API response types (as noted below)
class (Monad m) => TodoistProjectM m where
    getAllProjects :: m [Project]

    getProject :: ProjectId -> m Project

    getProjectCollaborators :: ProjectId -> m [Collaborator]

    -- todo: separate domain types from api types
    addProject :: ProjectCreate -> m ProjectId

    deleteProject :: ProjectId -> m ()

    archiveProject :: ProjectId -> m ProjectId

    unarchiveProject :: ProjectId -> m ProjectId
