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
    , ProjectUpdate (..)
    , PaginationParam (..)
    , newProject
    , emptyProjectUpdate
    , emptyPaginationParam
    ) where

import Web.Todoist.Builder (Initial, seed)
import Web.Todoist.Builder.Has
    ( HasDescription (..)
    , HasIsFavorite (..)
    , HasName (..)
    , HasParentId (..)
    , HasViewStyle (..)
    , HasWorkspaceId (..)
    )
import Web.Todoist.Domain.Types (ViewStyle (..))
import Web.Todoist.Internal.Types (Params, ProjectPermissions)
import Web.Todoist.QueryParam (QueryParam (..))

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
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Text (Text)
import qualified Data.Text
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

{- | Request body type for updating an existing project
All fields are optional to support partial updates
-}
data ProjectUpdate = ProjectUpdate
    { _name :: Maybe Text
    , _description :: Maybe Text
    , _color :: Maybe Text -- Note: API accepts string or integer, using Text for now
    , _is_favorite :: Maybe Bool
    , _view_style :: Maybe ViewStyle
    }
    deriving (Show, Eq, Generic)

instance ToJSON ProjectUpdate where
    toJSON :: ProjectUpdate -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance FromJSON ProjectUpdate where
    parseJSON :: Value -> Parser ProjectUpdate
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance HasName ProjectUpdate where
    hasName :: Text -> ProjectUpdate -> ProjectUpdate
    hasName name ProjectUpdate {..} = ProjectUpdate {_name = Just name, ..}

instance HasDescription ProjectUpdate where
    hasDescription :: Text -> ProjectUpdate -> ProjectUpdate
    hasDescription desc ProjectUpdate {..} = ProjectUpdate {_description = Just desc, ..}

instance HasViewStyle ProjectUpdate where
    hasViewStyle :: ViewStyle -> ProjectUpdate -> ProjectUpdate
    hasViewStyle style ProjectUpdate {..} = ProjectUpdate {_view_style = Just style, ..}

instance HasIsFavorite ProjectUpdate where
    hasIsFavorite :: Bool -> ProjectUpdate -> ProjectUpdate
    hasIsFavorite fav ProjectUpdate {..} = ProjectUpdate {_is_favorite = Just fav, ..}

-- projects
newProject :: Text -> Initial ProjectCreate
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

{- | Create an empty ProjectUpdate (for use with Builder combinators)
Use with runBuilder: runBuilder emptyProjectUpdate (setName "New Name" <> setDescription "desc")
-}
emptyProjectUpdate :: Initial ProjectUpdate
emptyProjectUpdate =
    seed
        ProjectUpdate
            { _name = Nothing
            , _description = Nothing
            , _color = Nothing
            , _is_favorite = Nothing
            , _view_style = Nothing
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

{- | Query parameters for paginated requests
Used for projects and collaborators endpoints
-}
data PaginationParam = PaginationParam
    { _cursor :: Maybe Text
    , _limit :: Maybe Int
    }
    deriving (Show, Eq)

instance QueryParam PaginationParam where
    toQueryParam :: PaginationParam -> Params
    toQueryParam PaginationParam {..} =
        maybe [] (\c -> [("cursor", c)]) _cursor
            <> maybe [] (\l -> [("limit", Data.Text.show l)]) _limit

-- | Create empty PaginationParam for first page fetch
emptyPaginationParam :: PaginationParam
emptyPaginationParam = PaginationParam {_cursor = Nothing, _limit = Nothing}

data ParentId = ParentIdStr String | ParentIdInt Int deriving (Show, Generic, FromJSON, ToJSON)

class (Monad m) => TodoistProjectM m where
    -- | Get all projects (automatically fetches all pages)
    getAllProjects :: m [Project]

    getProject :: ProjectId -> m Project

    getProjectCollaborators :: ProjectId -> m [Collaborator]

    addProject :: ProjectCreate -> m ProjectId

    deleteProject :: ProjectId -> m ()

    archiveProject :: ProjectId -> m ProjectId

    unarchiveProject :: ProjectId -> m ProjectId

    getProjectPermissions :: m ProjectPermissions

    -- | Update an existing project
    updateProject :: ProjectId -> ProjectUpdate -> m Project

    {- | Get projects with manual pagination control
    Returns a tuple of (results, next_cursor) for the requested page
    -}
    getAllProjectsPaginated :: PaginationParam -> m ([Project], Maybe Text)

    {- | Get project collaborators with manual pagination control
    Returns a tuple of (results, next_cursor) for the requested page
    -}
    getProjectCollaboratorsPaginated :: ProjectId -> PaginationParam -> m ([Collaborator], Maybe Text)

    -- | Get all projects with custom page size (fetches all pages automatically)
    getAllProjectsWithLimit :: Int -> m [Project]

    -- | Get all project collaborators with custom page size (fetches all pages automatically)
    getProjectCollaboratorsWithLimit :: ProjectId -> Int -> m [Collaborator]
