{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Todoist.Domain.Project
    ( TodoistProjectM (..)
    , Project (..)
    , Collaborator (..)
    , ProjectCreate (..)
    , ProjectUpdate (..)
    , PaginationParam (..)
    , newProject
    , emptyProjectUpdate
    , emptyPaginationParam
    , IsShared (..)
    , IsArchived (..)
    , CanAssignTasks (..)
    ) where

import Web.Todoist.Util.Builder
    ( HasDescription (..)
    , HasIsFavorite (..)
    , HasName (..)
    , HasParentId (..)
    , HasViewStyle (..)
    , HasWorkspaceId (..)
    , Initial
    , seed
    )
import Web.Todoist.Domain.Types
    ( Color (..)
    , Description (..)
    , IsCollapsed (..)
    , IsFavorite (..)
    , Name (..)
    , Order (..)
    , ProjectId (..)
    , ViewStyle (..)
    )
import Web.Todoist.Internal.Types (Params, ProjectPermissions)
import Web.Todoist.Util.QueryParam (QueryParam (..))

import Control.Applicative ((<|>))
import Control.Monad (Monad)
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON (toJSON)
    , Value
    , defaultOptions
    , fieldLabelModifier
    , genericParseJSON
    , genericToJSON
    , omitNothingFields
    )
import Data.Aeson.Types (Parser)
import Data.Bool (Bool (False, True))
import Data.Eq (Eq)
import Data.Functor ((<$>))
import Data.Int (Int)
import qualified Data.List as L
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Text (Text)
import qualified Data.Text
import GHC.Generics (Generic)
import Text.Show (Show)

data Project = Project
    { _id :: ProjectId
    , _name :: Name
    , _description :: Description
    , _order :: Order
    , _color :: Color
    , _is_collapsed :: IsCollapsed
    , _is_shared :: IsShared
    , _is_favorite :: IsFavorite
    , _is_archived :: IsArchived
    , _can_assign_tasks :: CanAssignTasks
    , _view_style :: ViewStyle
    , _created_at :: Maybe Text
    , _updated_at :: Maybe Text
    }
    deriving (Show, Eq)

data ProjectCreate = ProjectCreate
    { _name :: Name
    , _description :: Maybe Description
    , _parent_id :: Maybe ParentId
    , _color :: Maybe Color -- Default: {"name":"charcoal","hex":"#808080","database_index":47}
    , _is_favorite :: IsFavorite
    , _view_style :: Maybe ViewStyle
    , _workspace_id :: Maybe Int
    }
    deriving (Show, Generic)

instance ToJSON ProjectCreate where
    toJSON :: ProjectCreate -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1, omitNothingFields = True}

instance FromJSON ProjectCreate where
    parseJSON :: Value -> Parser ProjectCreate
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance HasDescription ProjectCreate where
    hasDescription :: Text -> ProjectCreate -> ProjectCreate
    hasDescription desc ProjectCreate {..} = ProjectCreate {_description = Just (Description desc), ..}

instance HasParentId ProjectCreate where
    hasParentId :: Text -> ProjectCreate -> ProjectCreate
    hasParentId pid ProjectCreate {..} = ProjectCreate {_parent_id = Just (ParentIdStr (Data.Text.unpack pid)), ..}

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
    { _name :: Maybe Name
    , _description :: Maybe Description
    , _color :: Maybe Color -- Note: API accepts string or integer, using Text for now
    , _is_favorite :: Maybe IsFavorite
    , _view_style :: Maybe ViewStyle
    }
    deriving (Show, Eq, Generic)

instance ToJSON ProjectUpdate where
    toJSON :: ProjectUpdate -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1, omitNothingFields = True}

instance FromJSON ProjectUpdate where
    parseJSON :: Value -> Parser ProjectUpdate
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance HasName ProjectUpdate where
    hasName :: Text -> ProjectUpdate -> ProjectUpdate
    hasName name ProjectUpdate {..} = ProjectUpdate {_name = Just (Name name), ..}

instance HasDescription ProjectUpdate where
    hasDescription :: Text -> ProjectUpdate -> ProjectUpdate
    hasDescription desc ProjectUpdate {..} = ProjectUpdate {_description = Just (Description desc), ..}

instance HasViewStyle ProjectUpdate where
    hasViewStyle :: ViewStyle -> ProjectUpdate -> ProjectUpdate
    hasViewStyle style ProjectUpdate {..} = ProjectUpdate {_view_style = Just style, ..}

instance HasIsFavorite ProjectUpdate where
    hasIsFavorite :: Bool -> ProjectUpdate -> ProjectUpdate
    hasIsFavorite fav ProjectUpdate {..} = ProjectUpdate {_is_favorite = Just (IsFavorite fav), ..}

-- projects
newProject :: Text -> Initial ProjectCreate
newProject name =
    seed
        ProjectCreate
            { _name = Name name
            , _description = Nothing
            , _parent_id = Nothing
            , _color = Nothing
            , _is_favorite = IsFavorite False
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
    , _name :: Name
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

data ParentId = ParentIdStr String | ParentIdInt Int deriving (Show, Generic)

instance ToJSON ParentId where
    toJSON :: ParentId -> Value
    toJSON (ParentIdStr s) = toJSON s
    toJSON (ParentIdInt i) = toJSON i

instance FromJSON ParentId where
    parseJSON :: Value -> Parser ParentId
    parseJSON v = (ParentIdStr <$> parseJSON v) <|> (ParentIdInt <$> parseJSON v)

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

newtype IsShared = IsShared {getIsShared :: Bool} deriving (Show, Eq, Generic)

instance FromJSON IsShared where
    parseJSON :: Value -> Parser IsShared
    parseJSON v = IsShared <$> parseJSON v

instance ToJSON IsShared where
    toJSON :: IsShared -> Value
    toJSON (IsShared txt) = toJSON txt

newtype IsArchived = IsArchived {getIsArchived :: Bool} deriving (Show, Eq, Generic)

instance FromJSON IsArchived where
    parseJSON :: Value -> Parser IsArchived
    parseJSON v = IsArchived <$> parseJSON v

instance ToJSON IsArchived where
    toJSON :: IsArchived -> Value
    toJSON (IsArchived txt) = toJSON txt

newtype CanAssignTasks = CanAssignTasks {getCanAssignTasks :: Bool} deriving (Show, Eq, Generic)

instance FromJSON CanAssignTasks where
    parseJSON :: Value -> Parser CanAssignTasks
    parseJSON v = CanAssignTasks <$> parseJSON v

instance ToJSON CanAssignTasks where
    toJSON :: CanAssignTasks -> Value
    toJSON (CanAssignTasks txt) = toJSON txt

newtype Email = Email {getEmail :: Text} deriving (Show, Eq, Generic)

instance FromJSON Email where
    parseJSON :: Value -> Parser Email
    parseJSON v = Email <$> parseJSON v

instance ToJSON Email where
    toJSON :: Email -> Value
    toJSON (Email txt) = toJSON txt
