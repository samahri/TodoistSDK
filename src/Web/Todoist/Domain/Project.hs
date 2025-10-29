{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: Create Web.Todoist.Types module for shared data types (ProjectId, TaskId, etc.)
module Web.Todoist.Domain.Project
    ( TodoistProjectM (..)
    , Project (..)
    , ProjectId (..)
    , Collaborator (..)
    , ViewStyle (..)
    , parseViewStyle
    -- defaultProject
    ) where

import Web.Todoist.Internal.Json (jsonOpts)
import Web.Todoist.Patch (ProjectCreate)

import Control.Monad (Monad)
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON (toJSON)
    , Value
    , genericParseJSON
    , genericToJSON
    )
import Data.Aeson.Types (Parser)
import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.String (String)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Base (undefined)
import GHC.Generics (Generic)
import Text.Show (Show)

-- TODO: NAMING - Remove p_ prefix from record fields
-- TODO: DOCUMENTATION - Add Haddock documentation for all exported types
newtype ProjectId = ProjectId
    { _id :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON ProjectId where
    parseJSON :: Value -> Parser ProjectId
    parseJSON = genericParseJSON jsonOpts

instance ToJSON ProjectId where
    toJSON :: ProjectId -> Value
    toJSON = genericToJSON jsonOpts

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

data ViewStyle = List | Board | Calendar deriving (Show, Eq)

-- | Parse a Text string into ViewStyle, defaulting to List for unrecognized values
parseViewStyle :: Text -> ViewStyle
parseViewStyle txt = case T.toLower txt of
    "list" -> List
    "board" -> Board
    "calendar" -> Calendar
    _ -> undefined -- Default to undefined for unrecognized values

data Collaborator = Collaborator
    { _id :: Text
    , _name :: Text
    , _email :: Text
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data ParentId = ParentIdStr String | ParentIdInt Int deriving (Show, Generic, FromJSON, ToJSON)

-- TODO: API_DESIGN - Separate domain types from API response types (as noted below)
class (Monad m) => TodoistProjectM m where
    -- TODO: DOCUMENTATION - Write algebraic laws
    getAllProjects :: m [Project]

    getProject :: ProjectId -> m Project

    getProjectCollaborators :: ProjectId -> m [Collaborator]

    -- todo: separate domain types from api types
    addProject :: ProjectCreate -> m ProjectId

    deleteProject :: ProjectId -> m ()

    archiveProject :: ProjectId -> m ProjectId

    unarchiveProject :: ProjectId -> m ProjectId
