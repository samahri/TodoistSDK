{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: Create Web.Todoist.Types module for shared data types (ProjectId, TaskId, etc.)
module Web.Todoist.Domain.Project
    ( TodoistProjectM (..)
    , ProjectId (..)
    , Collaborator (..)
    -- defaultProject
    ) where

import Web.Todoist.Internal.Json (jsonOpts)
import Web.Todoist.Patch ( ProjectCreate )

import Control.Monad (Monad)
import Data.Aeson
    ( FromJSON(parseJSON),
      Value,
      ToJSON(toJSON),
      genericParseJSON,
      genericToJSON )
import Data.Aeson.Types ( Parser )
import Data.Int (Int)
import Data.String (String)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show (Show)

-- TODO: NAMING - Remove p_ prefix from record fields
-- TODO: DOCUMENTATION - Add Haddock documentation for all exported types
newtype ProjectId = ProjectId
    { _id :: Text
    }
    deriving (Show, Generic)

instance FromJSON ProjectId where
    parseJSON :: Value -> Parser ProjectId
    parseJSON = genericParseJSON jsonOpts

instance ToJSON ProjectId where
    toJSON :: ProjectId -> Value
    toJSON = genericToJSON jsonOpts

data Collaborator = Collaborator
    { _id :: Text
    , _name :: Text
    , _email :: Text
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data ParentId = ParentIdStr String | ParentIdInt Int deriving (Show, Generic, FromJSON, ToJSON)

-- TODO: API_DESIGN - Separate domain types from API response types (as noted below)
class (Monad m) => TodoistProjectM m where
    -- TODO: DOCUMENTATION - Write algebraic laws
    getAllProjects :: m [ProjectId]

    getProject :: ProjectId -> m ProjectId

    getProjectCollaborators :: ProjectId -> m [Collaborator]

    -- todo: separate domain types from api types
    addProject :: ProjectCreate -> m ProjectId

    deleteProject :: ProjectId -> m ()

    archiveProject :: ProjectId -> m ProjectId

    unarchiveProject :: ProjectId -> m ProjectId
