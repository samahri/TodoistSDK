{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Todoist.Patch
    ( ProjectCreate
    , TaskCreate
    , TaskPatch
    , newProject
    , newTask
    , setDescription
    , emptyTaskPatch
    ) where

import Web.Todoist.Internal.Json (jsonOpts)

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value, genericParseJSON, genericToJSON)
import Data.Aeson.Types (Parser)
import Data.Maybe (Maybe (..))
import Data.String (String)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Show (Show)

class HasDescription p where
    setDescription :: Description -> p -> p

type Name = Text
type Description = Text

data ProjectCreate = ProjectCreate
    { p_name :: Name
    , p_description :: Maybe Description
    -- , _parentId :: Maybe ParentId
    }
    deriving (Show, Generic)

instance FromJSON ProjectCreate where
    parseJSON :: Value -> Parser ProjectCreate
    parseJSON = genericParseJSON jsonOpts

instance ToJSON ProjectCreate where
    toJSON :: ProjectCreate -> Value
    toJSON = genericToJSON jsonOpts

instance HasDescription ProjectCreate where
    setDescription :: Description -> ProjectCreate -> ProjectCreate
    setDescription desc ProjectCreate {..} = ProjectCreate {p_description = Just desc, ..}

-- projects
newProject :: Name -> ProjectCreate
newProject name =
    ProjectCreate
        { p_name = name
        , p_description = Nothing
        }

data TaskCreate = TaskCreate
    { p_content :: String
    , p_description :: Maybe Description
    , p_project_id :: Maybe String
    }
    deriving (Show, Generic)

instance ToJSON TaskCreate where
    toJSON :: TaskCreate -> Value
    toJSON = genericToJSON jsonOpts

newTask :: String -> TaskCreate
newTask content =
    TaskCreate
        { p_content = content
        , p_description = Nothing
        , p_project_id = Nothing
        }

instance HasDescription TaskCreate where
    setDescription :: Description -> TaskCreate -> TaskCreate
    setDescription desc TaskCreate {..} = TaskCreate {p_description = Just desc, ..}

data TaskPatch = TaskPatch
    { p_content :: Maybe Text
    , p_description :: Maybe Description
    , p_labels :: Maybe Text
    , p_priority :: Maybe Text
    , p_due_string :: Maybe Text
    , p_due_date :: Maybe Text
    , p_due_datetime :: Maybe Text
    , p_due_lang :: Maybe Text
    , p_assignee_id :: Maybe Text
    , p_duration :: Maybe Text
    , p_duration_unit :: Maybe Text
    , p_deadline_date :: Maybe Text
    , p_deadline_lang :: Maybe Text
    }
    deriving (Show, Generic)

emptyTaskPatch :: TaskPatch
emptyTaskPatch =
    TaskPatch
        { p_content = Nothing
        , p_description = Nothing
        , p_labels = Nothing
        , p_priority = Nothing
        , p_due_string = Nothing
        , p_due_date = Nothing
        , p_due_datetime = Nothing
        , p_due_lang = Nothing
        , p_assignee_id = Nothing
        , p_duration = Nothing
        , p_duration_unit = Nothing
        , p_deadline_date = Nothing
        , p_deadline_lang = Nothing
        }

instance ToJSON TaskPatch where
    toJSON :: TaskPatch -> Value
    toJSON = genericToJSON jsonOpts

instance HasDescription TaskPatch where
    setDescription :: Description -> TaskPatch -> TaskPatch
    setDescription desc TaskPatch {..} = TaskPatch {p_description = Just desc, ..}
