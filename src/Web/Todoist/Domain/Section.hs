{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Web.Todoist.Domain.Section
Description : Section API types and operations for Todoist REST API
Copyright   : (c) 2025 Sam S. Almahri
License     : MIT
Maintainer  : sam.salmahri@gmail.com

This module provides types and operations for working with Todoist sections.
Sections are used to organize tasks within a project into logical groups.

= Usage Example

@
import Web.Todoist.Domain.Section
import Web.Todoist.Runner
import Web.Todoist.Util.Builder

main :: IO ()
main = do
    let config = newTodoistConfig "your-api-token"

    -- Create a section in a project
    let newSec = runBuilder (newSection "To Do" "project-id-123") mempty
    section <- todoist config (addSection newSec)

    -- Get all sections in a project with builder pattern
    let params = runBuilder newSectionParam (withProjectId "project-id-123" <> withLimit 50)
    sections <- todoist config (getSections params)
@

For more details, see: <https://developer.todoist.com/rest/v2/#sections>
-}
module Web.Todoist.Domain.Section
    ( -- * Types
      Section (..)
    , SectionId (..)
    , SectionCreate
    , SectionUpdate (..)
    , SectionParam (..)

      -- * Type Class
    , TodoistSectionM (..)

      -- * Constructors
    , newSection
    , emptySectionUpdate
    , newSectionParam
    ) where

import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value
    , genericParseJSON
    , genericToJSON
    )
import Data.Aeson.Types (Parser)
import Data.Bool (Bool (True))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import qualified Data.Aeson as A
import qualified Data.List as L

import Control.Monad (Monad)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Monoid ((<>))
import Text.Show (Show (..))
import Web.Todoist.Domain.Types (IsCollapsed, Name (..), Order (..), ProjectId (..), getProjectId)
import Web.Todoist.Util.Builder
    ( HasCursor (..)
    , HasLimit (..)
    , HasName (..)
    , HasOrder (..)
    , HasProjectId (..)
    , Initial
    , seed
    )
import Web.Todoist.Util.QueryParam (QueryParam (..))

-- | Unique identifier for a Section
newtype SectionId = SectionId {_id :: Text}
    deriving (Eq, Show, Generic)

instance FromJSON SectionId where
    parseJSON :: Value -> Parser SectionId
    parseJSON = genericParseJSON A.defaultOptions {A.fieldLabelModifier = L.drop 1}

instance ToJSON SectionId where
    toJSON :: SectionId -> Value
    toJSON (SectionId txt) = toJSON txt

{- | Simplified domain representation of a Section (5 essential fields)
Note: API returns 11 fields, but we only expose the essential ones
-}
data Section = Section
    { _id :: SectionId
    , _name :: Name
    , _project_id :: ProjectId
    , _is_collapsed :: IsCollapsed
    , _order :: Order -- Maps from section_order in API
    }
    deriving (Show, Generic)

-- | Request body for creating a new Section
data SectionCreate = SectionCreate
    { _name :: Name
    , _project_id :: ProjectId
    , _order :: Maybe Order
    }
    deriving (Show, Generic)

instance ToJSON SectionCreate where
    toJSON :: SectionCreate -> Value
    toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = L.drop 1}

{- | Request body for updating a Section (partial updates)
Uses omitNothingFields to only send fields that are set
-}
newtype SectionUpdate = SectionUpdate
    { _name :: Maybe Name
    }
    deriving (Show, Generic)

instance ToJSON SectionUpdate where
    toJSON :: SectionUpdate -> Value
    toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = L.drop 1, A.omitNothingFields = True}

-- | Query parameters for filtering and paginating sections
data SectionParam = SectionParam
    { project_id :: Maybe ProjectId
    , cursor :: Maybe Text
    , limit :: Maybe Int
    }
    deriving (Show, Generic)

instance QueryParam SectionParam where
    toQueryParam :: SectionParam -> [(Text, Text)]
    toQueryParam SectionParam {..} =
        let projectIdParam = case project_id of
                Just pid -> [("project_id", getProjectId pid)]
                Nothing -> []
            cursorParam = case cursor of
                Just c -> [("cursor", c)]
                Nothing -> []
            limitParam = case limit of
                Just l -> [("limit", T.pack $ show l)]
                Nothing -> []
         in projectIdParam <> cursorParam <> limitParam

-- | Type class defining Section operations
class (Monad m) => TodoistSectionM m where
    -- | Get all sections (automatically fetches all pages)
    getSections :: SectionParam -> m [Section]

    -- | Get a single section by ID
    getSection :: SectionId -> m Section

    -- | Create a new section
    addSection :: SectionCreate -> m SectionId

    -- | Update a section
    updateSection :: SectionUpdate -> SectionId -> m Section

    -- | Delete a section (and all its tasks)
    deleteSection :: SectionId -> m ()

    {- | Get sections with manual pagination control
    Returns a tuple of (results, next_cursor) for the requested page
    -}
    getSectionsPaginated :: SectionParam -> m ([Section], Maybe Text)

-- | Smart constructor for creating a new section
newSection :: Text -> Text -> Initial SectionCreate
newSection name projectId =
    seed
        SectionCreate
            { _name = Name name
            , _project_id = ProjectId projectId
            , _order = Nothing
            }

-- | Empty section update for builder pattern
emptySectionUpdate :: Initial SectionUpdate
emptySectionUpdate =
    seed
        SectionUpdate
            { _name = Nothing
            }

-- | Create new SectionParam for use with builder pattern
newSectionParam :: Initial SectionParam
newSectionParam =
    seed
        SectionParam
            { project_id = Nothing
            , cursor = Nothing
            , limit = Nothing
            }

-- Builder instances for ergonomic construction
instance HasName SectionCreate where
    hasName :: Text -> SectionCreate -> SectionCreate
    hasName name SectionCreate {..} = SectionCreate {_name = Name name, ..}

instance HasOrder SectionCreate where
    hasOrder :: Int -> SectionCreate -> SectionCreate
    hasOrder order SectionCreate {..} = SectionCreate {_order = Just (Order order), ..}

instance HasName SectionUpdate where
    hasName :: Text -> SectionUpdate -> SectionUpdate
    hasName name SectionUpdate {} = SectionUpdate {_name = Just (Name name), ..}

-- HasX instances for SectionParam
instance HasProjectId SectionParam where
    hasProjectId :: Text -> SectionParam -> SectionParam
    hasProjectId pid SectionParam {..} = SectionParam {project_id = Just (ProjectId pid), ..}

instance HasCursor SectionParam where
    hasCursor :: Text -> SectionParam -> SectionParam
    hasCursor c SectionParam {..} = SectionParam {cursor = Just c, ..}

instance HasLimit SectionParam where
    hasLimit :: Int -> SectionParam -> SectionParam
    hasLimit l SectionParam {..} = SectionParam {limit = Just l, ..}
