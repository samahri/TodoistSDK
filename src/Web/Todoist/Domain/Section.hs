{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

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
    , emptySectionParam
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
import Web.Todoist.Builder (Initial, seed)
import Web.Todoist.Builder.Has (HasName (..), HasOrder (..))
import Web.Todoist.QueryParam (QueryParam (..))

-- | Unique identifier for a Section
newtype SectionId = SectionId {_id :: Text}
    deriving (Eq, Show, Generic)

instance FromJSON SectionId where
    parseJSON :: Value -> Parser SectionId
    parseJSON = genericParseJSON A.defaultOptions {A.fieldLabelModifier = L.drop 1}

{- | Simplified domain representation of a Section (5 essential fields)
Note: API returns 11 fields, but we only expose the essential ones
-}
data Section = Section
    { _id :: Text
    , _name :: Text
    , _project_id :: Text
    , _is_collapsed :: Bool
    , _order :: Int -- Maps from section_order in API
    }
    deriving (Show, Generic)

-- | Request body for creating a new Section
data SectionCreate = SectionCreate
    { _name :: Text
    , _project_id :: Text
    , _order :: Maybe Int
    }
    deriving (Show, Generic)

instance ToJSON SectionCreate where
    toJSON :: SectionCreate -> Value
    toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = L.drop 1}

{- | Request body for updating a Section (partial updates)
Uses omitNothingFields to only send fields that are set
-}
newtype SectionUpdate = SectionUpdate
    { _name :: Maybe Text
    }
    deriving (Show, Generic)

instance ToJSON SectionUpdate where
    toJSON :: SectionUpdate -> Value
    toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = L.drop 1, A.omitNothingFields = True}

-- | Query parameters for filtering and paginating sections
data SectionParam = SectionParam
    { project_id :: Maybe Text
    , cursor :: Maybe Text
    , limit :: Maybe Int
    }
    deriving (Show, Generic)

instance QueryParam SectionParam where
    toQueryParam :: SectionParam -> [(Text, Text)]
    toQueryParam SectionParam {..} =
        let projectIdParam = case project_id of
                Just pid -> [("project_id", pid)]
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
    updateSection :: SectionId -> SectionUpdate -> m Section

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
            { _name = name
            , _project_id = projectId
            , _order = Nothing
            }

-- | Empty section update for builder pattern
emptySectionUpdate :: Initial SectionUpdate
emptySectionUpdate =
    seed
        SectionUpdate
            { _name = Nothing
            }

-- | Empty section parameters for builder pattern
emptySectionParam :: Initial SectionParam
emptySectionParam =
    seed
        SectionParam
            { project_id = Nothing
            , cursor = Nothing
            , limit = Nothing
            }

-- Builder instances for ergonomic construction
instance HasName SectionCreate where
    hasName :: Text -> SectionCreate -> SectionCreate
    hasName name SectionCreate {..} = SectionCreate {_name = name, ..}

instance HasOrder SectionCreate where
    hasOrder :: Int -> SectionCreate -> SectionCreate
    hasOrder order SectionCreate {..} = SectionCreate {_order = Just order, ..}

instance HasName SectionUpdate where
    hasName :: Text -> SectionUpdate -> SectionUpdate
    hasName name SectionUpdate {} = SectionUpdate {_name = Just name, ..}
