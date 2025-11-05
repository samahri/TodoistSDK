{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Todoist.Domain.Label
    ( -- * Types
      Label (..)
    , LabelId (..)
    , LabelCreate (..)
    , LabelUpdate (..)
    , LabelParam (..)
    , SharedLabelParam (..)
    , SharedLabelRemove (..)
    , SharedLabelRename (..)

      -- * Type Class
    , TodoistLabelM (..)

      -- * Constructors
    , newLabel
    , emptyLabelUpdate
    , emptyLabelParam
    , emptySharedLabelParam
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import qualified Data.Aeson as A
import Data.Bool (Bool (False, True))
import Data.Eq (Eq)
import Data.Function (($))
import Data.Int (Int)
import qualified Data.List as L
import Data.Maybe (Maybe (..), maybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Show (Show, show)

import Control.Monad (Monad)
import Web.Todoist.Builder (Initial, seed)
import Web.Todoist.Builder.Has (HasColor (..), HasIsFavorite (..), HasName (..), HasOrder (..))
import Web.Todoist.QueryParam (QueryParam (..))

-- | Unique identifier for a Label
newtype LabelId = LabelId {_id :: Text}
    deriving (Eq, Show, Generic)

instance FromJSON LabelId where
    parseJSON = genericParseJSON A.defaultOptions {A.fieldLabelModifier = L.drop 1}

-- | Simplified domain representation of a Label (5 fields)
data Label = Label
    { _id :: Text
    , _name :: Text
    , _color :: Text
    , _order :: Maybe Int -- Can be null per API spec
    , _is_favorite :: Bool
    }
    deriving (Show, Generic)

-- | Request body for creating a new Label
data LabelCreate = LabelCreate
    { _name :: Text
    , _order :: Maybe Int
    , _color :: Maybe Text -- Can be string or integer per API, we'll use Text
    , _is_favorite :: Maybe Bool
    }
    deriving (Show, Generic)

instance ToJSON LabelCreate where
    toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = L.drop 1, A.omitNothingFields = True}

-- | Request body for updating a Label (partial updates)
data LabelUpdate = LabelUpdate
    { _name :: Maybe Text
    , _order :: Maybe Int
    , _color :: Maybe Text
    , _is_favorite :: Maybe Bool
    }
    deriving (Show, Generic)

instance ToJSON LabelUpdate where
    toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = L.drop 1, A.omitNothingFields = True}

-- | Query parameters for filtering and paginating labels
data LabelParam = LabelParam
    { _cursor :: Maybe Text
    , _limit :: Maybe Int
    }
    deriving (Show, Generic)

instance QueryParam LabelParam where
    toQueryParam LabelParam {..} =
        maybe [] (\c -> [("cursor", c)]) _cursor
            <> maybe [] (\l -> [("limit", T.pack $ show l)]) _limit

-- | Query parameters for shared labels
data SharedLabelParam = SharedLabelParam
    { _omit_personal :: Maybe Bool
    , _cursor :: Maybe Text
    , _limit :: Maybe Int
    }
    deriving (Show, Generic)

instance QueryParam SharedLabelParam where
    toQueryParam SharedLabelParam {..} =
        let omitParam = case _omit_personal of
                Just True -> [("omit_personal", "true")]
                Just False -> [("omit_personal", "false")]
                Nothing -> []
         in omitParam
                <> maybe [] (\c -> [("cursor", c)]) _cursor
                <> maybe [] (\l -> [("limit", T.pack $ show l)]) _limit

-- | Request body for removing a shared label
newtype SharedLabelRemove = SharedLabelRemove
    { _name :: Text
    }
    deriving (Show, Generic)

instance ToJSON SharedLabelRemove where
    toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = L.drop 1}

-- | Request body for renaming a shared label
data SharedLabelRename = SharedLabelRename
    { _name :: Text
    , _new_name :: Text
    }
    deriving (Show, Generic)

instance ToJSON SharedLabelRename where
    toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = L.drop 1}

-- | Type class defining Label operations
class (Monad m) => TodoistLabelM m where
    -- | Get all labels (automatically fetches all pages)
    getLabels :: LabelParam -> m [Label]

    -- | Get a single label by ID
    getLabel :: LabelId -> m Label

    -- | Create a new label
    addLabel :: LabelCreate -> m LabelId

    -- | Update a label
    updateLabel :: LabelId -> LabelUpdate -> m Label

    -- | Delete a label (removes from all tasks)
    deleteLabel :: LabelId -> m ()

    -- | Get labels with manual pagination control
    getLabelsPaginated :: LabelParam -> m ([Label], Maybe Text)

    -- | Get shared labels (automatically fetches all pages)
    getSharedLabels :: SharedLabelParam -> m [Text]

    -- | Get shared labels with manual pagination control
    getSharedLabelsPaginated :: SharedLabelParam -> m ([Text], Maybe Text)

    -- | Remove a shared label from all active tasks
    removeSharedLabels :: SharedLabelRemove -> m ()

    -- | Rename a shared label on all active tasks
    renameSharedLabels :: SharedLabelRename -> m ()

-- | Smart constructor for creating a new label
newLabel :: Text -> Initial LabelCreate
newLabel name =
    seed
        LabelCreate
            { _name = name
            , _order = Nothing
            , _color = Nothing
            , _is_favorite = Nothing
            }

-- | Empty label update for builder pattern
emptyLabelUpdate :: Initial LabelUpdate
emptyLabelUpdate =
    seed
        LabelUpdate
            { _name = Nothing
            , _order = Nothing
            , _color = Nothing
            , _is_favorite = Nothing
            }

-- | Create empty LabelParam for first page fetch
emptyLabelParam :: LabelParam
emptyLabelParam = LabelParam {_cursor = Nothing, _limit = Nothing}

-- | Create empty SharedLabelParam for first page fetch
emptySharedLabelParam :: SharedLabelParam
emptySharedLabelParam = SharedLabelParam {_omit_personal = Nothing, _cursor = Nothing, _limit = Nothing}

-- Builder instances for ergonomic construction
instance HasName LabelCreate where
    hasName name LabelCreate {..} = LabelCreate {_name = name, ..}

instance HasOrder LabelCreate where
    hasOrder order LabelCreate {..} = LabelCreate {_order = Just order, ..}

instance HasColor LabelCreate where
    hasColor color LabelCreate {..} = LabelCreate {_color = Just color, ..}

instance HasIsFavorite LabelCreate where
    hasIsFavorite fav LabelCreate {..} = LabelCreate {_is_favorite = Just fav, ..}

instance HasName LabelUpdate where
    hasName name LabelUpdate {..} = LabelUpdate {_name = Just name, ..}

instance HasOrder LabelUpdate where
    hasOrder order LabelUpdate {..} = LabelUpdate {_order = Just order, ..}

instance HasColor LabelUpdate where
    hasColor color LabelUpdate {..} = LabelUpdate {_color = Just color, ..}

instance HasIsFavorite LabelUpdate where
    hasIsFavorite fav LabelUpdate {..} = LabelUpdate {_is_favorite = Just fav, ..}
