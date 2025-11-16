{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Web.Todoist.Domain.Label
Description : Label API types and operations for Todoist REST API
Copyright   : (c) 2025 Sam S. Almahri
License     : MIT
Maintainer  : sam.salmahri@gmail.com

This module provides types and operations for working with Todoist labels.
Labels are tags that can be applied to tasks for categorization and filtering.
Supports both personal and shared labels.

= Usage Example

@
import Web.Todoist.Domain.Label
import Web.Todoist.Runner
import Web.Todoist.Util.Builder

main :: IO ()
main = do
    let config = newTodoistConfig "your-api-token"

    -- Create a label
    let newLbl = runBuilder (newLabel "urgent") mempty
    label <- todoist config (addLabel newLbl)

    -- Get all labels with builder pattern
    let params = runBuilder newLabelParam (withLimit 50)
    labels <- todoist config (getLabels params)
@

For more details, see: <https://developer.todoist.com/rest/v2/#labels>
-}
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
    , newLabelParam
    , newSharedLabelParam
    ) where

import Control.Applicative ((<$>))
import Control.Monad (Monad)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, genericToJSON)
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import Data.Bool (Bool (..))
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
import Web.Todoist.Domain.Types (Color (..), IsFavorite (..), Name (..), Order (..))
import Web.Todoist.Internal.Types (Params)
import Web.Todoist.Util.Builder
    ( HasColor (..)
    , HasCursor (..)
    , HasIsFavorite (..)
    , HasLimit (..)
    , HasName (..)
    , HasOmitPersonal (..)
    , HasOrder (..)
    , Initial
    , seed
    )
import Web.Todoist.Util.QueryParam (QueryParam (..))

-- | Unique identifier for a Label
newtype LabelId = LabelId {getLabelId :: Text}
    deriving (Eq, Show, Generic)

instance ToJSON LabelId where
    toJSON :: LabelId -> Value
    toJSON (LabelId txt) = A.object ["id" A..= txt]

instance FromJSON LabelId where
    parseJSON :: Value -> Parser LabelId
    parseJSON = A.withObject "LabelId" $ \obj ->
        LabelId <$> obj A..: "id"

data Label = Label
    { _id :: LabelId
    , _name :: Name
    , _color :: Color
    , _order :: Maybe Order
    , _is_favorite :: IsFavorite
    }
    deriving (Show, Generic)

-- | Request body for creating a new Label
data LabelCreate = LabelCreate
    { _name :: Name
    , _order :: Maybe Order
    , _color :: Maybe Color -- Can be string or integer per API, we'll use Text
    , _is_favorite :: Maybe IsFavorite
    }
    deriving (Show, Generic)

instance ToJSON LabelCreate where
    toJSON :: LabelCreate -> A.Value
    toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = L.drop 1, A.omitNothingFields = True}

-- | Request body for updating a Label (partial updates)
data LabelUpdate = LabelUpdate
    { _name :: Maybe Name
    , _order :: Maybe Order
    , _color :: Maybe Color
    , _is_favorite :: Maybe IsFavorite
    }
    deriving (Show, Generic)

instance ToJSON LabelUpdate where
    toJSON :: LabelUpdate -> A.Value
    toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = L.drop 1, A.omitNothingFields = True}

-- | Query parameters for filtering and paginating labels
data LabelParam = LabelParam
    { cursor :: Maybe Text
    , limit :: Maybe Int
    }
    deriving (Show, Generic)

instance QueryParam LabelParam where
    toQueryParam :: LabelParam -> Params
    toQueryParam LabelParam {..} =
        maybe [] (\c -> [("cursor", c)]) cursor
            <> maybe [] (\l -> [("limit", T.pack $ show l)]) limit

-- | Query parameters for shared labels
data SharedLabelParam = SharedLabelParam
    { omit_personal :: Maybe Bool
    , cursor :: Maybe Text
    , limit :: Maybe Int
    }
    deriving (Show, Generic)

instance QueryParam SharedLabelParam where
    toQueryParam :: SharedLabelParam -> Params
    toQueryParam SharedLabelParam {..} =
        let omitParam = case omit_personal of
                Just True -> [("omit_personal", "true")]
                Just False -> [("omit_personal", "false")]
                Nothing -> []
         in omitParam
                <> maybe [] (\c -> [("cursor", c)]) cursor
                <> maybe [] (\l -> [("limit", T.pack $ show l)]) limit

-- | Request body for removing a shared label
newtype SharedLabelRemove = SharedLabelRemove
    { _name :: Name
    }
    deriving (Show, Generic)

instance ToJSON SharedLabelRemove where
    toJSON :: SharedLabelRemove -> A.Value
    toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = L.drop 1}

-- | Request body for renaming a shared label
data SharedLabelRename = SharedLabelRename
    { _name :: Name
    , _new_name :: Name
    }
    deriving (Show, Generic)

instance ToJSON SharedLabelRename where
    toJSON :: SharedLabelRename -> A.Value
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
    updateLabel :: LabelUpdate -> LabelId -> m Label

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
            { _name = Name name
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

-- | Create new LabelParam for use with builder pattern
newLabelParam :: Initial LabelParam
newLabelParam = seed $ LabelParam {cursor = Nothing, limit = Nothing}

-- | Create new SharedLabelParam for use with builder pattern
newSharedLabelParam :: Initial SharedLabelParam
newSharedLabelParam = seed $ SharedLabelParam {omit_personal = Nothing, cursor = Nothing, limit = Nothing}

-- Builder instances for ergonomic construction
instance HasName LabelCreate where
    hasName :: Text -> LabelCreate -> LabelCreate
    hasName name LabelCreate {..} = LabelCreate {_name = Name name, ..}

instance HasOrder LabelCreate where
    hasOrder :: Int -> LabelCreate -> LabelCreate
    hasOrder order LabelCreate {..} = LabelCreate {_order = Just (Order order), ..}

instance HasColor LabelCreate where
    hasColor :: Text -> LabelCreate -> LabelCreate
    hasColor color LabelCreate {..} = LabelCreate {_color = Just (Color color), ..}

instance HasIsFavorite LabelCreate where
    hasIsFavorite :: Bool -> LabelCreate -> LabelCreate
    hasIsFavorite fav LabelCreate {..} = LabelCreate {_is_favorite = Just (IsFavorite fav), ..}

instance HasName LabelUpdate where
    hasName :: Text -> LabelUpdate -> LabelUpdate
    hasName name LabelUpdate {..} = LabelUpdate {_name = Just (Name name), ..}

instance HasOrder LabelUpdate where
    hasOrder :: Int -> LabelUpdate -> LabelUpdate
    hasOrder order LabelUpdate {..} = LabelUpdate {_order = Just (Order order), ..}

instance HasColor LabelUpdate where
    hasColor :: Text -> LabelUpdate -> LabelUpdate
    hasColor color LabelUpdate {..} = LabelUpdate {_color = Just (Color color), ..}

instance HasIsFavorite LabelUpdate where
    hasIsFavorite :: Bool -> LabelUpdate -> LabelUpdate
    hasIsFavorite fav LabelUpdate {..} = LabelUpdate {_is_favorite = Just (IsFavorite fav), ..}

-- HasX instances for LabelParam
instance HasCursor LabelParam where
    hasCursor :: Text -> LabelParam -> LabelParam
    hasCursor c LabelParam {..} = LabelParam {cursor = Just c, ..}

instance HasLimit LabelParam where
    hasLimit :: Int -> LabelParam -> LabelParam
    hasLimit l LabelParam {..} = LabelParam {limit = Just l, ..}

-- HasX instances for SharedLabelParam
instance HasOmitPersonal SharedLabelParam where
    hasOmitPersonal :: Bool -> SharedLabelParam -> SharedLabelParam
    hasOmitPersonal omit SharedLabelParam {..} = SharedLabelParam {omit_personal = Just omit, ..}

instance HasCursor SharedLabelParam where
    hasCursor :: Text -> SharedLabelParam -> SharedLabelParam
    hasCursor c SharedLabelParam {..} = SharedLabelParam {cursor = Just c, ..}

instance HasLimit SharedLabelParam where
    hasLimit :: Int -> SharedLabelParam -> SharedLabelParam
    hasLimit l SharedLabelParam {..} = SharedLabelParam {limit = Just l, ..}
