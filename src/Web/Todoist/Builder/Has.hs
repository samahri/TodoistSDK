module Web.Todoist.Builder.Has
    ( HasDescription (..)
    , HasParentId (..)
    , HasViewStyle (..)
    , HasWorkspaceId (..)
    , HasName (..)
    , HasIsFavorite (..)
    ) where

import Web.Todoist.Domain.Types (ViewStyle)

import Data.Bool (Bool)
import Data.Int (Int)
import Data.Text (Text)

class HasDescription p where
    hasDescription :: Text -> p -> p

class HasParentId p where
    hasParentId :: Text -> p -> p

class HasViewStyle p where
    hasViewStyle :: ViewStyle -> p -> p

class HasWorkspaceId p where
    hasWorkspaceId :: Int -> p -> p

-- | Type class for types that have a name field
class HasName p where
    hasName :: Text -> p -> p

-- | Type class for types that have an is_favorite field
class HasIsFavorite p where
    hasIsFavorite :: Bool -> p -> p
