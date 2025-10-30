{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Todoist.Builder.Has
    ( Has
    , HasDescription (..)
    , HasParentId (..)
    , HasViewStyle (..)
    , HasWorkspaceId (..)
    ) where

import Web.Todoist.Domain.Types (ViewStyle)

import Data.Int (Int)
import Data.Text (Text)

class (HasDescription p, HasParentId p, HasViewStyle p, HasWorkspaceId p) => Has p
instance (HasDescription p, HasParentId p, HasViewStyle p, HasWorkspaceId p) => Has p

class HasDescription p where
    hasDescription :: Text -> p -> p

class HasParentId p where
    hasParentId :: Text -> p -> p

class HasViewStyle p where
    hasViewStyle :: ViewStyle -> p -> p

class HasWorkspaceId p where
    hasWorkspaceId :: Int -> p -> p
