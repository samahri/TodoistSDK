module Web.Todoist.Builder
    ( setDescription
    , setParentId
    , setViewStyle
    , setWorkspaceId
    , runBuilder
    ) where

import Web.Todoist.Builder.Has
    ( HasDescription (..)
    , HasParentId (..)
    , HasViewStyle (..)
    , HasWorkspaceId (..)
    )
import Web.Todoist.Domain.Types (ViewStyle)

import Data.Int (Int)
import Data.Monoid (Endo (..), Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)

newtype Builder s = Builder {runBuilder :: Endo s}

instance Semigroup (Builder s) where
    (<>) :: Builder s -> Builder s -> Builder s
    Builder f <> Builder g = Builder (f <> g)

instance Monoid (Builder s) where
    mempty :: Builder s
    mempty = Builder mempty

setDescription :: (HasDescription s) => Text -> Builder s
setDescription desc = Builder (Endo (hasDescription desc))

setParentId :: (HasParentId s) => Text -> Builder s
setParentId pid = Builder (Endo (hasParentId pid))

setViewStyle :: (HasViewStyle s) => ViewStyle -> Builder s
setViewStyle style = Builder (Endo (hasViewStyle style))

setWorkspaceId :: (HasWorkspaceId s) => Int -> Builder s
setWorkspaceId wid = Builder (Endo (hasWorkspaceId wid))
