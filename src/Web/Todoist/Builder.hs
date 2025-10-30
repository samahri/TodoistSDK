module Web.Todoist.Builder
    ( setDescription
    , runBuilder
    ) where

import Web.Todoist.Builder.Has (HasDescription (..))

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
