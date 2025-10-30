module Web.Todoist.Builder.Has
    ( HasDescription (..)
    ) where

import Data.Text (Text)

class HasDescription p where
    hasDescription :: Text -> p -> p
