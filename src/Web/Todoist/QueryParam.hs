module Web.Todoist.QueryParam
    ( QueryParam (..)
    ) where

import Web.Todoist.Internal.Types (Params)

{- | Type class for converting domain types to HTTP query parameters.
Used by various parameter types across different domains (Task, Project, etc.)
-}
class QueryParam a where
    toQueryParam :: a -> Params
