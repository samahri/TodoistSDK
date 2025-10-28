-- | Error types for Todoist API operations
module Web.Todoist.Internal.Error
    ( TodoistError (..)
    ) where

import Text.Show (Show)

-- | Errors that can occur when interacting with the Todoist API
data TodoistError
    = BadRequest
    | NotFound
    deriving (Show)
