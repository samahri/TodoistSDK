-- | Error types for Todoist API operations
module Web.Todoist.Internal.Error
    ( TodoistError (..)
    ) where

import Data.String (String)
import Text.Show (Show)

-- | Errors that can occur when interacting with the Todoist API
data TodoistError
    = BadRequest
    | NotFound
    | Forbidden
    | Unauthorized
    | -- | Other HTTP errors (network, auth, etc.)
      HttpError String
    deriving (Show)
