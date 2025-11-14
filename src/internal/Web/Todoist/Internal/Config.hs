{- |
Module      : Web.Todoist.Internal.Config
Description : Configuration types for Todoist API client
Copyright   : (c) 2025 Sam S. Almahri
License     : MIT
Maintainer  : sam.salmahri@gmail.com

Internal module providing configuration types for the Todoist API client.
These types hold authentication credentials and are used by the HTTP layer
to make authenticated requests.

This is an internal module and not meant for direct use by end users.
Use 'newTodoistConfig' from "Web.Todoist.Runner" instead.
-}
module Web.Todoist.Internal.Config
    ( TodoistConfig (..)
    , Token (..)
    , getAuthToken
    ) where

import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Text.Show (Show)

{- | API authentication token wrapper

Wraps the raw API token text. Tokens can be obtained from
Todoist Settings → Integrations → Developer.
-}
newtype Token = Token Text deriving (Show)

{- | Configuration for Todoist API client

Contains the authentication token needed to make API requests.
Constructed using 'newTodoistConfig' from "Web.Todoist.Runner".
-}
newtype TodoistConfig = TodoistConfig
    { authToken :: Token
    }
    deriving (Show)

{- | Extract the raw ByteString from a Token

Converts the token to UTF-8 encoded ByteString for use in HTTP headers.
-}
getAuthToken :: Token -> B.ByteString
getAuthToken (Token tkn) = encodeUtf8 tkn
