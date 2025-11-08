-- | Configuration types for Todoist API client
module Web.Todoist.Internal.Config
    ( TodoistConfig (..)
    , Token (..)
    , getAuthToken
    ) where

import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Text.Show (Show)

-- | API authentication token
newtype Token = Token Text deriving (Show)

-- | Configuration for Todoist API client
newtype TodoistConfig = TodoistConfig
    { authToken :: Token
    }
    deriving (Show)

-- | Extract the raw ByteString from a Token
getAuthToken :: Token -> B.ByteString
getAuthToken (Token tkn) = encodeUtf8 tkn
