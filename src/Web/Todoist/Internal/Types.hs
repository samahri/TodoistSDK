{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Internal types used across the HTTP layer
module Web.Todoist.Internal.Types
    ( TodoistReturn (..)
    , Params
    , Endpoint
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (Maybe)
import Data.String (String)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show (Show)

-- | Wrapper type for paginated API responses
data TodoistReturn a = TodoistReturn
    { results :: [a]
    , next_cursor :: Maybe String
    }
    deriving (Show, Generic, ToJSON, FromJSON)

-- | Type alias for query parameters (key-value pairs)
type Params = [(Text, Text)]

-- | Type alias for API endpoint path segments
type Endpoint = [Text]
