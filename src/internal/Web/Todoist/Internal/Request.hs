{-# LANGUAGE DataKinds #-}

-- | Request building utilities for Todoist API
module Web.Todoist.Internal.Request
    ( TodoistRequest (..)
    , mkTodoistRequest
    , getScheme
    ) where

import Web.Todoist.Internal.Types (Endpoint, Params)

import qualified Data.List as L
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as Http

-- | Represents an HTTP request to the Todoist API
data TodoistRequest body = TodoistRequest
    { _domain :: Text
    , _endpoint :: Endpoint
    , _queryParams :: Params
    , _requestBody :: Maybe body
    }

-- | Construct a Todoist API request with the given endpoint, parameters, and body
mkTodoistRequest :: Endpoint -> Maybe Params -> Maybe body -> TodoistRequest body
mkTodoistRequest endpoint params body =
    TodoistRequest
        { _domain = "api.todoist.com"
        , _endpoint = ["api", "v1"] <> endpoint
        , _queryParams = fromMaybe [] params
        , _requestBody = body
        }

-- | Build the HTTPS URL from a request
getScheme :: TodoistRequest body -> Http.Url Http.Https
getScheme request = L.foldl (/:) (Http.https (_domain request)) (_endpoint request)
