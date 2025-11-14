{-# LANGUAGE DataKinds #-}

{- |
Module      : Web.Todoist.Internal.Request
Description : Request building utilities for Todoist API
Copyright   : (c) 2025 Sam S. Almahri
License     : MIT
Maintainer  : sam.salmahri@gmail.com

Internal module for building HTTP requests to the Todoist REST API.
Provides types and functions for constructing properly formatted API requests
with endpoints, query parameters, and request bodies.

All requests use the base URL @https:\/\/api.todoist.com\/api\/v1@.
-}
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

{- | Represents an HTTP request to the Todoist API

Contains all components needed to make an API request:

- Domain: @api.todoist.com@
- Endpoint: Path segments like @[\"projects\", \"12345\"]@
- Query parameters: Optional key-value pairs for filtering/pagination
- Request body: Optional JSON payload for POST requests
-}
data TodoistRequest body = TodoistRequest
    { _domain :: Text
    , _endpoint :: Endpoint
    , _queryParams :: Params
    , _requestBody :: Maybe body
    }

{- | Construct a Todoist API request with the given components

Automatically prepends @["api", "v1"]@ to the endpoint path and sets
the domain to @api.todoist.com@.

Example:

@
mkTodoistRequest [\"projects\"] Nothing Nothing
-- Creates request to: https:\/\/api.todoist.com\/api\/v1\/projects
@
-}
mkTodoistRequest :: Endpoint -> Maybe Params -> Maybe body -> TodoistRequest body
mkTodoistRequest endpoint params body =
    TodoistRequest
        { _domain = "api.todoist.com"
        , _endpoint = ["api", "v1"] <> endpoint
        , _queryParams = fromMaybe [] params
        , _requestBody = body
        }

{- | Build the HTTPS URL from a request

Constructs the complete HTTPS URL by combining the domain and endpoint path.
Used by the HTTP layer to create the final request URL.
-}
getScheme :: TodoistRequest body -> Http.Url Http.Https
getScheme request = L.foldl (/:) (Http.https (_domain request)) (_endpoint request)
