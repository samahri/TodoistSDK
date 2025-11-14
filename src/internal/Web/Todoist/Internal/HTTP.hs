{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Web.Todoist.Internal.HTTP
Description : HTTP client functions for Todoist REST API
Copyright   : (c) 2025 Sam S. Almahri
License     : MIT
Maintainer  : sam.salmahri@gmail.com

Internal module providing low-level HTTP operations for the Todoist REST API.
Uses the @req@ library to make authenticated HTTP requests (GET, POST, DELETE)
and handles error responses.

All functions return @Either TodoistError a@ to represent possible failures.
HTTP exceptions are caught and converted to 'TodoistError' values.
-}
module Web.Todoist.Internal.HTTP
    ( apiGet
    , apiPost
    , PostResponse (..)
    , apiDelete
    , getAuthHeader
    ) where

import Web.Todoist.Internal.Config (TodoistConfig (..), getAuthToken)
import Web.Todoist.Internal.Error (TodoistError (..))
import Web.Todoist.Internal.Request (TodoistRequest (..), getScheme)

import Control.Exception (try)
import Data.Either (Either (..))
import Data.Function (($), (.))
import qualified Data.List as L
import Data.Maybe (Maybe (..))
import Data.Monoid (mconcat, (<>))
import Data.Proxy (Proxy (..))
import System.IO (IO)
import Text.Show (show)

import Control.Applicative (Applicative (pure))
import Data.Aeson (FromJSON, ToJSON)
import Network.HTTP.Req ((=:))
import qualified Network.HTTP.Req as Http

{- | GADT for specifying how to handle POST response
This ties the response handling strategy to the return type
-}
data PostResponse a where
    -- | Ignore the response body, return ()
    IgnoreResponse :: PostResponse ()
    -- | Parse JSON response into type 'a' (requires FromJSON constraint)
    JsonResponse :: (FromJSON a) => Proxy a -> PostResponse a

{- | Perform a GET request to the Todoist API

Makes an authenticated GET request to retrieve data from the API.
Query parameters from the 'TodoistRequest' are added to the URL.

Returns @Right a@ on success with parsed JSON response,
or @Left TodoistError@ on failure.
-}
apiGet ::
    forall a b.
    (FromJSON a) => Proxy a -> TodoistConfig -> TodoistRequest b -> IO (Either TodoistError a)
apiGet _ config request = do
    let reqfn = Http.req Http.GET scheme Http.NoReqBody (Http.jsonResponse @a) header'

    responseEither <- try @Http.HttpException $ Http.runReq Http.defaultHttpConfig reqfn

    case responseEither of
        Right r -> (pure . pure) $ Http.responseBody r
        Left l -> pure $ handleException l
    where
        scheme = getScheme request
        params = mconcat $ L.map (\(key, val) -> (key =: val) :: Http.Option Http.Https) (_queryParams request)
        header' = getAuthHeader config <> params

{- | Perform a POST request to the Todoist API

Unified POST request function that handles all combinations of:

- Request body: 'Nothing' (no body) or 'Just b' (JSON body)
- Response handling: 'IgnoreResponse' or 'JsonResponse' (parse JSON)

Returns @Right responseBody@ on success (or @Right ()@ if ignoring response),
or @Left TodoistError@ on failure.

The 'PostResponse' GADT ensures type safety between response handling strategy
and return type.
-}
apiPost ::
    forall requestBody responseBody b.
    (ToJSON requestBody) =>
    Maybe requestBody ->
    PostResponse responseBody ->
    TodoistConfig ->
    TodoistRequest b ->
    IO (Either TodoistError responseBody)
apiPost maybeBody responseSpec config request =
    case (maybeBody, responseSpec) of
        (Nothing, IgnoreResponse) -> do
            let reqfn = Http.req Http.POST scheme Http.NoReqBody Http.ignoreResponse header'
            handleResponse reqfn
        (Nothing, JsonResponse (_ :: Proxy responseBody)) -> do
            let reqfn = Http.req Http.POST scheme Http.NoReqBody (Http.jsonResponse @responseBody) header'
            handleResponse reqfn
        (Just body, IgnoreResponse) -> do
            let reqfn = Http.req Http.POST scheme (Http.ReqBodyJson body) Http.ignoreResponse header'
            handleResponse reqfn
        (Just body, JsonResponse (_ :: Proxy responseBody)) -> do
            let reqfn = Http.req Http.POST scheme (Http.ReqBodyJson body) (Http.jsonResponse @responseBody) header'
            handleResponse reqfn
    where
        scheme = getScheme request
        header' = getAuthHeader config

        handleResponse reqfn = do
            responseEither <- try @Http.HttpException $ Http.runReq Http.defaultHttpConfig reqfn
            case responseEither of
                Right response -> (pure . pure) $ Http.responseBody response
                Left l -> pure $ handleException l

{- | Perform a DELETE request to the Todoist API

Makes an authenticated DELETE request to remove a resource.
Always ignores the response body and returns @()@ on success.

Returns @Right ()@ on successful deletion,
or @Left TodoistError@ on failure.
-}
apiDelete :: forall b. TodoistConfig -> TodoistRequest b -> IO (Either TodoistError ())
apiDelete config request = do
    let reqfn = Http.req Http.DELETE scheme Http.NoReqBody Http.ignoreResponse header'
    responseEither <- try @Http.HttpException $ Http.runReq Http.defaultHttpConfig reqfn

    case responseEither of
        Right response -> (pure . pure) $ Http.responseBody response
        Left l -> pure $ handleException l
    where
        scheme = getScheme request
        header' = getAuthHeader config

{- | Build authentication and content-type headers for API requests

Constructs HTTP headers for Todoist API requests:

- @Authorization@: Bearer token authentication (redacted in logs)
- @Content-Type@: @application\/json@
- @Accept@: @application\/json@

All API requests require these headers for proper authentication and
content negotiation.
-}
getAuthHeader :: TodoistConfig -> Http.Option Http.Https
getAuthHeader config =
    Http.headerRedacted "Authorization" ("Bearer " <> getAuthToken (authToken config))
        <> Http.header "Content-Type" "application/json"
        <> Http.header "Accept" "application/json"

handleException :: Http.HttpException -> Either TodoistError responseBody
handleException e =
    case Http.isStatusCodeException e of
        Just sce -> case Http.responseStatusCode sce of
            400 -> Left BadRequest
            401 -> Left Unauthorized
            403 -> Left Forbidden
            404 -> Left NotFound
            code -> Left $ HttpError ("HTTP status code: " <> show code)
        Nothing -> Left $ HttpError ("HTTP error: " <> show e)
