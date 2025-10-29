{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | HTTP client functions for Todoist REST API
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
import Data.Maybe (Maybe (..), fromJust)
import Data.Monoid (mconcat, (<>))
import Data.Proxy (Proxy (..))
import System.IO (IO)
import Prelude (error, print, pure, (>>))

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

-- | Perform a GET request to the Todoist API
apiGet ::
    forall a b.
    (FromJSON a) => Proxy a -> TodoistConfig -> TodoistRequest b -> IO (Either TodoistError a)
apiGet _ config request = do
    let reqfn = Http.req Http.GET scheme Http.NoReqBody (Http.jsonResponse @a) header'

    responseEither <- try @Http.HttpException $ Http.runReq Http.defaultHttpConfig reqfn

    case responseEither of
        Right r -> (pure . pure) $ Http.responseBody r
        Left l ->
            print l
                >> let sce = fromJust (Http.isStatusCodeException l)
                    in pure $ case Http.responseStatusCode sce of
                        404 -> Left NotFound
                        400 -> Left BadRequest
                        _ -> error ""
    where
        scheme = getScheme request
        params = mconcat $ L.map (\(key, val) -> (key =: val) :: Http.Option Http.Https) (_queryParams request)
        header' = getAuthHeader config <> params

{- | Unified POST request function that handles all combinations of:
  - Request body: Nothing (no body) or Just b (JSON body)
  - Response handling: IgnoreResponse or JsonResponse (parse JSON)
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
                Left l ->
                    let sce = fromJust (Http.isStatusCodeException l)
                     in pure $ case Http.responseStatusCode sce of
                            404 -> Left NotFound
                            400 -> Left BadRequest
                            _ -> error ""

-- | Perform a DELETE request to the Todoist API
apiDelete :: forall b. TodoistConfig -> TodoistRequest b -> IO (Either TodoistError ())
apiDelete config request = do
    let reqfn = Http.req Http.DELETE scheme Http.NoReqBody Http.ignoreResponse header'
    responseEither <- try @Http.HttpException $ Http.runReq Http.defaultHttpConfig reqfn

    case responseEither of
        Right response -> (pure . pure) $ Http.responseBody response
        Left l ->
            let sce = fromJust (Http.isStatusCodeException l)
             in pure $ case Http.responseStatusCode sce of
                    404 -> Left NotFound
                    400 -> Left BadRequest
                    _ -> error ""
    where
        scheme = getScheme request
        header' = getAuthHeader config

-- | Build authentication and content-type headers for API requests
getAuthHeader :: TodoistConfig -> Http.Option Http.Https
getAuthHeader config =
    Http.headerRedacted "Authorization" ("Bearer " <> getAuthToken (authToken config))
        <> Http.header "Content-Type" "application/json"
        <> Http.header "Accept" "application/json"
