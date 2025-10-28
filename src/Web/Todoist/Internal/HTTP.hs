{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | HTTP client functions for Todoist REST API
module Web.Todoist.Internal.HTTP
    ( apiGet
    , apiPost
    , apiPost'
    , apiPost''
    , apiDelete
    , getAuthHeader
    ) where

import Web.Todoist.Internal.Config (TodoistConfig (..), getAuthToken)
import Web.Todoist.Internal.Error (TodoistError (..))
import Web.Todoist.Internal.Request (TodoistRequest (..), getScheme)

import Control.Exception (try)
import qualified Data.ByteString.Char8 as B
import Data.Either (Either (..))
import Data.Function (($), (.))
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Monoid (mconcat, (<>))
import Data.Proxy (Proxy (..))
import Prelude (error, print, pure, (>>))
import System.IO (IO)

import Data.Aeson (FromJSON, ToJSON)
import qualified Network.HTTP.Req as Http
import Network.HTTP.Req ((=:))

-- | Perform a GET request to the Todoist API
apiGet ::
    forall a b. (FromJSON a) => Proxy a -> TodoistConfig -> TodoistRequest b -> IO (Either TodoistError a)
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

-- | Perform a POST request with a JSON body and expect a JSON response
apiPost ::
    forall a b.
    (FromJSON a, ToJSON b) => Proxy a -> TodoistConfig -> TodoistRequest b -> IO (Either TodoistError a)
apiPost _ config request = do
    let reqfn = Http.req Http.POST scheme (Http.ReqBodyJson body) (Http.jsonResponse @a) header'
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
        body = fromJust (_requestBody request)
        header' = getAuthHeader config

-- | Perform a POST request with no body and ignore the response
apiPost' :: forall b. TodoistConfig -> TodoistRequest b -> IO (Either TodoistError ())
apiPost' config request = do
    let reqfn = Http.req Http.POST scheme Http.NoReqBody Http.ignoreResponse header'
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

-- | Perform a POST request with a JSON body and ignore the response
apiPost'' :: forall b. (ToJSON b) => TodoistConfig -> TodoistRequest b -> IO (Either TodoistError ())
apiPost'' config request = do
    let reqfn = Http.req Http.POST scheme (Http.ReqBodyJson body) Http.ignoreResponse header'
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
        body = fromJust (_requestBody request)
        header' = getAuthHeader config

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
