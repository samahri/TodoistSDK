-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Web.Todoist.Runner.HttpClient (
  -- getRequest,
  TodoistConfig(..),
  Endpoint,
  Token(..), -- make it opaque
  TodoistError(..),
  TodoistRetrun(..),
  TodoistRequest,
  Params,
  apiGet,
  apiGet',
  apiPost,
  apiPost',
  apiDelete,
  mkTodoistRequest
) where

import Prelude
import Data.Proxy
import Control.Exception (try)
import Data.Text
import qualified Data.List as L
import Data.Text.Encoding ( encodeUtf8 )
import qualified Data.ByteString.Char8 as B
import Data.Maybe

import GHC.Generics (Generic)
import Data.Aeson hiding (Options)
import qualified Network.HTTP.Req as Http
-- import qualified Network.HTTP.Client as H
import Network.HTTP.Req 


newtype Token = Token Text deriving Show

newtype TodoistConfig = TodoistConfig {
  authToken :: Token
  -- , baseUrl   :: String
} deriving Show

getAuthToken :: Token -> B.ByteString
getAuthToken (Token tkn) = encodeUtf8 tkn

data TodoistError = BadRequest | NotFound deriving Show

data TodoistRetrun a = TodoistRetrun {
  results     :: [a]
  , next_cursor :: Maybe String
  } deriving (Show, Generic, ToJSON, FromJSON)

apiGet :: forall a b. FromJSON a => Proxy a -> TodoistConfig -> TodoistRequest b -> IO (Either TodoistError [a])
apiGet _ config request = do
  let reqfn = Http.req Http.GET scheme Http.NoReqBody (Http.jsonResponse @(TodoistRetrun a)) header'

  responseEither <- try @Http.HttpException $ Http.runReq Http.defaultHttpConfig reqfn

  case responseEither of
    Right r -> let TodoistRetrun results' _ = Http.responseBody r in (pure . pure) results'
    Left l -> let sce = fromJust (Http.isStatusCodeException l) in 
        pure $ case Http.responseStatusCode sce of
          404 -> Left NotFound
          400 -> Left BadRequest
          _ -> error ""
  where
    scheme = getScheme request
    params = mconcat $ L.map (\(key, val) -> (key =: val) :: Http.Option Http.Https)(_queryParams request)
    header' = getAuthHeader config <> params

-- similar to apiGet, but its response isn't packaged in TodoistReturn
apiGet' :: forall a b. FromJSON a => Proxy a -> TodoistConfig -> TodoistRequest b -> IO (Either TodoistError a)
apiGet' _ config request = do
  let reqfn = Http.req Http.GET scheme Http.NoReqBody (Http.jsonResponse @a) header'

  responseEither <- try @Http.HttpException $ Http.runReq Http.defaultHttpConfig reqfn

  case responseEither of
    Right r -> (pure . pure) $ Http.responseBody r
    Left l -> let sce = fromJust (Http.isStatusCodeException l) in 
        pure $ case Http.responseStatusCode sce of
          404 -> Left NotFound
          400 -> Left BadRequest
          _ -> error ""
  where
    scheme = getScheme request
    params = mconcat $ L.map (\(key, val) -> (key =: val) :: Http.Option Http.Https)(_queryParams request)
    header' = getAuthHeader config <> params

apiPost :: forall a b. (FromJSON a, ToJSON b) => Proxy a -> TodoistConfig -> TodoistRequest b -> IO (Either TodoistError a)
apiPost _ config request = do
  let reqfn = Http.req Http.POST scheme (Http.ReqBodyJson body) (Http.jsonResponse @a) header'
  responseEither <- try @Http.HttpException $ Http.runReq Http.defaultHttpConfig reqfn

  case responseEither of
    Right response -> (pure . pure) $ Http.responseBody response
    Left l -> let sce = fromJust (Http.isStatusCodeException l) in 
        pure $ case Http.responseStatusCode sce of
          404 -> Left NotFound
          400 -> Left BadRequest
          _ -> error ""
  where
    scheme = getScheme request
    body = fromJust (_requestBody request)
    header' = getAuthHeader config

apiPost' :: forall b. TodoistConfig -> TodoistRequest b -> IO (Either TodoistError ())
apiPost' config request = do
  let reqfn = Http.req Http.POST scheme Http.NoReqBody Http.ignoreResponse header'
  responseEither <- try @Http.HttpException $ Http.runReq Http.defaultHttpConfig reqfn

  case responseEither of
    Right response -> (pure . pure) $ Http.responseBody response
    Left l -> let sce = fromJust (Http.isStatusCodeException l) in 
        pure $ case Http.responseStatusCode sce of
          404 -> Left NotFound
          400 -> Left BadRequest
          _ -> error ""
    where
    scheme = getScheme request
    header' = getAuthHeader config

apiDelete :: forall b. TodoistConfig -> TodoistRequest b -> IO (Either TodoistError ())
apiDelete config request = do
  let reqfn = Http.req Http.DELETE scheme Http.NoReqBody Http.ignoreResponse header'
  responseEither <- try @Http.HttpException $ Http.runReq Http.defaultHttpConfig reqfn

  case responseEither of
    Right response -> (pure . pure) $ Http.responseBody response
    Left l -> let sce = fromJust (Http.isStatusCodeException l) in 
        pure $ case Http.responseStatusCode sce of
          404 -> Left NotFound
          400 -> Left BadRequest
          _ -> error ""
  where
    scheme = getScheme request
    header' = getAuthHeader config

type Endpoint = [Text]
type Params = [(Text, Text)] -- also in Web.Todoist.Param

data TodoistRequest body = TodoistRequest {
  _domain :: Text,
  _endpoint :: Endpoint,
  _queryParams :: Params,
  _requestBody :: Maybe body
}

mkTodoistRequest :: forall body. Endpoint -> Maybe Params -> Maybe body -> TodoistRequest body
mkTodoistRequest endpoint params body = TodoistRequest {
    _domain = "api.todoist.com",
    _endpoint = endpoint,
    _queryParams = fromMaybe [] params,
    _requestBody = body
  }

getAuthHeader :: TodoistConfig -> Http.Option Http.Https
getAuthHeader config = Http.headerRedacted "Authorization" ("Bearer " <> getAuthToken (authToken config)) 
            <> Http.header "Content-Type" "application/json" 
            <> Http.header "Accept" "application/json"

getScheme :: TodoistRequest body -> Http.Url Http.Https
getScheme request = L.foldl (/:) (Http.https (_domain request)) (_endpoint request)