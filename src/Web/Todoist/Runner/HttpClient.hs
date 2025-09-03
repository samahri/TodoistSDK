-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE RankNTypes #-}

module Web.Todoist.Runner.HttpClient (
  -- getRequest,
  TodoistConfig(..),
  Endpoint(..),
  Token(..), -- make it opaque
  TodoistError(..),
  TodoistRetrun(..),
  Method(..),
  apiGet,
  mkTodoistRequest
) where

import Web.Todoist.Project

import Control.Applicative (pure)
import Control.Monad.IO.Class (liftIO)
import Prelude
import Data.Proxy

import Data.Text
import Data.List
import Data.Text.Encoding ( encodeUtf8 )
import qualified Data.ByteString.Char8 as B

import Network.Wreq.Types (Options)
import Control.Monad.Trans.Except
import GHC.Generics (Generic)
import Data.Aeson hiding (Options)
import qualified Network.HTTP.Req as Http
import Network.HTTP.Req ((/:))
import qualified Data.Maybe as Data
import Data.ByteString (ByteString)

newtype Token = Token Text deriving Show

data TodoistConfig = TodoistConfig {
  authToken :: Token
  -- , baseUrl   :: String
} deriving Show

getAuthToken :: Token -> B.ByteString
getAuthToken (Token tkn) = encodeUtf8 tkn

data TodoistError = NotTwoHunderd deriving Show

data TodoistRetrun a = TodoistRetrun {
  results     :: [a]
  , next_cursor :: Maybe String
  } deriving (Show, Generic, ToJSON, FromJSON)


apiGet :: forall a. (FromJSON a, Show a) => Proxy a -> TodoistConfig -> TodoistRequest -> IO (Either TodoistError (TodoistRetrun a))
apiGet _ config request = do

  let reqfn = Http.req Http.GET scheme Http.NoReqBody (Http.jsonResponse @(TodoistRetrun a)) header 
  response <- Http.runReq Http.defaultHttpConfig reqfn
  (pure . pure) $ Http.responseBody response
  -- POST -> do 
  --     liftIO $ print (Http.responseBody response)
  --     (pure . pure) (TodoistRetrun [] Nothing)

  where
    scheme = Data.List.foldl (/:) (Http.https (_domain request)) (_endpoint request)
    body = _requestBody request
    header = Http.headerRedacted "Authorization" ("Bearer " <> getAuthToken (authToken config)) 
            <> Http.header "Content-Type" "application/json" 
            <> Http.header "Accept" "application/json"

    -- reqfn = case method of 
    --   POST -> Http.req Http.POST scheme (Http.ReqBodyBs body) (Http.jsonResponse @a) header 

--- 
data Method = GET | POST deriving Show -- (Eq, Ord, Read, Show)
-- data Endpoint = Endpoint { path :: [String], queryParams :: [(String, String)] }
type Endpoint = [Text]
-- | HTTP Params
type Params = [(Text, Text)]

data TodoistRequest = TodoistRequest {
  _method :: Method,
  _domain :: Text,
  _endpoint :: Endpoint,
  _queryParams :: Maybe Params,
  _requestBody :: ByteString
}

mkTodoistRequest :: Method -> Endpoint -> Maybe Params -> TodoistRequest
mkTodoistRequest method endpoint params = TodoistRequest {
    _domain = "api.todoist.com",
    _endpoint = endpoint,
    _queryParams = params,
    _requestBody = encodeUtf8 "{\"name\": \"new test project\"}"
  }