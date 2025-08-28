-- everything related to making HTTP calls
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module TodoistSDK.Internal.HTTP (
  getRequest,
  Endpoint(..)
) where

import TodoistSDK.Interpreter.Types
import TodoistSDK.Types

import Data.Text.Encoding ( encodeUtf8 )
import Network.Wreq.Types (Options)
import Control.Monad.Trans.Except
import Control.Lens              ((&), (.~), (^.))
import qualified Data.List as L
import Control.Monad.IO.Class (liftIO)
import GHC.Generics         (Generic)
import Data.Aeson hiding (Options)
import Network.Wreq (defaults, getWith, header, responseBody, responseStatus, statusCode)
import Control.Monad

data Endpoint = Endpoint { path :: [String], query :: [(String, String)] }

data ProjectAPIResponse = ProjectAPIResponse {
  results     :: [ProjectResponse]
  , next_cursor :: Maybe String
  } deriving (Show, Generic, FromJSON, ToJSON)

data ProjectResponse = ProjectResponse {id :: String} deriving (Show, Generic, FromJSON, ToJSON)

authHeader :: Token -> Options
authHeader (Token apiKey) = defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 apiKey]

getRequest :: Endpoint -> Token -> ExceptT TodoistError IO [ProjectId]
getRequest endpoint token = do
  let url = constructUrl endpoint
  response <- liftIO $ getWith (authHeader token) url
  let statCode = response ^. (responseStatus . statusCode)
  unless (statCode == 200) (except $ Left NotTwoHunderd)
  let body = response ^. responseBody
      projectsMaybe = results <$> (decode body :: Maybe ProjectAPIResponse)
  maybe (except $ Left NotTwoHunderd) (pure . map fromFresponseToID) projectsMaybe
  where
    fromFresponseToID :: ProjectResponse -> ProjectId
    fromFresponseToID ProjectResponse{id} = ProjectId id 

constructUrl :: Endpoint -> String
constructUrl Endpoint{..} = L.intercalate "/" path <> if L.null query then "" else "?=" <> L.intercalate "&" (fmap (\(key, val) -> key <> "=" <> val) query)

-- use as a uniform interface

-- runRequest
--   :: TodoistEnv
--   -> Method
--   -> Endpoint
--   -> Maybe Value           -- ^ JSON body (if any)
--   -> IO (Either HttpError LBS.ByteString)