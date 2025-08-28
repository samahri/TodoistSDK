{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

-- everything related to the interface between the function and low http request code
module TodoistSDK.Interpreter.TodoistIO (
  TodoistEnv(..),
  Token(..),
  TodoistIO(..)
) where

import TodoistSDK.Capabilities
import TodoistSDK.Types
import TodoistSDK.Interpreter.Types
import TodoistSDK.Internal.HTTP

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

newtype TodoistIO a 
  = TodoistIO { unTodoist :: ReaderT TodoistEnv (ExceptT TodoistError IO) a }
    deriving newtype (Functor, Applicative, Monad)

instance TodoistProjectM TodoistIO where
  getAllProjects :: TodoistIO [ProjectId]
  getAllProjects = TodoistIO $ do
    TodoistEnv{authToken, baseUrl} <- ask
    let endpoint = Endpoint { path = [baseUrl, "api", "v1", "projects"], query = [] }
    lift $ getRequest endpoint authToken


