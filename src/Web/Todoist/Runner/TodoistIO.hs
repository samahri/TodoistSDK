{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

-- everything related to the interface between the function and low http request code
module Web.Todoist.Runner.TodoistIO (
  TodoistConfig(..),
  TodoistIO(..)
) where

import Web.Todoist.Project
import Web.Todoist.Task
import Web.Todoist.Runner.HttpClient

import Data.Either
import Data.Proxy
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Control.Applicative (Applicative, pure)
import Control.Monad (Functor, Monad)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import System.IO (IO, print)
import qualified Data.Maybe as Data
import GHC.Base (undefined)

newtype TodoistIO a 
  = TodoistIO { unTodoist :: ReaderT TodoistConfig (ExceptT TodoistError IO) a }
    deriving newtype (Functor, Applicative, Monad)

instance TodoistProjectM TodoistIO where
  getAllProjects :: TodoistIO [ProjectId]
  getAllProjects = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest GET ["api", "v1", "projects"] Nothing
    resp <- liftIO $ apiGet (Proxy @ProjectId) config apiRequest

    case resp of
      Right (TodoistRetrun results _) -> pure results
      Left _ -> pure []

  getProjectCollaborators :: ProjectId -> TodoistIO [Collaborator]
  getProjectCollaborators ProjectId{..} = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest GET ["api", "v1", "projects", id, "collaborators"] Nothing
    resp <- liftIO $ apiGet (Proxy @Collaborator) config apiRequest
    case resp of
      Right (TodoistRetrun results _) -> pure results
      Left _ -> pure []

  -- createProject :: Project -> TodoistIO Project
  -- createProject project = TodoistIO $ do
  --   config <- ask
  --   let apiRequest = mkTodoistRequest POST ["api", "v1", "projects"] Nothing
  --   resp <- liftIO $ apiGet (Proxy @Project) config apiRequest
  --   pure project

instance TodoistTaskM TodoistIO where
  getTasks :: TodoistIO [TaskId]
  getTasks = undefined

