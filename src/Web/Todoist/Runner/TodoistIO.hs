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
import Web.Todoist.Patch
import Web.Todoist.Runner.HttpClient
import Web.Todoist.Param 

import Data.Either
import Data.Proxy
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Control.Applicative (Applicative, pure)
import Control.Monad (Functor, Monad)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import System.IO (IO)
import Data.Void (Void)

newtype TodoistIO a 
  = TodoistIO { unTodoist :: ReaderT TodoistConfig (ExceptT TodoistError IO) a }
    deriving newtype (Functor, Applicative, Monad)

instance TodoistProjectM TodoistIO where
  getAllProjects :: TodoistIO [ProjectId]
  getAllProjects = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @Void ["api", "v1", "projects"] Nothing Nothing
    resp <- liftIO $ apiGet (Proxy @ProjectId) config apiRequest

    case resp of
      Right results -> pure results
      Left err -> lift $ except (Left err)

  getProjectCollaborators :: ProjectId -> TodoistIO [Collaborator]
  getProjectCollaborators ProjectId{..} = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @Void ["api", "v1", "projects", id, "collaborators"] Nothing Nothing
    resp <- liftIO $ apiGet (Proxy @Collaborator) config apiRequest
    case resp of
      Right results -> pure results
      Left err -> lift $ except (Left err)

  addProject :: ProjectCreate -> TodoistIO ProjectId
  addProject project = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @ProjectCreate ["api", "v1", "projects"] Nothing (Just project)
    resp <- liftIO $ apiPost (Proxy @ProjectId) config apiRequest
    case resp of
      Right result -> pure result
      Left err -> lift $ except (Left err)

instance TodoistTaskM TodoistIO where
  getTasks :: TaskParam -> TodoistIO [TaskId]
  getTasks taskparams = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @Void ["api", "v1", "tasks"] (Just $ toQueryParam taskparams) Nothing
    resp <- liftIO $ apiGet (Proxy @TaskId) config apiRequest
    case resp of
      Right result -> pure result
      Left err -> lift $ except (Left err)

  getTask :: TaskId -> TodoistIO Task
  getTask TaskId{..} = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @Void ["api", "v1", "tasks", p_id] Nothing Nothing
    resp <- liftIO $ apiGet' (Proxy @Task) config apiRequest
    case resp of
      Right result -> pure result
      Left err -> lift $ except (Left err)
  
  addTask :: TaskCreate -> TodoistIO NewTask
  addTask taskCreate = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @TaskCreate ["api", "v1", "tasks"] Nothing (Just taskCreate)
    resp <- liftIO $ apiPost (Proxy @NewTask) config apiRequest
    case resp of
      Right result -> pure result
      Left err -> lift $ except (Left err)

  updateTask :: TaskId -> TaskPatch -> TodoistIO NewTask
  updateTask TaskId{..} taskPatch = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @TaskPatch ["api", "v1", "tasks", p_id] Nothing (Just taskPatch)
    resp <- liftIO $ apiPost (Proxy @NewTask) config apiRequest
    case resp of
      Right result -> pure result
      Left err -> lift $ except (Left err)
  
  closeTask :: TaskId -> TodoistIO ()
  closeTask TaskId{..} = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @Void ["api", "v1", "tasks", p_id, "close"] Nothing Nothing
    resp <- liftIO $ apiPost' config apiRequest
    case resp of
      Right _ -> pure ()
      Left err -> lift $ except (Left err)

  uncloseTask :: TaskId -> TodoistIO ()
  uncloseTask TaskId{..} = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @Void ["api", "v1", "tasks", p_id, "reopen"] Nothing Nothing
    resp <- liftIO $ apiPost' config apiRequest
    case resp of
      Right _ -> pure ()
      Left err -> lift $ except (Left err)

  deleteTask :: TaskId -> TodoistIO ()
  deleteTask TaskId{..} = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @Void ["api", "v1", "tasks", p_id] Nothing Nothing
    resp <- liftIO $ apiDelete config apiRequest
    case resp of
      Right _ -> pure ()
      Left err -> lift $ except (Left err)

  


