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
    let apiRequest = mkTodoistRequest @Void ["projects"] Nothing Nothing
    resp <- liftIO $ apiGet (Proxy @ProjectId) config apiRequest

    case resp of
      Right results -> pure results
      Left err -> lift $ except (Left err)

  getProjectCollaborators :: ProjectId -> TodoistIO [Collaborator]
  getProjectCollaborators ProjectId{..} = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @Void ["projects", id, "collaborators"] Nothing Nothing
    resp <- liftIO $ apiGet (Proxy @Collaborator) config apiRequest
    case resp of
      Right results -> pure results
      Left err -> lift $ except (Left err)

  addProject :: ProjectCreate -> TodoistIO ProjectId
  addProject project = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @ProjectCreate ["projects"] Nothing (Just project)
    resp <- liftIO $ apiPost (Proxy @ProjectId) config apiRequest
    case resp of
      Right result -> pure result
      Left err -> lift $ except (Left err)

instance TodoistTaskM TodoistIO where
  getTasks :: TaskParam -> TodoistIO [TaskId]
  getTasks taskparams = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @Void ["tasks"] (Just $ toQueryParam taskparams) Nothing
    resp <- liftIO $ apiGet (Proxy @TaskId) config apiRequest
    case resp of
      Right result -> pure result
      Left err -> lift $ except (Left err)

  getTask :: TaskId -> TodoistIO Task
  getTask TaskId{..} = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @Void ["tasks", p_id] Nothing Nothing
    resp <- liftIO $ apiGet' (Proxy @Task) config apiRequest
    case resp of
      Right result -> pure result
      Left err -> lift $ except (Left err)
  
  addTask :: TaskCreate -> TodoistIO NewTask
  addTask taskCreate = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @TaskCreate ["tasks"] Nothing (Just taskCreate)
    resp <- liftIO $ apiPost (Proxy @NewTask) config apiRequest
    case resp of
      Right result -> pure result
      Left err -> lift $ except (Left err)

  updateTask :: TaskId -> TaskPatch -> TodoistIO NewTask
  updateTask TaskId{..} taskPatch = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @TaskPatch ["tasks", p_id] Nothing (Just taskPatch)
    resp <- liftIO $ apiPost (Proxy @NewTask) config apiRequest
    case resp of
      Right result -> pure result
      Left err -> lift $ except (Left err)
  
  closeTask :: TaskId -> TodoistIO ()
  closeTask TaskId{..} = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @Void ["tasks", p_id, "close"] Nothing Nothing
    resp <- liftIO $ apiPost' config apiRequest
    case resp of
      Right _ -> pure ()
      Left err -> lift $ except (Left err)

  uncloseTask :: TaskId -> TodoistIO ()
  uncloseTask TaskId{..} = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @Void ["tasks", p_id, "reopen"] Nothing Nothing
    resp <- liftIO $ apiPost' config apiRequest
    case resp of
      Right _ -> pure ()
      Left err -> lift $ except (Left err)

  deleteTask :: TaskId -> TodoistIO ()
  deleteTask TaskId{..} = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @Void ["tasks", p_id] Nothing Nothing
    resp <- liftIO $ apiDelete config apiRequest
    case resp of
      Right _ -> pure ()
      Left err -> lift $ except (Left err)

  getTasksByFilter :: TaskFilter -> TodoistIO [TaskId]
  getTasksByFilter taskFilter = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @Void ["tasks", "filter"] (Just $ toQueryParam taskFilter) Nothing
    resp <- liftIO $ apiGet (Proxy @TaskId) config apiRequest
    case resp of
      Right result -> pure result
      Left err -> lift $ except (Left err)

  moveTask :: TaskId -> MoveTask -> TodoistIO TaskId
  moveTask TaskId{..} moveTaskBody = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @MoveTask ["tasks", p_id, "move"] Nothing (Just moveTaskBody)
    resp <- liftIO $ apiPost (Proxy @TaskId) config apiRequest
    case resp of
      Right result -> pure result
      Left err -> lift $ except (Left err)

  addTaskQuick :: AddTaskQuick -> TodoistIO ()
  addTaskQuick atqBody = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @AddTaskQuick ["tasks", "quick"] Nothing (Just atqBody)
    resp <- liftIO $ apiPost'' config apiRequest
    case resp of
      Right _ -> pure ()
      Left err -> lift $ except (Left err)

  getCompletedTasksByDueDate :: CompletedTasksQueryParam -> TodoistIO [TaskId]
  getCompletedTasksByDueDate completedTasksQueryParam = TodoistIO $ do
    config <- ask
    let apiRequest = mkTodoistRequest @CompletedTasksQueryParam ["tasks", "completed", "by_due_date"] (Just $ toQueryParam completedTasksQueryParam) Nothing
    resp <- liftIO $ apiGet' (Proxy @CompletedTasksQueryParamAPI) config apiRequest
    case resp of
      Right result -> pure $ items result
      Left err -> lift $ except (Left err)
  


