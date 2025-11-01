{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | TodoistTaskM instance for the TodoistIO monad
module Web.Todoist.Runner.TodoistIO.Task () where

import Web.Todoist.Domain.Task
    ( AddTaskQuick
    , CompletedTasksQueryParam
    , CompletedTasksQueryParamAPI (items)
    , MoveTask
    , NewTask
    , Task
    , TaskCreate
    , TaskFilter (..)
    , TaskId (..)
    , TaskParam (..)
    , TaskPatch
    , TodoistTaskM (..)
    )
import Web.Todoist.Internal.Config (TodoistConfig)
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Internal.HTTP (PostResponse (..), apiDelete, apiGet, apiPost)
import Web.Todoist.Internal.Request (mkTodoistRequest)
import Web.Todoist.Internal.Types (TodoistReturn (next_cursor, results))
import Web.Todoist.QueryParam (QueryParam (toQueryParam))

import Control.Applicative (pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Monoid ((<>))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import System.IO (IO)

-- Import TodoistIO type from Core module to avoid circular dependencies
import Web.Todoist.Runner.TodoistIO.Core (TodoistIO (..))

instance TodoistTaskM TodoistIO where
    getTasks :: TaskParam -> TodoistIO [TaskId]
    getTasks initialParams = TodoistIO $ do
        config <- ask
        let loop :: Maybe Text -> [TaskId] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [TaskId]
            loop cursorVal acc = do
                let TaskParam {project_id, section_id, parent_id, task_ids} = initialParams
                    params = TaskParam {project_id, section_id, parent_id, task_ids, cursor = cursorVal, limit = Nothing}
                    apiRequest = mkTodoistRequest @Void ["tasks"] (Just $ toQueryParam params) Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn TaskId)) config apiRequest
                case resp of
                    Right res -> do
                        let newAcc = acc <> results res
                        case next_cursor res of
                            Nothing -> pure newAcc
                            Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    getTask :: TaskId -> TodoistIO Task
    getTask TaskId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", _id] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @Task) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    addTask :: TaskCreate -> TodoistIO NewTask
    addTask taskCreate = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @TaskCreate ["tasks"] Nothing Nothing
        resp <- liftIO $ apiPost (Just taskCreate) (JsonResponse (Proxy @NewTask)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    updateTask :: TaskId -> TaskPatch -> TodoistIO NewTask
    updateTask TaskId {..} taskPatch = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @TaskPatch ["tasks", _id] Nothing Nothing
        resp <- liftIO $ apiPost (Just taskPatch) (JsonResponse (Proxy @NewTask)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    closeTask :: TaskId -> TodoistIO ()
    closeTask TaskId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", _id, "close"] Nothing Nothing
        resp <- liftIO $ apiPost (Nothing @Void) IgnoreResponse config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    uncloseTask :: TaskId -> TodoistIO ()
    uncloseTask TaskId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", _id, "reopen"] Nothing Nothing
        resp <- liftIO $ apiPost (Nothing @Void) IgnoreResponse config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    deleteTask :: TaskId -> TodoistIO ()
    deleteTask TaskId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", _id] Nothing Nothing
        resp <- liftIO $ apiDelete config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    getTasksByFilter :: TaskFilter -> TodoistIO [TaskId]
    getTasksByFilter initialFilter = TodoistIO $ do
        config <- ask
        let loop :: Maybe Text -> [TaskId] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [TaskId]
            loop cursorVal acc = do
                let TaskFilter {query, lang, limit} = initialFilter
                    filter' = TaskFilter {query, lang, cursor = cursorVal, limit}
                    apiRequest = mkTodoistRequest @Void ["tasks", "filter"] (Just $ toQueryParam filter') Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn TaskId)) config apiRequest
                case resp of
                    Right res -> do
                        let newAcc = acc <> results res
                        case next_cursor res of
                            Nothing -> pure newAcc
                            Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    moveTask :: TaskId -> MoveTask -> TodoistIO TaskId
    moveTask TaskId {..} moveTaskBody = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @MoveTask ["tasks", _id, "move"] Nothing Nothing
        resp <- liftIO $ apiPost (Just moveTaskBody) (JsonResponse (Proxy @TaskId)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    addTaskQuick :: AddTaskQuick -> TodoistIO ()
    addTaskQuick atqBody = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @AddTaskQuick ["tasks", "quick"] Nothing Nothing
        resp <- liftIO $ apiPost (Just atqBody) IgnoreResponse config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    getCompletedTasksByDueDate :: CompletedTasksQueryParam -> TodoistIO [TaskId]
    getCompletedTasksByDueDate completedTasksQueryParam = TodoistIO $ do
        config <- ask
        let apiRequest =
                mkTodoistRequest @CompletedTasksQueryParam
                    ["tasks", "completed", "by_due_date"]
                    (Just $ toQueryParam completedTasksQueryParam)
                    Nothing
        resp <- liftIO $ apiGet (Proxy @CompletedTasksQueryParamAPI) config apiRequest
        case resp of
            Right res -> pure $ items res
            Left err -> lift $ except (Left err)

    getCompletedTasksByCompletionDate :: CompletedTasksQueryParam -> TodoistIO [TaskId]
    getCompletedTasksByCompletionDate completedTasksQueryParam = TodoistIO $ do
        config <- ask
        let apiRequest =
                mkTodoistRequest @CompletedTasksQueryParam
                    ["tasks", "completed", "by_completion_date"]
                    (Just $ toQueryParam completedTasksQueryParam)
                    Nothing
        resp <- liftIO $ apiGet (Proxy @CompletedTasksQueryParamAPI) config apiRequest
        case resp of
            Right res -> pure $ items res
            Left err -> lift $ except (Left err)

    getTasksPaginated :: TaskParam -> TodoistIO ([TaskId], Maybe Text)
    getTasksPaginated taskparams = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks"] (Just $ toQueryParam taskparams) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn TaskId)) config apiRequest
        case resp of
            Right res -> pure (results res, fmap T.pack (next_cursor res))
            Left err -> lift $ except (Left err)

    getTasksByFilterPaginated :: TaskFilter -> TodoistIO ([TaskId], Maybe Text)
    getTasksByFilterPaginated taskFilter = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["tasks", "filter"] (Just $ toQueryParam taskFilter) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn TaskId)) config apiRequest
        case resp of
            Right res -> pure (results res, fmap T.pack (next_cursor res))
            Left err -> lift $ except (Left err)

    getTasksWithLimit :: TaskParam -> Int -> TodoistIO [TaskId]
    getTasksWithLimit baseParams pageLimit = TodoistIO $ do
        let loop :: Maybe Text -> [TaskId] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [TaskId]
            loop cursorVal acc = do
                let TaskParam {project_id, section_id, parent_id, task_ids} = baseParams
                    params =
                        TaskParam {project_id, section_id, parent_id, task_ids, cursor = cursorVal, limit = Just pageLimit}
                (tasks, nextCursor) <- unTodoist $ getTasksPaginated params
                let newAcc = acc <> tasks
                case nextCursor of
                    Nothing -> pure newAcc
                    Just c -> loop (Just c) newAcc
        loop Nothing []

    getTasksByFilterWithLimit :: TaskFilter -> Int -> TodoistIO [TaskId]
    getTasksByFilterWithLimit baseFilter pageLimit = TodoistIO $ do
        let loop :: Maybe Text -> [TaskId] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [TaskId]
            loop cursorVal acc = do
                let TaskFilter {query, lang} = baseFilter
                    filter' = TaskFilter {query, lang, cursor = cursorVal, limit = Just pageLimit}
                (tasks, nextCursor) <- unTodoist $ getTasksByFilterPaginated filter'
                let newAcc = acc <> tasks
                case nextCursor of
                    Nothing -> pure newAcc
                    Just c -> loop (Just c) newAcc
        loop Nothing []
