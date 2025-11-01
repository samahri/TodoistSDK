{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | TodoistProjectM instance for the TodoistIO monad
module Web.Todoist.Runner.TodoistIO.Project
    ( projectResponseToProject
    ) where

import Web.Todoist.Domain.Project
    ( Collaborator
    , PaginationParam (..)
    , Project (..)
    , ProjectCreate
    , ProjectId (..)
    , ProjectUpdate
    , TodoistProjectM (..)
    , emptyPaginationParam
    )
import Web.Todoist.Domain.Types (parseViewStyle)
import Web.Todoist.Internal.Config (TodoistConfig)
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Internal.HTTP (PostResponse (..), apiDelete, apiGet, apiPost)
import Web.Todoist.Internal.Request (mkTodoistRequest)
import Web.Todoist.Internal.Types
    ( CreatedAt (..)
    , ProjectPermissions
    , ProjectResponse (..)
    , TodoistReturn (next_cursor, results)
    , UpdatedAt (..)
    )
import Web.Todoist.QueryParam (QueryParam (toQueryParam))
import Web.Todoist.Runner.TodoistIO.Core (TodoistIO (..))

import Control.Applicative (pure, (<$>))
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

instance TodoistProjectM TodoistIO where
    getProject :: ProjectId -> TodoistIO Project
    getProject ProjectId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", _id] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @ProjectResponse) config apiRequest
        case resp of
            Right res -> pure $ projectResponseToProject res
            Left err -> lift $ except (Left err)

    getAllProjects :: TodoistIO [Project]
    getAllProjects = TodoistIO $ do
        config <- ask
        let loop :: Maybe Text -> [Project] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Project]
            loop cursor acc = do
                let params = emptyPaginationParam {_cursor = cursor}
                    apiRequest = mkTodoistRequest @Void ["projects"] (Just $ toQueryParam params) Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn ProjectResponse)) config apiRequest
                case resp of
                    Right res -> do
                        let newAcc = acc <> (projectResponseToProject <$> results res)
                        case next_cursor res of
                            Nothing -> pure newAcc
                            Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    getProjectCollaborators :: ProjectId -> TodoistIO [Collaborator]
    getProjectCollaborators ProjectId {..} = TodoistIO $ do
        config <- ask
        let loop ::
                Maybe Text -> [Collaborator] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Collaborator]
            loop cursor acc = do
                let params = emptyPaginationParam {_cursor = cursor}
                    apiRequest = mkTodoistRequest @Void ["projects", _id, "collaborators"] (Just $ toQueryParam params) Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn Collaborator)) config apiRequest
                case resp of
                    Right res -> do
                        let newAcc = acc <> results res
                        case next_cursor res of
                            Nothing -> pure newAcc
                            Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    deleteProject :: ProjectId -> TodoistIO ()
    deleteProject ProjectId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", _id] Nothing Nothing
        resp <- liftIO $ apiDelete config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    addProject :: ProjectCreate -> TodoistIO ProjectId
    addProject project = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @ProjectCreate ["projects"] Nothing Nothing
        resp <- liftIO $ apiPost (Just project) (JsonResponse (Proxy @ProjectId)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    archiveProject :: ProjectId -> TodoistIO ProjectId
    archiveProject ProjectId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", _id, "archive"] Nothing Nothing
        resp <- liftIO $ apiPost (Nothing @Void) (JsonResponse (Proxy @ProjectId)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    unarchiveProject :: ProjectId -> TodoistIO ProjectId
    unarchiveProject ProjectId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", _id, "unarchive"] Nothing Nothing
        resp <- liftIO $ apiPost (Nothing @Void) (JsonResponse (Proxy @ProjectId)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    getProjectPermissions :: TodoistIO ProjectPermissions
    getProjectPermissions = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", "permissions"] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @ProjectPermissions) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    updateProject :: ProjectId -> ProjectUpdate -> TodoistIO Project
    updateProject ProjectId {..} projectUpdate = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @ProjectUpdate ["projects", _id] Nothing Nothing
        resp <-
            liftIO $ apiPost (Just projectUpdate) (JsonResponse (Proxy @ProjectResponse)) config apiRequest
        case resp of
            Right res -> pure $ projectResponseToProject res
            Left err -> lift $ except (Left err)

    getAllProjectsPaginated :: PaginationParam -> TodoistIO ([Project], Maybe Text)
    getAllProjectsPaginated params = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects"] (Just $ toQueryParam params) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn ProjectResponse)) config apiRequest
        case resp of
            Right res ->
                pure (projectResponseToProject <$> results res, fmap T.pack (next_cursor res))
            Left err -> lift $ except (Left err)

    getProjectCollaboratorsPaginated ::
        ProjectId -> PaginationParam -> TodoistIO ([Collaborator], Maybe Text)
    getProjectCollaboratorsPaginated ProjectId {..} params = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", _id, "collaborators"] (Just $ toQueryParam params) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn Collaborator)) config apiRequest
        case resp of
            Right res -> pure (results res, fmap T.pack (next_cursor res))
            Left err -> lift $ except (Left err)

    getAllProjectsWithLimit :: Int -> TodoistIO [Project]
    getAllProjectsWithLimit pageLimit = TodoistIO $ do
        let loop :: Maybe Text -> [Project] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Project]
            loop cursor acc = do
                let params = PaginationParam {_cursor = cursor, _limit = Just pageLimit}
                (projects, nextCursor) <- unTodoist $ getAllProjectsPaginated params
                let newAcc = acc <> projects
                case nextCursor of
                    Nothing -> pure newAcc
                    Just c -> loop (Just c) newAcc
        loop Nothing []

    getProjectCollaboratorsWithLimit :: ProjectId -> Int -> TodoistIO [Collaborator]
    getProjectCollaboratorsWithLimit projectId pageLimit = TodoistIO $ do
        let loop ::
                Maybe Text -> [Collaborator] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Collaborator]
            loop cursor acc = do
                let params = PaginationParam {_cursor = cursor, _limit = Just pageLimit}
                (collaborators, nextCursor) <- unTodoist $ getProjectCollaboratorsPaginated projectId params
                let newAcc = acc <> collaborators
                case nextCursor of
                    Nothing -> pure newAcc
                    Just c -> loop (Just c) newAcc
        loop Nothing []

-- | Convert a ProjectResponse (HTTP API type) to a Project (domain type)
projectResponseToProject :: ProjectResponse -> Project
projectResponseToProject ProjectResponse {..} =
    let (CreatedAt createdAt) = p_created_at
        (UpdatedAt updatedAt) = p_updated_at
     in Project
            { _id = p_id
            , _name = p_name
            , _description = p_description
            , _order = p_child_order
            , _color = p_color
            , _is_collapsed = p_is_collapsed
            , _is_shared = p_is_shared
            , _is_favorite = p_is_favorite
            , _is_archived = p_is_archived
            , _can_assign_tasks = p_can_assign_tasks
            , _view_style = parseViewStyle p_view_style
            , _created_at = createdAt
            , _updated_at = updatedAt
            }
