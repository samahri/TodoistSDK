{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | TodoistProjectM instance for the TodoistIO monad
module Web.Todoist.Runner.TodoistIO.Project
    ( projectResponseToProject
    ) where

import Web.Todoist.Domain.Project
    ( Collaborator
    , Project (..)
    , ProjectCreate
    , ProjectId (..)
    , TodoistProjectM (..)
    )
import Web.Todoist.Domain.Types (parseViewStyle)
import Web.Todoist.Internal.HTTP (PostResponse (..), apiDelete, apiGet, apiPost)
import Web.Todoist.Internal.Request (mkTodoistRequest)
import Web.Todoist.Internal.Types
    ( CreatedAt (..)
    , ProjectPermissions
    , ProjectResponse (..)
    , TodoistReturn (results)
    , UpdatedAt (..)
    )
import Web.Todoist.Runner.TodoistIO.Core (TodoistIO (..))

import Control.Applicative (pure, (<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Reader (ask)
import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Maybe (Maybe (..))
import Data.Proxy (Proxy (Proxy))
import Data.Void (Void)

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
        let apiRequest = mkTodoistRequest @Void ["projects"] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn ProjectResponse)) config apiRequest

        case resp of
            Right res -> pure $ projectResponseToProject <$> results res
            Left err -> lift $ except (Left err)

    getProjectCollaborators :: ProjectId -> TodoistIO [Collaborator]
    getProjectCollaborators ProjectId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["projects", _id, "collaborators"] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn Collaborator)) config apiRequest
        case resp of
            Right res -> pure $ results res
            Left err -> lift $ except (Left err)

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
