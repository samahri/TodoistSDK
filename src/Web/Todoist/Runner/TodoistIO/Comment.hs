{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Todoist.Runner.TodoistIO.Comment
    ( commentResponseToComment
    ) where

import Web.Todoist.Domain.Comment
    ( Comment (..)
    , CommentCreate
    , CommentId (..)
    , CommentParam (..)
    , CommentUpdate
    , TodoistCommentM (..)
    )
import Web.Todoist.Internal.Config (TodoistConfig)
import Web.Todoist.Internal.Error (TodoistError (HttpError))
import Web.Todoist.Internal.HTTP
    ( PostResponse (JsonResponse)
    , apiDelete
    , apiGet
    , apiPost
    )
import Web.Todoist.Internal.Request (mkTodoistRequest)
import Web.Todoist.Internal.Types (CommentResponse (..), TodoistReturn (..))
import Web.Todoist.QueryParam (toQueryParam)
import Web.Todoist.Runner.TodoistIO.Core (TodoistIO (..))

import Control.Applicative (pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Functor (fmap)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (traverse)
import Data.Void (Void)
import Prelude (IO, (<>))

{- | Convert CommentResponse to Comment
Validates that at least one of task_id or project_id is present
-}
commentResponseToComment :: CommentResponse -> Either TodoistError Comment
commentResponseToComment CommentResponse {..} =
    case (p_item_id, p_project_id) of
        (Nothing, Nothing) -> Left $ HttpError "Comment must have either task_id or project_id"
        _ ->
            Right $
                Comment
                    { _id = p_id
                    , _content = p_content
                    , _poster_id = p_posted_uid
                    , _posted_at = p_posted_at
                    , _task_id = p_item_id
                    , _project_id = p_project_id
                    , _attachment = p_file_attachment
                    }

instance TodoistCommentM TodoistIO where
    -- \| Add a new comment
    addComment :: CommentCreate -> TodoistIO Comment
    addComment comment = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @CommentCreate ["comments"] Nothing Nothing
        resp <- liftIO $ apiPost (Just comment) (JsonResponse (Proxy @CommentResponse)) config apiRequest
        case resp of
            Right res -> do
                let commentResults = commentResponseToComment res
                case commentResults of
                    Left err -> lift $ except (Left err)
                    Right cmnt -> pure cmnt
            Left err -> lift $ except (Left err)

    -- \| Get all comments (automatic pagination)
    getComments :: CommentParam -> TodoistIO [Comment]
    getComments initialParams = TodoistIO $ do
        config <- ask
        let loop :: Maybe Text -> [Comment] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Comment]
            loop cursorVal acc = do
                let CommentParam {project_id, task_id, public_key} = initialParams
                    params = CommentParam {project_id, task_id, cursor = cursorVal, limit = Nothing, public_key}
                    apiRequest = mkTodoistRequest @Void ["comments"] (Just $ toQueryParam params) Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn CommentResponse)) config apiRequest
                case resp of
                    Right res -> do
                        let commentResults = traverse commentResponseToComment (results res)
                        case commentResults of
                            Left err -> lift $ except (Left err)
                            Right comments -> do
                                let newAcc = acc <> comments
                                case next_cursor res of
                                    Nothing -> pure newAcc
                                    Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    -- \| Get comments with pagination control
    getCommentsPaginated :: CommentParam -> TodoistIO ([Comment], Maybe Text)
    getCommentsPaginated params = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["comments"] (Just $ toQueryParam params) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn CommentResponse)) config apiRequest
        case resp of
            Right res -> do
                let commentResults = traverse commentResponseToComment (results res)
                    nextCursor = fmap T.pack (next_cursor res)
                case commentResults of
                    Left err -> lift $ except (Left err)
                    Right comments -> pure (comments, nextCursor)
            Left err -> lift $ except (Left err)

    -- \| Get a single comment by ID
    getComment :: CommentId -> TodoistIO Comment
    getComment CommentId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["comments", _id] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @CommentResponse) config apiRequest
        case resp of
            Right res ->
                case commentResponseToComment res of
                    Left err -> lift $ except (Left err)
                    Right comment -> pure comment
            Left err -> lift $ except (Left err)

    -- \| Update a comment
    updateComment :: CommentId -> CommentUpdate -> TodoistIO Comment
    updateComment CommentId {..} update = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @CommentUpdate ["comments", _id] Nothing Nothing
        resp <- liftIO $ apiPost (Just update) (JsonResponse (Proxy @CommentResponse)) config apiRequest
        case resp of
            Right res ->
                case commentResponseToComment res of
                    Left err -> lift $ except (Left err)
                    Right comment -> pure comment
            Left err -> lift $ except (Left err)

    -- \| Delete a comment
    deleteComment :: CommentId -> TodoistIO ()
    deleteComment CommentId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["comments", _id] Nothing Nothing
        resp <- liftIO $ apiDelete config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)
