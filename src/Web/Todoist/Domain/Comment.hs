{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Web.Todoist.Domain.Comment
Description : Comment API types and operations for Todoist REST API
Copyright   : (c) 2025 Sam Saud Almahri
License     : BSD-3-Clause
Maintainer  : sam.salmahri@gmail.com

This module provides types and operations for working with Todoist comments. Comments can be
attached to either projects or tasks, and support attachments, user notifications, and pagination.

= Usage Example

@
import Web.Todoist.Domain.Comment
import Web.Todoist.Builder
import Web.Todoist.Runner (todoist, newTodoistConfig)

main :: IO ()
main = do
    let config = newTodoistConfig "your-api-token"

    -- Create a comment on a project
    let projectComment = runBuilder (newComment "Great project!")
                         (setProjectId "project-id-123")
    result <- todoist config (addComment projectComment)

    -- Create a comment on a task
    let taskComment = runBuilder (newComment "Don't forget this!")
                      (setTaskId "task-id-456")
    result <- todoist config (addComment taskComment)

    -- Get all comments for a project (automatic pagination)
    let params = CommentParam
            { project_id = Just "project-id-123"
            , task_id = Nothing
            , cursor = Nothing
            , limit = Nothing
            , public_key = Nothing
            }
    comments <- todoist config (getComments params)

    -- Update a comment
    let update = runBuilder (newCommentUpdate "Updated text!") mempty
    updatedComment <- todoist config (updateComment commentId update)
@

For more details on the Todoist Comments API, see:
<https://developer.todoist.com/rest/v2/#comments>
-}
module Web.Todoist.Domain.Comment
    ( -- * Types
      CommentId (..)
    , Comment (..)
    , CommentCreate
    , CommentUpdate
    , CommentParam (..)

      -- * Constructors
    , newComment
    , newCommentUpdate

      -- * Type Class
    , TodoistCommentM (..)
    ) where

import Web.Todoist.Builder (Initial, seed)
import Web.Todoist.Builder.Has
    ( HasAttachment (..)
    , HasContent (..)
    , HasProjectId (..)
    , HasTaskId (..)
    , HasUidsToNotify (..)
    )
import Web.Todoist.Internal.Types (FileAttachment, Params)
import Web.Todoist.QueryParam (QueryParam (..))

import Data.Aeson
    ( ToJSON (..)
    , Value
    , genericToJSON
    , object
    )
import qualified Data.Aeson as Aeson
import Data.Bool (Bool (False, True), not)
import Data.Eq (Eq)
import Data.Foldable (null)
import qualified Data.List as L
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Tuple as L
import GHC.Generics (Generic)
import Text.Show (Show)
import Web.Todoist.Domain.Types (Attachment)
import Prelude (Int, Monad, filter, show, ($), (.), (<>))

-- | Newtype wrapper for Comment ID
newtype CommentId = CommentId {_id :: Text}
    deriving (Show, Eq, Generic)

-- | Comment domain type (cleaned up, user-facing representation)
data Comment = Comment
    { _id :: Text
    , _content :: Text
    , _poster_id :: Maybe Text
    , _posted_at :: Maybe Text
    , _task_id :: Maybe Text
    , _project_id :: Maybe Text
    , _attachment :: Maybe FileAttachment
    }
    deriving (Show, Eq, Generic)

-- | Request body for creating a comment
data CommentCreate = CommentCreate
    { _content :: Text
    , _project_id :: Maybe Text
    , _task_id :: Maybe Text
    , _attachment :: Maybe Attachment
    , _uids_to_notify :: [Int]
    }
    deriving (Show, Eq, Generic)

-- Custom ToJSON instance to omit empty lists and Nothing fields
instance ToJSON CommentCreate where
    toJSON :: CommentCreate -> Value
    toJSON CommentCreate {..} =
        object $
            filter
                (not . isEmptyValue . L.snd)
                [ ("content", Aeson.toJSON _content)
                , ("project_id", Aeson.toJSON _project_id)
                , ("task_id", Aeson.toJSON _task_id)
                , ("attachment", Aeson.toJSON _attachment)
                , ("uids_to_notify", Aeson.toJSON _uids_to_notify)
                ]
        where
            isEmptyValue :: Value -> Bool
            isEmptyValue Aeson.Null = True
            isEmptyValue (Aeson.Array v) = null v
            isEmptyValue _ = False

-- | Request body for updating a comment
newtype CommentUpdate = CommentUpdate
    { _content :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON CommentUpdate where
    toJSON :: CommentUpdate -> Value
    toJSON =
        genericToJSON
            Aeson.defaultOptions {Aeson.fieldLabelModifier = L.drop 1, Aeson.omitNothingFields = True}

-- | Query parameters for fetching comments
data CommentParam = CommentParam
    { project_id :: Maybe Text
    , task_id :: Maybe Text
    , cursor :: Maybe Text
    , limit :: Maybe Int
    , public_key :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance QueryParam CommentParam where
    toQueryParam :: CommentParam -> Params
    toQueryParam CommentParam {..} =
        let projectParam = maybe [] (\pid -> [("project_id", pid)]) project_id
            taskParam = maybe [] (\tid -> [("task_id", tid)]) task_id
            cursorParam = maybe [] (\c -> [("cursor", c)]) cursor
            limitParam = maybe [] (\l -> [("limit", T.pack $ show l)]) limit
            publicKeyParam = maybe [] (\pk -> [("public_key", pk)]) public_key
         in projectParam <> taskParam <> cursorParam <> limitParam <> publicKeyParam

{- | Constructor for CommentCreate
Truncates content to 15000 characters (Todoist API limit)
-}
newComment :: Text -> Initial CommentCreate
newComment content =
    let truncated = T.take 15000 content
     in seed $
            CommentCreate
                { _content = truncated
                , _project_id = Nothing
                , _task_id = Nothing
                , _attachment = Nothing
                , _uids_to_notify = []
                }

{- | Constructor for CommentUpdate
Truncates content to 15000 characters (Todoist API limit)
-}
newCommentUpdate :: Text -> Initial CommentUpdate
newCommentUpdate content =
    let truncated = T.take 15000 content
     in seed $ CommentUpdate {_content = Just truncated}

-- | Type class for comment operations
class (Monad m) => TodoistCommentM m where
    {- | Add a new comment to a project or task
    Todo: have it return CommentId
    -}
    addComment :: CommentCreate -> m Comment

    -- | Get all comments (automatic pagination)
    getComments :: CommentParam -> m [Comment]

    -- | Get comments with manual pagination control
    getCommentsPaginated :: CommentParam -> m ([Comment], Maybe Text)

    -- | Get a single comment by ID
    getComment :: CommentId -> m Comment

    -- | Update a comment's content
    updateComment :: CommentId -> CommentUpdate -> m Comment

    -- | Delete a comment
    deleteComment :: CommentId -> m ()

-- Builder pattern instances
instance HasContent CommentCreate where
    hasContent :: Text -> CommentCreate -> CommentCreate
    hasContent content CommentCreate {..} = CommentCreate {_content = content, ..}

instance HasProjectId CommentCreate where
    hasProjectId :: Text -> CommentCreate -> CommentCreate
    hasProjectId pid CommentCreate {..} = CommentCreate {_project_id = Just pid, ..}

instance HasTaskId CommentCreate where
    hasTaskId :: Text -> CommentCreate -> CommentCreate
    hasTaskId tid CommentCreate {..} = CommentCreate {_task_id = Just tid, ..}

instance HasAttachment CommentCreate where
    hasAttachment :: Attachment -> CommentCreate -> CommentCreate
    hasAttachment att CommentCreate {..} = CommentCreate {_attachment = Just att, ..}

instance HasUidsToNotify CommentCreate where
    hasUidsToNotify :: [Int] -> CommentCreate -> CommentCreate
    hasUidsToNotify uids CommentCreate {..} = CommentCreate {_uids_to_notify = uids, ..}
