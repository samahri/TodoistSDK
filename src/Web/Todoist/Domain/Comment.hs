{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Web.Todoist.Domain.Comment
Description : Comment API types and operations for Todoist REST API
Copyright   : (c) 2025 Sam Saud Almahri
License     : MIT
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

    -- Get all comments for a project with builder pattern
    let params = runBuilder newCommentParam (setProjectId "project-id-123" <> setLimit 50)
    comments <- todoist config (getComments params)

    -- Update a comment
    let update = runBuilder (newCommentUpdate "Updated text!") mempty
    updatedComment <- todoist config (updateComment update commentId)
@

For more details on the Todoist Comments API, see:
<https://developer.todoist.com/rest/v2/#comments>
-}
module Web.Todoist.Domain.Comment
    ( -- * Types
      CommentId (..)
    , Content (..)
    , Comment (..)
    , CommentCreate
    , CommentUpdate
    , CommentParam (..)

      -- * Constructors
    , newComment
    , newCommentUpdate
    , newCommentParam

      -- * Type Class
    , TodoistCommentM (..)
    ) where

import Web.Todoist.Internal.Types (FileAttachment, Params)
import Web.Todoist.Util.Builder
    ( HasAttachment (..)
    , HasContent (..)
    , HasCursor (..)
    , HasLimit (..)
    , HasProjectId (..)
    , HasPublicKey (..)
    , HasTaskId (..)
    , HasUidsToNotify (..)
    , Initial
    , seed
    )
import Web.Todoist.Util.QueryParam (QueryParam (..))

import Control.Applicative ((<$>))
import Control.Monad (Monad)
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value
    , genericToJSON
    , object
    )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Bool (Bool (False, True), not)
import Data.Eq (Eq)
import Data.Foldable (null)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List (filter)
import qualified Data.List as L
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Tuple as L
import GHC.Generics (Generic)
import Text.Show (Show (..))
import Web.Todoist.Domain.Types (Attachment, Content (..), ProjectId (..), TaskId (..), Uid)

-- | Newtype wrapper for Comment ID
newtype CommentId = CommentId {getCommentId :: Text}
    deriving (Show, Eq, Generic)

instance ToJSON CommentId where
    toJSON :: CommentId -> Value
    toJSON (CommentId txt) = toJSON txt

instance FromJSON CommentId where
    parseJSON :: Value -> Parser CommentId
    parseJSON v = CommentId <$> parseJSON v

-- | Comment domain type
data Comment = Comment
    { _id :: CommentId
    , _content :: Content
    , _poster_id :: Maybe Uid
    , _posted_at :: Maybe Uid
    , _task_id :: Maybe TaskId
    , _project_id :: Maybe ProjectId
    , _attachment :: Maybe FileAttachment
    }
    deriving (Show, Eq, Generic)

-- | Request body for creating a comment
data CommentCreate = CommentCreate
    { _content :: Content
    , _project_id :: Maybe ProjectId
    , _task_id :: Maybe TaskId
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
                , ("project_id", maybe Aeson.Null (Aeson.toJSON . getProjectId) _project_id)
                , ("task_id", maybe Aeson.Null (Aeson.toJSON . getTaskId) _task_id)
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
                { _content = Content truncated
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

-- | Create new CommentParam for use with builder pattern
newCommentParam :: Initial CommentParam
newCommentParam =
    seed $
        CommentParam
            { project_id = Nothing
            , task_id = Nothing
            , cursor = Nothing
            , limit = Nothing
            , public_key = Nothing
            }

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
    updateComment :: CommentUpdate -> CommentId -> m Comment

    -- | Delete a comment
    deleteComment :: CommentId -> m ()

-- Builder pattern instances
instance HasContent CommentCreate where
    hasContent :: Text -> CommentCreate -> CommentCreate
    hasContent content CommentCreate {..} = CommentCreate {_content = Content content, ..}

instance HasProjectId CommentCreate where
    hasProjectId :: Text -> CommentCreate -> CommentCreate
    hasProjectId pid CommentCreate {..} = CommentCreate {_project_id = Just (ProjectId pid), ..}

instance HasTaskId CommentCreate where
    hasTaskId :: Text -> CommentCreate -> CommentCreate
    hasTaskId tid CommentCreate {..} = CommentCreate {_task_id = Just (TaskId tid), ..}

instance HasAttachment CommentCreate where
    hasAttachment :: Attachment -> CommentCreate -> CommentCreate
    hasAttachment att CommentCreate {..} = CommentCreate {_attachment = Just att, ..}

instance HasUidsToNotify CommentCreate where
    hasUidsToNotify :: [Int] -> CommentCreate -> CommentCreate
    hasUidsToNotify uids CommentCreate {..} = CommentCreate {_uids_to_notify = uids, ..}

-- HasX instances for CommentParam
instance HasProjectId CommentParam where
    hasProjectId :: Text -> CommentParam -> CommentParam
    hasProjectId pid CommentParam {..} = CommentParam {project_id = Just pid, ..}

instance HasTaskId CommentParam where
    hasTaskId :: Text -> CommentParam -> CommentParam
    hasTaskId tid CommentParam {..} = CommentParam {task_id = Just tid, ..}

instance HasCursor CommentParam where
    hasCursor :: Text -> CommentParam -> CommentParam
    hasCursor c CommentParam {..} = CommentParam {cursor = Just c, ..}

instance HasLimit CommentParam where
    hasLimit :: Int -> CommentParam -> CommentParam
    hasLimit l CommentParam {..} = CommentParam {limit = Just l, ..}

instance HasPublicKey CommentParam where
    hasPublicKey :: Text -> CommentParam -> CommentParam
    hasPublicKey pk CommentParam {..} = CommentParam {public_key = Just pk, ..}
