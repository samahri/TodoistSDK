{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Todoist.Runner.Trace
    ( Op (..)
    , Trace (..)
    ) where

import Web.Todoist.Domain.Comment
    ( Comment (..)
    , CommentCreate
    , CommentId (..)
    , CommentParam
    , CommentUpdate
    , Content (..)
    , TodoistCommentM (..)
    )
import Web.Todoist.Domain.Label
    ( Label (..)
    , LabelCreate
    , LabelId (LabelId)
    , LabelParam
    , LabelUpdate
    , SharedLabelParam
    , SharedLabelRemove
    , SharedLabelRename
    , TodoistLabelM (..)
    )
import Web.Todoist.Domain.Project
    ( Collaborator (Collaborator)
    , Project
    , ProjectCreate
    , TodoistProjectM (..)
    )
import Web.Todoist.Domain.Section
    ( Section (..)
    , SectionCreate
    , SectionId (SectionId)
    , SectionParam
    , SectionUpdate
    , TodoistSectionM (..)
    )
import Web.Todoist.Domain.Task
    ( Task
    , TaskParam
    , TodoistTaskM (..)
    )
import Web.Todoist.Domain.Types (ProjectId (ProjectId), TaskId)

import Control.Applicative (Applicative, pure)
import Control.Monad (Functor, Monad)
import Control.Monad.Trans.Writer (Writer, tell)
import Data.Bool (Bool (False))
import Data.Function (($))
import Data.Maybe (Maybe (Nothing))
import Data.Text (Text)
import Text.Show (Show)

-- emptyTask :: NewTask
-- emptyTask =
--     NewTask
--         { _user_id = ""
--         , _id = ""
--         , _project_id = ""
--         , _section_id = Nothing
--         , _parent_id = Nothing
--         , _added_by_uid = Nothing
--         , _assigned_by_uid = Nothing
--         , _responsible_uid = Nothing
--         , _labels = []
--         , _checked = False
--         , _is_deleted = False
--         , added_at = Nothing
--         , _completed_at = Nothing
--         , _updated_at = Nothing
--         , _priority = 0
--         , _child_order = 0
--         , _content = ""
--         , _description = ""
--         , _note_count = 0
--         , _day_order = 0
--         , _is_collapsed = False
--         }

data Op
    = ProjectOp ProjectOp
    | TaskOp TaskOp
    | CommentOp CommentOp
    | SectionOp SectionOp
    | LabelOp LabelOp
    deriving (Show)

data ProjectOp
    = GetAllProjects
    | GetProjectCollaborators
    | AddProject
    deriving (Show)

data TaskOp
    = GetTasks
    | GetTask
    | AddTask
    | UpdateTask
    | CloseTask
    | DeleteTask
    | UncloseTask
    deriving (Show)

data CommentOp
    = AddComment
    | GetComments
    | GetCommentsPaginated
    | GetComment
    | UpdateComment
    | DeleteComment
    deriving (Show)

data SectionOp
    = GetSections
    | GetSection
    | AddSection
    | UpdateSection
    | DeleteSection
    | GetSectionsPaginated
    deriving (Show)

data LabelOp
    = GetLabels
    | GetLabel
    | AddLabel
    | UpdateLabel
    | DeleteLabel
    | GetLabelsPaginated
    | GetSharedLabels
    | GetSharedLabelsPaginated
    | RemoveSharedLabels
    | RenameSharedLabels
    deriving (Show)

newtype Trace a = Trace {runTrace :: Writer [Op] a}
    deriving (Functor, Applicative, Monad)

instance TodoistProjectM Trace where
    getAllProjects :: Trace [Project]
    getAllProjects = Trace $ do
        tell [ProjectOp GetAllProjects]
        pure []

    getProjectCollaborators :: ProjectId -> Trace [Collaborator]
    getProjectCollaborators _ = Trace $ do
        tell [ProjectOp GetProjectCollaborators]
        pure [Collaborator "" "" ""]

    addProject :: ProjectCreate -> Trace ProjectId
    addProject _ = Trace $ do
        tell [ProjectOp AddProject]
        pure $ ProjectId ""

instance TodoistTaskM Trace where
    getTasks :: TaskParam -> Trace [Task]
    getTasks _ = Trace $ do
        tell [TaskOp GetTasks]
        pure []

    -- getTask :: TaskId -> Trace Task
    -- getTask tid = Trace $ do
    --     tell [TaskOp GetTasks]
    --     pure $ Task
    --         { _id = tid
    --         , _content = ""
    --         , _description = ""
    --         , _project_id = ""
    --         , _section_id = Nothing
    --         , _parent_id = Nothing
    --         , _labels = Nothing
    --         , _priority = 0
    --         , _due = Nothing
    --         , _deadline = Nothing
    --         , _duration = Nothing
    --         , _is_collapsed = False
    --         , _order = 0
    --         , _assignee_id = Nothing
    --         , _assigner_id = Nothing
    --         , _completed_at = Nothing
    --         , _creator_id = ""
    --         , _created_at = ""
    --         , _updated_at = ""
    --         }

    -- addTask :: TaskCreate -> Trace NewTask
    -- addTask _ = Trace $ do
    --     tell [TaskOp AddTask]
    --     pure emptyTask

    -- updateTask :: TaskId -> TaskPatch -> Trace NewTask
    -- updateTask _ _ = Trace $ do
    --     tell [TaskOp UpdateTask]
    --     pure emptyTask

    closeTask :: TaskId -> Trace ()
    closeTask _ = Trace $ do
        tell [TaskOp CloseTask]
        pure ()

    uncloseTask :: TaskId -> Trace ()
    uncloseTask _ = Trace $ do
        tell [TaskOp UncloseTask]
        pure ()

    deleteTask :: TaskId -> Trace ()
    deleteTask _ = Trace $ do
        tell [TaskOp DeleteTask]
        pure ()

instance TodoistCommentM Trace where
    addComment :: CommentCreate -> Trace Comment
    addComment _ = Trace $ do
        tell [CommentOp AddComment]
        pure $ Comment (CommentId "") (Content "") Nothing Nothing Nothing Nothing Nothing

    getComments :: CommentParam -> Trace [Comment]
    getComments _ = Trace $ do
        tell [CommentOp GetComments]
        pure []

    getCommentsPaginated :: CommentParam -> Trace ([Comment], Maybe Text)
    getCommentsPaginated _ = Trace $ do
        tell [CommentOp GetCommentsPaginated]
        pure ([], Nothing)

    getComment :: CommentId -> Trace Comment
    getComment _ = Trace $ do
        tell [CommentOp GetComment]
        pure $ Comment (CommentId "") (Content "") Nothing Nothing Nothing Nothing Nothing

    updateComment :: CommentId -> CommentUpdate -> Trace Comment
    updateComment _ _ = Trace $ do
        tell [CommentOp UpdateComment]
        pure $ Comment (CommentId "") (Content "") Nothing Nothing Nothing Nothing Nothing

    deleteComment :: CommentId -> Trace ()
    deleteComment _ = Trace $ do
        tell [CommentOp DeleteComment]
        pure ()

instance TodoistSectionM Trace where
    getSections :: SectionParam -> Trace [Section]
    getSections _ = Trace $ do
        tell [SectionOp GetSections]
        pure []

    getSection :: SectionId -> Trace Section
    getSection _ = Trace $ do
        tell [SectionOp GetSection]
        pure $ Section "" "" "" False 0

    addSection :: SectionCreate -> Trace SectionId
    addSection _ = Trace $ do
        tell [SectionOp AddSection]
        pure $ SectionId ""

    updateSection :: SectionId -> SectionUpdate -> Trace Section
    updateSection _ _ = Trace $ do
        tell [SectionOp UpdateSection]
        pure $ Section "" "" "" False 0

    deleteSection :: SectionId -> Trace ()
    deleteSection _ = Trace $ do
        tell [SectionOp DeleteSection]
        pure ()

    getSectionsPaginated :: SectionParam -> Trace ([Section], Maybe Text)
    getSectionsPaginated _ = Trace $ do
        tell [SectionOp GetSectionsPaginated]
        pure ([], Nothing)

instance TodoistLabelM Trace where
    getLabels :: LabelParam -> Trace [Label]
    getLabels _ = Trace $ do
        tell [LabelOp GetLabels]
        pure []

    getLabel :: LabelId -> Trace Label
    getLabel _ = Trace $ do
        tell [LabelOp GetLabel]
        pure $ Label "" "" "" Nothing False

    addLabel :: LabelCreate -> Trace LabelId
    addLabel _ = Trace $ do
        tell [LabelOp AddLabel]
        pure $ LabelId ""

    updateLabel :: LabelId -> LabelUpdate -> Trace Label
    updateLabel _ _ = Trace $ do
        tell [LabelOp UpdateLabel]
        pure $ Label "" "" "" Nothing False

    deleteLabel :: LabelId -> Trace ()
    deleteLabel _ = Trace $ do
        tell [LabelOp DeleteLabel]
        pure ()

    getLabelsPaginated :: LabelParam -> Trace ([Label], Maybe Text)
    getLabelsPaginated _ = Trace $ do
        tell [LabelOp GetLabelsPaginated]
        pure ([], Nothing)

    getSharedLabels :: SharedLabelParam -> Trace [Text]
    getSharedLabels _ = Trace $ do
        tell [LabelOp GetSharedLabels]
        pure []

    getSharedLabelsPaginated :: SharedLabelParam -> Trace ([Text], Maybe Text)
    getSharedLabelsPaginated _ = Trace $ do
        tell [LabelOp GetSharedLabelsPaginated]
        pure ([], Nothing)

    removeSharedLabels :: SharedLabelRemove -> Trace ()
    removeSharedLabels _ = Trace $ do
        tell [LabelOp RemoveSharedLabels]
        pure ()

    renameSharedLabels :: SharedLabelRename -> Trace ()
    renameSharedLabels _ = Trace $ do
        tell [LabelOp RenameSharedLabels]
        pure ()
