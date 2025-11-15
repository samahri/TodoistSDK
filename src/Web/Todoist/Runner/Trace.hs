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
    ( CanAssignTasks (..)
    , Collaborator (Collaborator)
    , IsArchived (..)
    , IsShared (..)
    , PaginationParam
    , Project (..)
    , ProjectCreate
    , ProjectUpdate
    , TodoistProjectM (..)
    )
import Web.Todoist.Internal.Types (ProjectPermissions (ProjectPermissions))
import Web.Todoist.Domain.Section
    ( Section (..)
    , SectionCreate
    , SectionId (SectionId)
    , SectionParam
    , SectionUpdate
    , TodoistSectionM (..)
    )
import Web.Todoist.Domain.Task
    ( AddTaskQuick
    , CompletedTasksQueryParam
    , MoveTask
    , NewTask (..)
    , Task (..)
    , TaskCreate
    , TaskFilter
    , TaskParam
    , TaskPatch
    , TodoistTaskM (..)
    )
import Web.Todoist.Domain.Types
    ( Color (..)
    , Description (..)
    , IsCollapsed (..)
    , IsFavorite (..)
    , Name (..)
    , Order (..)
    , ProjectId (..)
    , TaskId (..)
    , Uid (..)
    , ViewStyle (..)
    )

import Control.Applicative (Applicative, pure)
import Control.Monad (Functor, Monad)
import Control.Monad.Trans.Writer (Writer, tell)
import Data.Bool (Bool (False))
import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe (Nothing))
import Data.Text (Text)
import Text.Show (Show)

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

    getProject :: ProjectId -> Trace Project
    getProject _ = Trace $ do
        tell [ProjectOp GetAllProjects]
        pure $ Project (ProjectId "") (Name "") (Description "") (Order 0) (Color "") (IsCollapsed False) (IsShared False) (IsFavorite False) (IsArchived False) (CanAssignTasks False) List Nothing Nothing

    getProjectCollaborators :: ProjectId -> Trace [Collaborator]
    getProjectCollaborators _ = Trace $ do
        tell [ProjectOp GetProjectCollaborators]
        pure [Collaborator "" (Name "") ""]

    addProject :: ProjectCreate -> Trace ProjectId
    addProject _ = Trace $ do
        tell [ProjectOp AddProject]
        pure $ ProjectId ""

    deleteProject :: ProjectId -> Trace ()
    deleteProject _ = Trace $ do
        tell [ProjectOp AddProject]
        pure ()

    archiveProject :: ProjectId -> Trace ProjectId
    archiveProject pid = Trace $ do
        tell [ProjectOp AddProject]
        pure pid

    unarchiveProject :: ProjectId -> Trace ProjectId
    unarchiveProject pid = Trace $ do
        tell [ProjectOp AddProject]
        pure pid

    getProjectPermissions :: Trace ProjectPermissions
    getProjectPermissions = Trace $ do
        tell [ProjectOp GetAllProjects]
        pure $ ProjectPermissions [] []

    updateProject :: ProjectUpdate -> ProjectId -> Trace Project
    updateProject _ _ = Trace $ do
        tell [ProjectOp AddProject]
        pure $ Project (ProjectId "") (Name "") (Description "") (Order 0) (Color "") (IsCollapsed False) (IsShared False) (IsFavorite False) (IsArchived False) (CanAssignTasks False) List Nothing Nothing

    getAllProjectsPaginated :: PaginationParam -> Trace ([Project], Maybe Text)
    getAllProjectsPaginated _ = Trace $ do
        tell [ProjectOp GetAllProjects]
        pure ([], Nothing)

    getProjectCollaboratorsPaginated :: PaginationParam -> ProjectId -> Trace ([Collaborator], Maybe Text)
    getProjectCollaboratorsPaginated _ _ = Trace $ do
        tell [ProjectOp GetProjectCollaborators]
        pure ([], Nothing)

    getAllProjectsWithLimit :: Int -> Trace [Project]
    getAllProjectsWithLimit _ = Trace $ do
        tell [ProjectOp GetAllProjects]
        pure []

    getProjectCollaboratorsWithLimit :: Int -> ProjectId -> Trace [Collaborator]
    getProjectCollaboratorsWithLimit _ _ = Trace $ do
        tell [ProjectOp GetProjectCollaborators]
        pure []

instance TodoistTaskM Trace where
    getTasks :: TaskParam -> Trace [Task]
    getTasks _ = Trace $ do
        tell [TaskOp GetTasks]
        pure []

    getTask :: TaskId -> Trace Task
    getTask tid = Trace $ do
        tell [TaskOp GetTask]
        pure $ Task tid (Content "") (Description "") (ProjectId "") Nothing Nothing [] 0 Nothing Nothing Nothing (IsCollapsed False) (Order 0) Nothing Nothing Nothing (Uid "") "" ""

    addTask :: TaskCreate -> Trace NewTask
    addTask _ = Trace $ do
        tell [TaskOp AddTask]
        pure $ NewTask "" (TaskId "") (ProjectId "") Nothing Nothing Nothing Nothing Nothing [] False False Nothing Nothing Nothing 0 (Order 0) (Content "") (Description "") 0 (Order 0) (IsCollapsed False)

    updateTask :: TaskPatch -> TaskId -> Trace NewTask
    updateTask _ _ = Trace $ do
        tell [TaskOp UpdateTask]
        pure $ NewTask "" (TaskId "") (ProjectId "") Nothing Nothing Nothing Nothing Nothing [] False False Nothing Nothing Nothing 0 (Order 0) (Content "") (Description "") 0 (Order 0) (IsCollapsed False)

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

    getTasksByFilter :: TaskFilter -> Trace [TaskId]
    getTasksByFilter _ = Trace $ do
        tell [TaskOp GetTasks]
        pure []

    moveTask :: MoveTask -> TaskId -> Trace TaskId
    moveTask _ tid = Trace $ do
        tell [TaskOp UpdateTask]
        pure tid

    addTaskQuick :: AddTaskQuick -> Trace ()
    addTaskQuick _ = Trace $ do
        tell [TaskOp AddTask]
        pure ()

    getCompletedTasksByDueDate :: CompletedTasksQueryParam -> Trace [TaskId]
    getCompletedTasksByDueDate _ = Trace $ do
        tell [TaskOp GetTasks]
        pure []

    getCompletedTasksByCompletionDate :: CompletedTasksQueryParam -> Trace [TaskId]
    getCompletedTasksByCompletionDate _ = Trace $ do
        tell [TaskOp GetTasks]
        pure []

    getTasksPaginated :: TaskParam -> Trace ([TaskId], Maybe Text)
    getTasksPaginated _ = Trace $ do
        tell [TaskOp GetTasks]
        pure ([], Nothing)

    getTasksByFilterPaginated :: TaskFilter -> Trace ([TaskId], Maybe Text)
    getTasksByFilterPaginated _ = Trace $ do
        tell [TaskOp GetTasks]
        pure ([], Nothing)

    getTasksWithLimit :: Int -> TaskParam -> Trace [TaskId]
    getTasksWithLimit _ _ = Trace $ do
        tell [TaskOp GetTasks]
        pure []

    getTasksByFilterWithLimit :: TaskFilter -> Int -> Trace [TaskId]
    getTasksByFilterWithLimit _ _ = Trace $ do
        tell [TaskOp GetTasks]
        pure []

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

    updateComment :: CommentUpdate -> CommentId -> Trace Comment
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
        pure $ Section (SectionId "") (Name "") (ProjectId "") (IsCollapsed False) (Order 0)

    addSection :: SectionCreate -> Trace SectionId
    addSection _ = Trace $ do
        tell [SectionOp AddSection]
        pure $ SectionId ""

    updateSection :: SectionUpdate -> SectionId -> Trace Section
    updateSection _ _ = Trace $ do
        tell [SectionOp UpdateSection]
        pure $ Section (SectionId "") (Name "") (ProjectId "") (IsCollapsed False) (Order 0)

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
        pure $ Label (LabelId "") (Name "") (Color "") Nothing (IsFavorite False)

    addLabel :: LabelCreate -> Trace LabelId
    addLabel _ = Trace $ do
        tell [LabelOp AddLabel]
        pure $ LabelId ""

    updateLabel :: LabelUpdate -> LabelId -> Trace Label
    updateLabel _ _ = Trace $ do
        tell [LabelOp UpdateLabel]
        pure $ Label (LabelId "") (Name "") (Color "") Nothing (IsFavorite False)

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
