{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Todoist.Runner.Trace
    ( Op (..)
    , Trace (..)
    ) where

import Web.Todoist.Domain.Project
    ( Collaborator (Collaborator)
    , Project
    , ProjectCreate
    , ProjectId (ProjectId)
    , TodoistProjectM (..)
    )
import Web.Todoist.Domain.Task
    ( Task
    , TaskId
    , TaskParam
    , TodoistTaskM (..)
    )

import Control.Applicative (Applicative, pure)
import Control.Monad (Functor, Monad)
import Control.Monad.Trans.Writer (Writer, tell)
import Data.Function (($))
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
