{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Todoist.Runner.Trace
    ( Op (..)
    , Trace (..)
    ) where

import Web.Todoist.Patch
import Web.Todoist.Domain.Project
import Web.Todoist.Domain.Task

import Control.Applicative (Applicative, pure)
import Control.Monad (Functor, Monad)
import Control.Monad.Trans.Writer (Writer, tell)
import Data.Function (($))
import Text.Show (Show)

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
    getAllProjects :: Trace [ProjectId]
    getAllProjects = Trace $ do
        tell [ProjectOp GetAllProjects]
        pure [ProjectId ""]

    getProjectCollaborators :: ProjectId -> Trace [Collaborator]
    getProjectCollaborators _ = Trace $ do
        tell [ProjectOp GetProjectCollaborators]
        pure [Collaborator "" "" ""]

    addProject :: ProjectCreate -> Trace ProjectId
    addProject _ = Trace $ do
        tell [ProjectOp AddProject]
        pure $ ProjectId ""

instance TodoistTaskM Trace where
    getTasks :: TaskParam -> Trace [TaskId]
    getTasks _ = Trace $ do
        tell [TaskOp GetTasks]
        pure [TaskId ""]

    getTask :: TaskId -> Trace Task
    getTask tid = Trace $ do
        tell [TaskOp GetTasks]
        pure $ Task tid ""

    addTask :: TaskCreate -> Trace NewTask
    addTask _ = Trace $ do
        tell [TaskOp AddTask]
        pure emptyTask

    updateTask :: TaskId -> TaskPatch -> Trace NewTask
    updateTask _ _ = Trace $ do
        tell [TaskOp UpdateTask]
        pure emptyTask

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
