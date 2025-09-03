{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Web.Todoist.Runner.Trace (
  Op(..),
  Trace(..)
) where

import Web.Todoist.Project
import Web.Todoist.Task
import Control.Applicative (Applicative, pure)
import Control.Monad (Functor, Monad)
import Control.Monad.Trans.Writer (Writer, tell)
import Text.Show (Show)
import Data.Function (($))
import Web.Todoist.Project (Collaborator, ProjectId)

data Op = GetAllProjects
          | GetProjectCollaborators
          | GetTasks deriving (Show)

newtype Trace a = Trace { runTrace :: Writer [Op] a }
  deriving (Functor, Applicative, Monad)

instance TodoistProjectM Trace where
  getAllProjects :: Trace [ProjectId]
  getAllProjects = Trace $ do
    tell [GetAllProjects]
    pure [ProjectId ""]

  getProjectCollaborators :: ProjectId -> Trace [Collaborator]
  getProjectCollaborators _ = Trace $ do
    tell [GetProjectCollaborators]
    pure [Collaborator "" "" ""]

instance TodoistTaskM Trace where
  -- todo write algebraic laws
  getTasks :: Trace [TaskId]
  getTasks = Trace $ do
    tell [GetTasks]
    pure [TaskId ""]