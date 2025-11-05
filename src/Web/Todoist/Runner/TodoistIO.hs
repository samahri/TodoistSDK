{-# LANGUAGE DerivingStrategies #-}

-- | Main TodoistIO module - re-exports the TodoistIO type and brings instances into scope
module Web.Todoist.Runner.TodoistIO
    ( TodoistConfig (..)
    , TodoistIO (..)
    , projectResponseToProject
    ) where

import Web.Todoist.Internal.Config (TodoistConfig (..))

-- Re-export TodoistIO from Core
import Web.Todoist.Runner.TodoistIO.Core (TodoistIO (..))

-- Import submodules to bring instances into scope
import Web.Todoist.Runner.TodoistIO.Comment ()
import Web.Todoist.Runner.TodoistIO.Label ()
import Web.Todoist.Runner.TodoistIO.Project (projectResponseToProject)
import Web.Todoist.Runner.TodoistIO.Section ()
import Web.Todoist.Runner.TodoistIO.Task ()
