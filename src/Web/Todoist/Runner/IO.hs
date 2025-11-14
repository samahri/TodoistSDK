{-# LANGUAGE DerivingStrategies #-}

-- | Main IO module - re-exports the TodoistIO type and brings instances into scope
module Web.Todoist.Runner.IO
    ( TodoistConfig (..)
    , TodoistIO (..)
    ) where

import Web.Todoist.Internal.Config (TodoistConfig (..))

-- Re-export TodoistIO from Core
import Web.Todoist.Runner.IO.Core (TodoistIO (..))

-- Import Interpreters module to bring all instances into scope
import Web.Todoist.Runner.IO.Interpreters ()
