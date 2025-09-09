{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Todoist.Project (
  TodoistProjectM(..),
  ProjectId (..),
  Collaborator(..),
  -- defaultProject
) where

import Data.String (String)
import Data.Int (Int)
import Text.Show (Show)
import Control.Monad (Monad)
import Data.Text ( Text )
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, ToJSON )

import Web.Todoist.Patch

newtype ProjectId = ProjectId {
  id :: Text
} deriving (Show, Generic, FromJSON, ToJSON) 

data Collaborator = Collaborator {
  id :: Text,
  name :: Text,
  email :: Text
} deriving (Show, Generic, FromJSON, ToJSON)

data ParentId = ParentIdStr String | ParentIdInt Int deriving (Show, Generic, FromJSON, ToJSON)



class Monad m => TodoistProjectM m where
  -- todo write algebraic laws
  getAllProjects :: m [ProjectId]
  -- (Optional) getProject :: ProjectId -> m Project

  getProjectCollaborators :: ProjectId -> m [Collaborator]

  -- todo: separate domain types from api types
  addProject :: ProjectCreate -> m ProjectId
