module Web.Todoist.Patch (
  Project(..),
) where

import Data.String(String)
import Data.Maybe (Maybe)

type Name = String
type Description = String

data Project = Project {
  _name :: Name
  , _desc :: Maybe Description
  -- , _parentId :: Maybe ParentId
} 
-- deriving (Show, Generic, FromJSON)


