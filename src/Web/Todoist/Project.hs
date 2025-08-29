module Web.Todoist.Project (
  TodoistProjectM,
  ProjectId (..),
  getAllProjects
) where

-- TODO: use Text
newtype ProjectId = ProjectId String deriving (Show)

class Monad m => TodoistProjectM m where
  -- todo write algebraic laws
  getAllProjects :: m [ProjectId]
  -- (Optional) getProject :: ProjectId -> m Project

  -- getProjectCollaborators :: ProjectId -> 
