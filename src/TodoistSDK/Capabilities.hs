module TodoistSDK.Capabilities (
  TodoistProjectM,
  getAllProjects
) where

import TodoistSDK.Types ( ProjectId )  

class Monad m => TodoistProjectM m where
  -- todo write algebraic laws
  getAllProjects :: m [ProjectId]
  -- (Optional) getProject :: ProjectId -> m Project
