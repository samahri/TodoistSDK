module TodoistSDK.Types (
  ProjectId (..)
) where

-- TODO: use Text
newtype ProjectId = ProjectId String deriving (Show)