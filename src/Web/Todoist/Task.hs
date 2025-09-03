{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Web.Todoist.Task (
  TodoistTaskM,
  TaskId(..),
  getTasks,

) where

import Data.String (String)
import Text.Show (Show)
import Control.Monad (Monad)
import Data.Text
import GHC.Generics (Generic)
import Data.Aeson

-- TODO: use Text
newtype TaskId = TaskId {
  id :: String
} deriving (Show, Generic, FromJSON, ToJSON)


class Monad m => TodoistTaskM m where
  -- todo write algebraic laws
  getTasks :: m [TaskId]

