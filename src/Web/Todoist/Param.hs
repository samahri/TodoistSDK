{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Web.Todoist.Param (
  TaskParam,
  emptyParams,
  QueryParam(..),
  TaskFilter,
  taskFilterWithQuery,
  setProjectId,
  CompletedTasksQueryParam,
  addTaskQuickWithQuery
) where

import Data.Maybe
import Web.Todoist.Runner.HttpClient (Params)
import Data.Text
import qualified Data.List as L
import Data.Monoid ((<>))
import Data.Int 
import Text.Show (Show)

class QueryParam a where
  toQueryParam :: a -> Params

data TaskParam = TaskParam {
  project_id :: Maybe Text,
  section_id :: Maybe Text,
  parent_id :: Maybe Text,
  task_ids :: [Text]
} deriving Show

instance QueryParam TaskParam where
  toQueryParam :: TaskParam -> Params
  toQueryParam TaskParam{..} = 
    maybe [] (\projId -> [("project_id", projId)]) project_id
    <> maybe [] (\secId -> [("section_id", secId)]) section_id
    <> maybe [] (\parId -> [("parent_id", parId)]) parent_id
    <> L.map ("task_id",) task_ids

emptyParams :: TaskParam
emptyParams = TaskParam {
  project_id = Nothing,
  section_id = Nothing,
  parent_id = Nothing,
  task_ids = []
} 

data TaskFilter = TaskFilter {
  query :: Text,
  lang :: Maybe Text,
  cursor :: Maybe Text,
  limit :: Maybe Int
} deriving Show

instance QueryParam TaskFilter where
  toQueryParam :: TaskFilter -> Params
  toQueryParam TaskFilter{..} = 
    [("query", query)]
    <> maybe [] (\p -> [("lang", p)]) lang
    <> maybe [] (\p -> [("cursor", p)]) cursor
    <> maybe [] (\p -> [("limit", show p)]) limit

taskFilterWithQuery :: Text -> TaskFilter
taskFilterWithQuery query = TaskFilter {
  query,
  lang = Nothing,
  cursor = Nothing,
  limit = Nothing
}

data CompletedTasksQueryParam = CompletedTasksQueryParam {
  since :: Text,
  until :: Text,
  workspace_id :: Maybe Text,
  project_id :: Maybe Text,
  section_id :: Maybe Text,
  parent_id :: Maybe Text,
  filter_query :: Maybe Text,
  filter_lang :: Maybe Text,
  cursor :: Maybe Text,
  limit :: Int
} deriving Show

instance QueryParam CompletedTasksQueryParam where
  toQueryParam :: CompletedTasksQueryParam -> Params
  toQueryParam CompletedTasksQueryParam{..} = 
    [("since", since)]
    <> [("until", until)]
    <> maybe [] (\p -> [("workspace_id", p)]) workspace_id
    <> maybe [] (\p -> [("project_id", p)]) project_id
    <> maybe [] (\p -> [("section_id", p)]) section_id
    <> maybe [] (\p -> [("parent_id", p)]) parent_id
    <> maybe [] (\p -> [("filter_query", p)]) filter_query
    <> maybe [] (\p -> [("filter_lang", p)]) filter_lang
    <> maybe [] (\p -> [("cursor", p)]) cursor
    <> [("limit", show limit)]

addTaskQuickWithQuery :: Text -> Text -> CompletedTasksQueryParam
addTaskQuickWithQuery since until = CompletedTasksQueryParam {
  since,
  until,
  workspace_id  = Nothing,
  project_id = Nothing,
  section_id = Nothing,
  parent_id = Nothing,
  filter_query = Nothing,
  filter_lang = Nothing,
  cursor = Nothing,
  limit = 50
} 

setProjectId :: Text -> TaskParam -> TaskParam
setProjectId pid taskparam = taskparam { project_id = Just pid}

