{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Web.Todoist.Param (
  TaskParam,
  emptyParams,
  QueryParam(..),
  setProjectId
) where

import Data.Maybe
import Web.Todoist.Runner.HttpClient (Params)
import Data.Text
import qualified Data.List as L
import Data.Monoid ((<>))

-- type Params = [(Text, Text)] -- also in Web.Todoist.Runner.HttpClient

class QueryParam a where
  toQueryParam :: a -> Params

data TaskParam = TaskParam {
  project_id :: Maybe Text,
  section_id :: Maybe Text,
  parent_id :: Maybe Text,
  task_ids :: [Text]
}

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

setProjectId :: Text -> TaskParam -> TaskParam
setProjectId pid taskparam = taskparam { project_id = Just pid}

