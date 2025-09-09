
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Todoist.Runner (
  todoist,
  newTodoistConfig,
  todoistTraceRunner,
  runTodoistWith,
) where

import Web.Todoist.Runner.TodoistIO
import Web.Todoist.Runner.Trace
import Web.Todoist.Runner.HttpClient
import Web.Todoist.Task
import Web.Todoist.Project

import Data.Either ( Either )
import Data.Text ( Text )
import Control.Monad.Trans.Reader ( ReaderT(runReaderT) )
import Control.Monad.Trans.Except ( runExceptT )
import Control.Monad.Trans.Writer (execWriter)
import System.IO ( IO ) 
import Control.Applicative ( Applicative(pure) )
import Data.Function ((.))

newTodoistConfig :: Text -> TodoistConfig
newTodoistConfig token = TodoistConfig { authToken = Token token }


class (TodoistProjectM m, TodoistTaskM m) => MonadTodoist m
instance (TodoistProjectM m, TodoistTaskM m) => MonadTodoist m

todoist :: TodoistConfig -> TodoistIO a -> IO (Either TodoistError a)
todoist env operations = runExceptT (runReaderT (unTodoist operations) env)

todoistTraceRunner :: TodoistConfig -> Trace a -> IO (Either TodoistError [Op])
todoistTraceRunner _  = pure . pure . execWriter . runTrace

class MonadTodoist r => TodoistRunner r where
  type Output r a -- Associated Type Families
  runTodoistWith :: TodoistConfig -> r a -> IO (Either TodoistError (Output r a))

instance TodoistRunner TodoistIO where
  type Output TodoistIO a = a

  runTodoistWith :: TodoistConfig -> TodoistIO a -> IO (Either TodoistError a)
  runTodoistWith = todoist

instance TodoistRunner Trace where
  type Output Trace a = [Op]

  runTodoistWith :: TodoistConfig -> Trace a -> IO (Either TodoistError [Op])
  runTodoistWith = todoistTraceRunner