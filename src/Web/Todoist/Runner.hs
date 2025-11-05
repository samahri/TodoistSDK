{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Todoist.Runner
    ( todoist
    , newTodoistConfig
    , todoistTraceRunner
    , runTodoistWith
    , MonadTodoist
    ) where

import Web.Todoist.Domain.Comment (TodoistCommentM)
import Web.Todoist.Domain.Project (TodoistProjectM)
import Web.Todoist.Domain.Section (TodoistSectionM)
import Web.Todoist.Domain.Task (TodoistTaskM)
import Web.Todoist.Internal.Config (Token (..))
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Runner.TodoistIO
    ( TodoistConfig (..)
    , TodoistIO (unTodoist)
    )
import Web.Todoist.Runner.Trace (Op, Trace (runTrace))

import Control.Applicative (Applicative (pure))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Writer (execWriter)
import Data.Either (Either)
import Data.Function ((.))
import Data.Text (Text)
import System.IO (IO)

newTodoistConfig :: Text -> TodoistConfig
newTodoistConfig token = TodoistConfig {authToken = Token token}

class (TodoistProjectM m, TodoistTaskM m, TodoistCommentM m, TodoistSectionM m) => MonadTodoist m
instance (TodoistProjectM m, TodoistTaskM m, TodoistCommentM m, TodoistSectionM m) => MonadTodoist m

todoist :: TodoistConfig -> TodoistIO a -> IO (Either TodoistError a)
todoist env operations = runExceptT (runReaderT (unTodoist operations) env)

todoistTraceRunner :: TodoistConfig -> Trace a -> IO (Either TodoistError [Op])
todoistTraceRunner _ = pure . pure . execWriter . runTrace

class (MonadTodoist r) => TodoistRunner r where
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
