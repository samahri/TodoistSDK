
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Web.Todoist.Runner (
  todoistIORunner,
  newTodoistEnv,
  todoistTraceRunner,
  runTodoistWith,
) where

import Web.Todoist.Runner.TodoistIO
import Web.Todoist.Runner.Trace
import Web.Todoist.Runner.HttpClient

import Data.Text
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer (execWriter)
import Data.Kind
import Web.Todoist.Project ( TodoistProjectM(..) )

newTodoistEnv :: Text -> TodoistEnv
newTodoistEnv token = TodoistEnv {authToken = Token token, baseUrl = "https://api.todoist.com"}

type TodoistSDKRunner m a r = TodoistProjectM m => TodoistEnv -> m a -> IO (Either TodoistError r)
runTodoistWith 
  :: forall (m :: Type -> Type) a r. TodoistProjectM m => 
  TodoistSDKRunner m a r
  -> TodoistEnv 
  -> m a 
  -> IO (Either TodoistError r)
runTodoistWith runner = runner

todoistIORunner :: TodoistEnv -> TodoistIO a -> IO (Either TodoistError a)
todoistIORunner env operations = runExceptT (runReaderT (unTodoist operations) env)

todoistTraceRunner :: TodoistEnv -> Trace a -> IO (Either TodoistError [Op])
todoistTraceRunner _  = pure . pure . execWriter . runTrace