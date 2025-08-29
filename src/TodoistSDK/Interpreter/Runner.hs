
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module TodoistSDK.Interpreter.Runner (
  todoistIORunner,
  newTodoistEnv,
  todoistTraceRunner,
  runTodoistWith,
  TodoistEnv,
  Token
) where

import TodoistSDK.Interpreter.TodoistIO
import TodoistSDK.Interpreter.Types
import TodoistSDK.Interpreter.Trace

import Data.Text
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer (execWriter)
import Data.Kind
import TodoistSDK.Capabilities ( TodoistProjectM(..) )

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