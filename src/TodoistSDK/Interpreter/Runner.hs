
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module TodoistSDK.Interpreter.Runner (
  runTodoist,
  newTodoistEnv,
  runTodoistWith,
  TodoistEnv,
  Token
) where

import TodoistSDK.Capabilities ( TodoistProjectM(..) )
import TodoistSDK.Interpreter.TodoistIO
import TodoistSDK.Interpreter.Types

import Data.Kind
import Data.Text
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

newTodoistEnv :: Text -> TodoistEnv
newTodoistEnv token = TodoistEnv {authToken = Token token, baseUrl = "https://api.todoist.com"}

runTodoist :: TodoistEnv -> TodoistIO a -> IO (Either TodoistError a)
runTodoist env operations = runExceptT $ runReaderT (unTodoist operations) env

runTodoistWith :: TodoistEnv -> (forall (m :: Type -> Type). TodoistProjectM m => m a) -> IO (Either TodoistError a)
runTodoistWith env operations = runExceptT $ runReaderT (unTodoist operations) env