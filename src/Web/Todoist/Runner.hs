{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Web.Todoist.Runner
Description : Entry points for executing Todoist operations
Copyright   : (c) 2025 Sam S. Almahri
License     : MIT
Maintainer  : sam.salmahri@gmail.com

This module provides the main entry points for executing Todoist operations.
It defines the 'TodoistRunner' type class and provides convenience functions
for running operations with different interpreters.

= Usage Example

@
import Web.Todoist.Runner
import Web.Todoist.Domain.Project

main :: IO ()
main = do
    let config = newTodoistConfig "your-api-token"
    result <- todoist config getAllProjects
    case result of
        Left err -> print err
        Right projects -> print projects
@
-}
module Web.Todoist.Runner
    ( todoist
    , newTodoistConfig
    , todoistTraceRunner
    , runTodoistWith
    , MonadTodoist
    ) where

import Web.Todoist.Domain.Comment (TodoistCommentM)
import Web.Todoist.Domain.Label (TodoistLabelM)
import Web.Todoist.Domain.Project (TodoistProjectM)
import Web.Todoist.Domain.Section (TodoistSectionM)
import Web.Todoist.Domain.Task (TodoistTaskM)
import Web.Todoist.Internal.Config (Token (..))
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Runner.IO
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

{- | Create a new Todoist configuration with an API token

The token can be obtained from Todoist Settings → Integrations → Developer.
This configuration is used to authenticate all API requests.

Example:

@
let config = newTodoistConfig "your-api-token-here"
@
-}
newTodoistConfig :: Text -> TodoistConfig
newTodoistConfig token = TodoistConfig {authToken = Token token}

{- | Constraint synonym for operations requiring all domain capabilities

This type alias combines 'TodoistProjectM', 'TodoistTaskM', 'TodoistCommentM',
'TodoistSectionM', and 'TodoistLabelM' constraints, allowing functions to work
with all Todoist resources without listing all individual constraints.
-}
class
    (TodoistProjectM m, TodoistTaskM m, TodoistCommentM m, TodoistSectionM m, TodoistLabelM m) =>
    MonadTodoist m
instance
    (TodoistProjectM m, TodoistTaskM m, TodoistCommentM m, TodoistSectionM m, TodoistLabelM m) =>
    MonadTodoist m

{- | Execute Todoist operations using the TodoistIO interpreter

This is the primary function for making real HTTP requests to the Todoist API.
It handles authentication, error handling, and returns results in an Either type.

Example:

@
result <- todoist config getAllProjects
case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right projects -> mapM_ print projects
@
-}
todoist :: TodoistConfig -> TodoistIO a -> IO (Either TodoistError a)
todoist env operations = runExceptT (runReaderT (unTodoist operations) env)

{- | Execute operations with the Trace interpreter for testing

The Trace interpreter records operations without executing them, useful for
testing and debugging. Returns a list of recorded operations.

Example:

@
ops <- todoistTraceRunner config getAllProjects
print ops  -- [GetAllProjects]
@
-}
todoistTraceRunner :: TodoistConfig -> Trace a -> IO (Either TodoistError [Op])
todoistTraceRunner _ = pure . pure . execWriter . runTrace

{- | Type class for running operations with different interpreters

This class allows you to run Todoist operations with any interpreter
that implements the required type class methods. The 'Output' type family
determines what type of result is returned.

Use 'runTodoistWith' for custom interpreters or when you need explicit
interpreter selection.
-}
class (MonadTodoist r) => TodoistRunner r where
    type Output r a -- Associated Type Families

    {- | Execute operations with a custom interpreter

    This function allows you to run operations with any interpreter that implements
    'TodoistRunner'. Use this for testing with the Trace interpreter or implementing
    custom interpreters.

    Example with Trace:

    @
    ops <- runTodoistWith config (getAllProjects :: Trace [Project])
    print ops  -- Shows recorded operations without executing them
    @
    -}
    runTodoistWith :: TodoistConfig -> r a -> IO (Either TodoistError (Output r a))

instance TodoistRunner TodoistIO where
    type Output TodoistIO a = a

    runTodoistWith :: TodoistConfig -> TodoistIO a -> IO (Either TodoistError a)
    runTodoistWith = todoist

instance TodoistRunner Trace where
    type Output Trace a = [Op]

    runTodoistWith :: TodoistConfig -> Trace a -> IO (Either TodoistError [Op])
    runTodoistWith = todoistTraceRunner
