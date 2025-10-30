{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Core TodoistIO type definition
module Web.Todoist.Runner.TodoistIO.Core
    ( TodoistIO (..)
    ) where

import Web.Todoist.Internal.Config (TodoistConfig)
import Web.Todoist.Internal.Error (TodoistError)

import Control.Applicative (Applicative)
import Control.Monad (Functor, Monad)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import System.IO (IO)

-- | TodoistIO monad - provides IO-based execution for Todoist operations
newtype TodoistIO a
    = TodoistIO {unTodoist :: ReaderT TodoistConfig (ExceptT TodoistError IO) a}
    deriving newtype (Functor, Applicative, Monad)
