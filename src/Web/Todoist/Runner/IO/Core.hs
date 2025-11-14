{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Web.Todoist.Runner.IO.Core
Description : TodoistIO monad definition and core types
Copyright   : (c) 2025 Sam S. Almahri
License     : MIT
Maintainer  : sam.salmahri@gmail.com

This module defines the 'TodoistIO' monad used for executing real HTTP requests
to the Todoist API. Most users will use the 'todoist' function from
"Web.Todoist.Runner" rather than working with this monad directly.

The TodoistIO monad is implemented as a ReaderT/ExceptT stack over IO, providing
implicit configuration threading and automatic error handling.
-}
module Web.Todoist.Runner.IO.Core
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
