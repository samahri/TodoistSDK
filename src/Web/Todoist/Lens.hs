{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module: Web.Todoist.Lens
Description: Getter-only lenses for TodoistSDK types

This module re-exports the @microlens@ operators needed for read-only field access.
All lenses in TodoistSDK are getter-only - they provide read access but no setters.

For field mutations, use the builder pattern (@Web.Todoist.Util.Builder@).

Example usage:
@
import Web.Todoist
import Web.Todoist.Lens ((^.))

-- Extract field from Project
let projectName = myProject ^. name

-- Compose getters for nested access
let dueDateValue = myTask ^. due . date
@
-}
module Web.Todoist.Lens
    ( -- * Getter Types
      Getter
    , Getting
      -- * Operators
    , (^.)
    , to
    ) where

import Lens.Micro (Getting, to, (^.))

-- | A 'Getter' is a read-only lens
type Getter s a = forall r. Getting r s a
