{-# LANGUAGE DuplicateRecordFields #-}

{- |
Module      : Web.Todoist
Description : Main entry point for the Todoist SDK
Copyright   : (c) 2025 Sam S. Almahri
License     : MIT
Maintainer  : sam.salmahri@gmail.com

This module provides a convenient single import for using the Todoist SDK.
It re-exports the most commonly used types and functions.

= Quick Start

> import Web.Todoist
>
> main :: IO ()
> main = do
>     let config = newTodoistConfig "your-api-token"
>     result <- todoist config $ do
>         projects <- getAllProjects
>         pure projects
>     case result of
>         Left err -> print err
>         Right projects -> print projects
-}
module Web.Todoist
    ( -- * Running Operations
      todoist
    , newTodoistConfig
    , runTodoistWith
    , MonadTodoist

      -- * Core Types
    , TodoistConfig (..)
    , TodoistIO (..)
    , TodoistError (..)

      -- * Domain Type Classes
    , TodoistProjectM (..)
    , TodoistTaskM (..)
    , TodoistCommentM (..)
    , TodoistSectionM (..)
    , TodoistLabelM (..)

      -- * Project Types
    , Project
    , ProjectCreate
    , ProjectUpdate
    , Collaborator
    , PaginationParam (..)
    , createProjectBuilder
    , updateProjectBuilder
    , paginationParamBuilder
    , IsShared (..)
    , IsArchived (..)
    , CanAssignTasks (..)

      -- * Task Types
    , Task (..)
    , TaskParam (..)
    , NewTask (..)
    , MoveTask
    , taskParamBuilder

      -- * Comment Types
    , Comment (..)
    , CommentId (..)
    , CommentCreate
    , CommentUpdate
    , CommentParam (..)
    , newCommentBuilder
    , updateCommentBuilder
    , commentParamBuilder

      -- * Section Types
    , Section (..)
    , SectionId (..)
    , SectionCreate
    , SectionUpdate
    , SectionParam (..)
    , newSectionBuilder
    , updateSectionBuilder
    , sectionParamBuilder

      -- * Label Types
    , Label (..)
    , LabelId (..)
    , LabelCreate
    , LabelUpdate
    , LabelParam (..)
    , SharedLabelParam (..)
    , SharedLabelRename
    , SharedLabelRemove
    , createLabelBuilder
    , updateLabelBuilder
    , labelParamBuilder
    , sharedLabelParamBuilder
    , mkSharedLabelRename
    , mkSharedLabelRemove

      -- * Common Domain Types
    , ProjectId (..)
    , TaskId (..)
    , Name (..)
    , Description (..)
    , Color (..)
    , IsFavorite (..)
    , ViewStyle (..)
    , Order (..)
    , Content (..)
    , ParentId (..)

      -- * Builder Pattern
    , Builder
    , Initial
    , runBuilder
    , seed
    , withName
    , withDescription
    , withIsFavorite
    , withViewStyle
    , withParentId
    , withProjectId
    , withSectionId
    , withContent
    , withColor
    , withPriority
    , withDueString
    , withDueDate
    , withDueDatetime
    , withDueLang
    , withAssigneeId
    , withLabels
    , withWorkspaceId
    , withOrder
    , withTaskId
    ) where

import Web.Todoist.Runner
    ( MonadTodoist
    , newTodoistConfig
    , runTodoistWith
    , todoist
    )

import Web.Todoist.Runner.IO
    ( TodoistConfig (..)
    , TodoistIO (..)
    )

import Web.Todoist.Internal.Error (TodoistError (..))

import Web.Todoist.Domain.Project
    ( CanAssignTasks (..)
    , Collaborator
    , IsArchived (..)
    , IsShared (..)
    , PaginationParam (..)
    , Project
    , ProjectCreate
    , ProjectUpdate
    , TodoistProjectM (..)
    , updateProjectBuilder
    , paginationParamBuilder
    , createProjectBuilder
    )

import Web.Todoist.Domain.Task
    ( MoveTask
    , NewTask (..)
    , Task (..)
    , TaskParam (..)
    , TodoistTaskM (..)
    , taskParamBuilder
    )

import Web.Todoist.Domain.Comment
    ( Comment (..)
    , CommentCreate
    , CommentId (..)
    , CommentParam (..)
    , CommentUpdate
    , TodoistCommentM (..)
    , newCommentBuilder
    , commentParamBuilder
    , updateCommentBuilder
    )

import Web.Todoist.Domain.Section
    ( Section (..)
    , SectionCreate
    , SectionId (..)
    , SectionParam (..)
    , SectionUpdate
    , TodoistSectionM (..)
    , updateSectionBuilder
    , newSectionBuilder
    , sectionParamBuilder
    )

import Web.Todoist.Domain.Label
    ( Label (..)
    , LabelCreate
    , LabelId (..)
    , LabelParam (..)
    , LabelUpdate
    , SharedLabelParam (..)
    , SharedLabelRemove
    , SharedLabelRename
    , TodoistLabelM (..)
    , updateLabelBuilder
    , createLabelBuilder
    , labelParamBuilder
    , sharedLabelParamBuilder
    , mkSharedLabelRename
    , mkSharedLabelRemove
    )

import Web.Todoist.Domain.Types
    ( Color (..)
    , Content (..)
    , Description (..)
    , IsFavorite (..)
    , Name (..)
    , Order (..)
    , ParentId (..)
    , ProjectId (..)
    , TaskId (..)
    , ViewStyle (..)
    )

import Web.Todoist.Util.Builder
    ( Builder
    , Initial
    , runBuilder
    , seed
    , withAssigneeId
    , withColor
    , withContent
    , withDescription
    , withDueDate
    , withDueDatetime
    , withDueLang
    , withDueString
    , withIsFavorite
    , withLabels
    , withName
    , withOrder
    , withParentId
    , withPriority
    , withProjectId
    , withSectionId
    , withTaskId
    , withViewStyle
    , withWorkspaceId
    )
