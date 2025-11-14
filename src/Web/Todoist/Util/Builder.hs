{-# LANGUAGE RankNTypes #-}

-- | Builder pattern for constructing domain types with optional fields.
--
-- This module provides a type-safe builder pattern for creating domain request types
-- (like ProjectCreate, TaskCreate, etc.) with ergonomic field setting.
--
-- Example usage:
--
-- @
-- let project = runBuilder (newProject \"My Project\")
--               (setDescription \"A description\" <> setViewStyle Board)
-- @
module Web.Todoist.Util.Builder
    ( -- * Builder Core Types
      Builder
    , Initial
    , runBuilder
    , seed
      -- * Setter Functions
    , setDescription
    , setParentId
    , setProjectId
    , setTaskId
    , setViewStyle
    , setWorkspaceId
    , setName
    , setIsFavorite
    , setContent
    , setSectionId
    , setOrder
    , setLabels
    , setPriority
    , setAssigneeId
    , setDueString
    , setDueDate
    , setDueDatetime
    , setDueLang
    , setDuration
    , setDurationUnit
    , setDeadlineDate
    , setAttachment
    , setUidsToNotify
      -- * Type Classes (for implementing Has* instances)
    , HasDescription (..)
    , HasParentId (..)
    , HasProjectId (..)
    , HasViewStyle (..)
    , HasWorkspaceId (..)
    , HasName (..)
    , HasIsFavorite (..)
    , HasContent (..)
    , HasSectionId (..)
    , HasOrder (..)
    , HasLabels (..)
    , HasPriority (..)
    , HasAssigneeId (..)
    , HasDueString (..)
    , HasDueDate (..)
    , HasDueDatetime (..)
    , HasDueLang (..)
    , HasDuration (..)
    , HasDurationUnit (..)
    , HasDeadlineDate (..)
    , HasTaskId (..)
    , HasUidsToNotify (..)
    , HasAttachment (..)
    , HasColor (..)
    ) where

import Web.Todoist.Domain.Types (Attachment, ViewStyle)

import Data.Bool (Bool)
import Data.Int (Int)
import Data.Monoid (Dual (..), Endo (..), Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)

-- ============================================================================
-- Type Classes
-- ============================================================================

class HasDescription p where
    hasDescription :: Text -> p -> p

class HasParentId p where
    hasParentId :: Text -> p -> p

class HasProjectId p where
    hasProjectId :: Text -> p -> p

class HasViewStyle p where
    hasViewStyle :: ViewStyle -> p -> p

class HasWorkspaceId p where
    hasWorkspaceId :: Int -> p -> p

-- | Type class for types that have a name field
class HasName p where
    hasName :: Text -> p -> p

-- | Type class for types that have an is_favorite field
class HasIsFavorite p where
    hasIsFavorite :: Bool -> p -> p

class HasContent p where
    hasContent :: Text -> p -> p

class HasSectionId p where
    hasSectionId :: Text -> p -> p

class HasOrder p where
    hasOrder :: Int -> p -> p

class HasLabels p where
    hasLabels :: [Text] -> p -> p

class HasPriority p where
    hasPriority :: Int -> p -> p

class HasAssigneeId p where
    hasAssigneeId :: Int -> p -> p

class HasDueString p where
    hasDueString :: Text -> p -> p

class HasDueDate p where
    hasDueDate :: Text -> p -> p

class HasDueDatetime p where
    hasDueDatetime :: Text -> p -> p

class HasDueLang p where
    hasDueLang :: Text -> p -> p

class HasDuration p where
    hasDuration :: Int -> p -> p

class HasDurationUnit p where
    hasDurationUnit :: Text -> p -> p

class HasDeadlineDate p where
    hasDeadlineDate :: Text -> p -> p

-- Comment-specific setters
class HasTaskId p where
    hasTaskId :: Text -> p -> p

class HasUidsToNotify p where
    hasUidsToNotify :: [Int] -> p -> p

class HasAttachment p where
    hasAttachment :: Attachment -> p -> p

class HasColor p where
    hasColor :: Text -> p -> p

-- ============================================================================
-- Builder Types and Functions
-- ============================================================================

-- Newtype for seed values (constructor NOT exported)
newtype Initial s = Initial s

newtype Builder s = Builder
    { bMods :: Dual (Endo s) -- Dual for left-to-right application order
    }

instance Semigroup (Builder s) where
    (<>) :: Builder s -> Builder s -> Builder s
    Builder md1 <> Builder md2 = Builder (md1 <> md2)

instance Monoid (Builder s) where
    mempty :: Builder s
    mempty = Builder mempty

runBuilder :: Initial s -> Builder s -> s
runBuilder (Initial s) (Builder (Dual (Endo f))) = f s

seed :: s -> Initial s
seed = Initial

modB :: (s -> s) -> Builder s
modB f = Builder {bMods = Dual {getDual = Endo {appEndo = f}}}

-- ============================================================================
-- Setter Functions
-- ============================================================================

setDescription :: (HasDescription s) => Text -> Builder s
setDescription desc = modB (hasDescription desc)

setParentId :: (HasParentId s) => Text -> Builder s
setParentId pid = modB (hasParentId pid)

setProjectId :: (HasProjectId s) => Text -> Builder s
setProjectId pid = modB (hasProjectId pid)

setViewStyle :: (HasViewStyle s) => ViewStyle -> Builder s
setViewStyle style = modB (hasViewStyle style)

setWorkspaceId :: (HasWorkspaceId s) => Int -> Builder s
setWorkspaceId wid = modB (hasWorkspaceId wid)

setName :: (HasName s) => Text -> Builder s
setName name = modB (hasName name)

setIsFavorite :: (HasIsFavorite s) => Bool -> Builder s
setIsFavorite fav = modB (hasIsFavorite fav)

setContent :: (HasContent s) => Text -> Builder s
setContent content = modB (hasContent content)

setSectionId :: (HasSectionId s) => Text -> Builder s
setSectionId sid = modB (hasSectionId sid)

setOrder :: (HasOrder s) => Int -> Builder s
setOrder order = modB (hasOrder order)

setLabels :: (HasLabels s) => [Text] -> Builder s
setLabels labels = modB (hasLabels labels)

setPriority :: (HasPriority s) => Int -> Builder s
setPriority priority = modB (hasPriority priority)

setAssigneeId :: (HasAssigneeId s) => Int -> Builder s
setAssigneeId aid = modB (hasAssigneeId aid)

setDueString :: (HasDueString s) => Text -> Builder s
setDueString dueStr = modB (hasDueString dueStr)

setDueDate :: (HasDueDate s) => Text -> Builder s
setDueDate dueDate = modB (hasDueDate dueDate)

setDueDatetime :: (HasDueDatetime s) => Text -> Builder s
setDueDatetime dueDatetime = modB (hasDueDatetime dueDatetime)

setDueLang :: (HasDueLang s) => Text -> Builder s
setDueLang dueLang = modB (hasDueLang dueLang)

setDuration :: (HasDuration s) => Int -> Builder s
setDuration duration = modB (hasDuration duration)

setDurationUnit :: (HasDurationUnit s) => Text -> Builder s
setDurationUnit durationUnit = modB (hasDurationUnit durationUnit)

setDeadlineDate :: (HasDeadlineDate s) => Text -> Builder s
setDeadlineDate deadlineDate = modB (hasDeadlineDate deadlineDate)

setAttachment :: (HasAttachment s) => Attachment -> Builder s
setAttachment attachment = modB (hasAttachment attachment)

setTaskId :: (HasTaskId s) => Text -> Builder s
setTaskId tid = modB (hasTaskId tid)

setUidsToNotify :: (HasUidsToNotify s) => [Int] -> Builder s
setUidsToNotify uids = modB (hasUidsToNotify uids)
