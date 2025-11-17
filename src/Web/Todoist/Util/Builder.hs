{-# LANGUAGE RankNTypes #-}

{- | Builder pattern for constructing domain types with optional fields.

This module provides a type-safe builder pattern for creating domain request types
(like ProjectCreate, TaskCreate, etc.) with ergonomic field setting.

Example usage:

@
let project = runBuilder (createProjectBuilder \"My Project\")
              (withDescription \"A description\" <> withViewStyle Board)
@
-}
module Web.Todoist.Util.Builder
    ( -- * Builder Core Types
      Builder
    , Initial
    , runBuilder
    , seed

      -- * Setter Functions
    , withDescription
    , withParentId
    , withProjectId
    , withTaskId
    , withViewStyle
    , withWorkspaceId
    , withName
    , withIsFavorite
    , withContent
    , withColor
    , withSectionId
    , withOrder
    , withLabels
    , withPriority
    , withAssigneeId
    , withDueString
    , withDueDate
    , withDueDatetime
    , withDueLang
    , withDuration
    , withDurationUnit
    , withDeadlineDate
    , withAttachment
    , withUidsToNotify
    , withCursor
    , withLimit
    , withQuery
    , withLang
    , withSince
    , withUntil
    , withTaskIds
    , withPublicKey
    , withOmitPersonal
    , withFilterQuery
    , withFilterLang

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
    , HasCursor (..)
    , HasLimit (..)
    , HasQuery (..)
    , HasLang (..)
    , HasSince (..)
    , HasUntil (..)
    , HasTaskIds (..)
    , HasPublicKey (..)
    , HasOmitPersonal (..)
    , HasFilterQuery (..)
    , HasFilterLang (..)
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

-- QueryParam-specific type classes
class HasCursor p where
    hasCursor :: Text -> p -> p

class HasLimit p where
    hasLimit :: Int -> p -> p

class HasQuery p where
    hasQuery :: Text -> p -> p

class HasLang p where
    hasLang :: Text -> p -> p

class HasSince p where
    hasSince :: Text -> p -> p

class HasUntil p where
    hasUntil :: Text -> p -> p

class HasTaskIds p where
    hasTaskIds :: [Text] -> p -> p

class HasPublicKey p where
    hasPublicKey :: Text -> p -> p

class HasOmitPersonal p where
    hasOmitPersonal :: Bool -> p -> p

class HasFilterQuery p where
    hasFilterQuery :: Text -> p -> p

class HasFilterLang p where
    hasFilterLang :: Text -> p -> p

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

-- | Set the description field for projects or tasks
withDescription :: (HasDescription s) => Text -> Builder s
withDescription desc = modB (hasDescription desc)

-- | Set the parent project or task ID for hierarchical organization
withParentId :: (HasParentId s) => Text -> Builder s
withParentId pid = modB (hasParentId pid)

-- | Set the project ID to assign a task to a specific project
withProjectId :: (HasProjectId s) => Text -> Builder s
withProjectId pid = modB (hasProjectId pid)

-- | Set the view style for a project (List, Board, or Calendar)
withViewStyle :: (HasViewStyle s) => ViewStyle -> Builder s
withViewStyle style = modB (hasViewStyle style)

-- | Set the workspace ID for team/workspace assignment
withWorkspaceId :: (HasWorkspaceId s) => Int -> Builder s
withWorkspaceId wid = modB (hasWorkspaceId wid)

-- | Set the name field for projects, sections, or labels
withName :: (HasName s) => Text -> Builder s
withName name = modB (hasName name)

-- | Set whether an item is marked as favorite
withIsFavorite :: (HasIsFavorite s) => Bool -> Builder s
withIsFavorite fav = modB (hasIsFavorite fav)

-- | Set the content (title/description) field for tasks
withContent :: (HasContent s) => Text -> Builder s
withContent content = modB (hasContent content)

-- | Set the color field for labels
withColor :: (HasColor s) => Text -> Builder s
withColor color = modB (hasColor color)

-- | Set the section ID to organize a task within a project section
withSectionId :: (HasSectionId s) => Text -> Builder s
withSectionId sid = modB (hasSectionId sid)

-- | Set the sort order (lower numbers appear first)
withOrder :: (HasOrder s) => Int -> Builder s
withOrder order = modB (hasOrder order)

-- | Set the list of label names to tag a task
withLabels :: (HasLabels s) => [Text] -> Builder s
withLabels labels = modB (hasLabels labels)

-- | Set the priority level (1=urgent, 2=high, 3=normal, 4=low)
withPriority :: (HasPriority s) => Int -> Builder s
withPriority priority = modB (hasPriority priority)

-- | Set the assignee user ID for task assignment
withAssigneeId :: (HasAssigneeId s) => Int -> Builder s
withAssigneeId aid = modB (hasAssigneeId aid)

-- | Set a natural language due date string (e.g., \"tomorrow\", \"next Monday\")
withDueString :: (HasDueString s) => Text -> Builder s
withDueString dueStr = modB (hasDueString dueStr)

-- | Set the due date in YYYY-MM-DD format
withDueDate :: (HasDueDate s) => Text -> Builder s
withDueDate dueDate = modB (hasDueDate dueDate)

-- | Set the due datetime in RFC3339 format with timezone
withDueDatetime :: (HasDueDatetime s) => Text -> Builder s
withDueDatetime dueDatetime = modB (hasDueDatetime dueDatetime)

-- | Set the language code for parsing natural language due strings
withDueLang :: (HasDueLang s) => Text -> Builder s
withDueLang dueLang = modB (hasDueLang dueLang)

-- | Set the task duration amount (used with duration_unit)
withDuration :: (HasDuration s) => Int -> Builder s
withDuration duration = modB (hasDuration duration)

-- | Set the task duration unit (\"minute\" or \"day\")
withDurationUnit :: (HasDurationUnit s) => Text -> Builder s
withDurationUnit durationUnit = modB (hasDurationUnit durationUnit)

-- | Set the deadline date for a task
withDeadlineDate :: (HasDeadlineDate s) => Text -> Builder s
withDeadlineDate deadlineDate = modB (hasDeadlineDate deadlineDate)

-- | Set an attachment for a comment
withAttachment :: (HasAttachment s) => Attachment -> Builder s
withAttachment attachment = modB (hasAttachment attachment)

-- | Set the task ID for comments to associate a comment with a task
withTaskId :: (HasTaskId s) => Text -> Builder s
withTaskId tid = modB (hasTaskId tid)

-- | Set the list of user IDs to notify about a comment
withUidsToNotify :: (HasUidsToNotify s) => [Int] -> Builder s
withUidsToNotify uids = modB (hasUidsToNotify uids)

-- | Set the pagination cursor for fetching the next page of results
withCursor :: (HasCursor s) => Text -> Builder s
withCursor cursor = modB (hasCursor cursor)

-- | Set the maximum number of items to return per page
withLimit :: (HasLimit s) => Int -> Builder s
withLimit limit = modB (hasLimit limit)

-- | Set the search query text for filtering items
withQuery :: (HasQuery s) => Text -> Builder s
withQuery query = modB (hasQuery query)

-- | Set the language code for natural language processing
withLang :: (HasLang s) => Text -> Builder s
withLang lang = modB (hasLang lang)

-- | Set the start date for date range filtering
withSince :: (HasSince s) => Text -> Builder s
withSince since = modB (hasSince since)

-- | Set the end date for date range filtering
withUntil :: (HasUntil s) => Text -> Builder s
withUntil until = modB (hasUntil until)

-- | Set the list of task IDs for filtering by specific tasks
withTaskIds :: (HasTaskIds s) => [Text] -> Builder s
withTaskIds taskIds = modB (hasTaskIds taskIds)

-- | Set the public key for accessing shared comments
withPublicKey :: (HasPublicKey s) => Text -> Builder s
withPublicKey publicKey = modB (hasPublicKey publicKey)

-- | Set whether to omit personal labels from results
withOmitPersonal :: (HasOmitPersonal s) => Bool -> Builder s
withOmitPersonal omitPersonal = modB (hasOmitPersonal omitPersonal)

-- | Set the filter query for completed tasks
withFilterQuery :: (HasFilterQuery s) => Text -> Builder s
withFilterQuery filterQuery = modB (hasFilterQuery filterQuery)

-- | Set the filter language for completed tasks
withFilterLang :: (HasFilterLang s) => Text -> Builder s
withFilterLang filterLang = modB (hasFilterLang filterLang)
