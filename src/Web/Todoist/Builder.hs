{-# LANGUAGE RankNTypes #-}

module Web.Todoist.Builder
    ( -- Setter functions
      setDescription
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
    -- Core types and functions
    , Initial -- Type exported for internal use only
    , seed
    , Builder -- Type exported with constructor for internal use
    , runBuilder
    ) where

import Web.Todoist.Builder.Has
    ( HasAssigneeId (..)
    , HasAttachment (hasAttachment)
    , HasContent (..)
    , HasDeadlineDate (..)
    , HasDescription (..)
    , HasDueDate (..)
    , HasDueDatetime (..)
    , HasDueLang (..)
    , HasDueString (..)
    , HasDuration (..)
    , HasDurationUnit (..)
    , HasIsFavorite (..)
    , HasLabels (..)
    , HasName (..)
    , HasOrder (..)
    , HasParentId (..)
    , HasPriority (..)
    , HasProjectId (..)
    , HasSectionId (..)
    , HasTaskId (..)
    , HasUidsToNotify (..)
    , HasViewStyle (..)
    , HasWorkspaceId (..)
    )
import Web.Todoist.Domain.Types (Attachment, ViewStyle)

import Data.Bool (Bool)
import Data.Int (Int)
import Data.Monoid (Dual (..), Endo (..), Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)

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
