module Web.Todoist.Builder
    ( setDescription
    , setParentId
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
    , seed
    , runBuilder
    , Builder
    ) where

import Web.Todoist.Builder.Has
    ( HasDescription (..)
    , HasIsFavorite (..)
    , HasName (..)
    , HasParentId (..)
    , HasViewStyle (..)
    , HasWorkspaceId (..)
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
    )
import Web.Todoist.Domain.Types (ViewStyle)

import Data.Bool (Bool)
import Data.Int (Int)
import Data.Monoid (Dual (..), Endo (..), Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import GHC.Err (error)

data Builder s = Builder
    { bSeed :: s
    , bMods :: Dual (Endo s) -- Dual for left-to-right application order
    }

instance Semigroup (Builder s) where
    (<>) :: Builder s -> Builder s -> Builder s
    Builder sd md <> Builder _ n = Builder sd (md <> n)

-- The seed must be explicitly added; change signature to seed -> builder s -> s; the builder can be null or mempty; have Builder be a typeclass with `runBuilder` implemented ?
runBuilder :: Builder s -> s 
runBuilder (Builder sd (Dual (Endo f))) = f sd

seed :: s -> Builder s
seed s = Builder {bSeed = s, bMods = mempty}

modB :: (s -> s) -> Builder s
modB f =
    Builder
        { bSeed =
            error
                "modifier used without seed; ensure seed function is leftmost"
        , bMods = Dual {getDual = Endo {appEndo = f}}
        }

setDescription :: (HasDescription s) => Text -> Builder s
setDescription desc = modB (hasDescription desc)

setParentId :: (HasParentId s) => Text -> Builder s
setParentId pid = modB (hasParentId pid)

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
