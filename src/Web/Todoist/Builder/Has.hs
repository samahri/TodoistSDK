module Web.Todoist.Builder.Has
    ( HasDescription (..)
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
import Data.Text (Text)

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
