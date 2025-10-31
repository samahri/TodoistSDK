module Web.Todoist.Builder
    ( setDescription
    , setParentId
    , setViewStyle
    , setWorkspaceId
    , setName
    , setIsFavorite
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
