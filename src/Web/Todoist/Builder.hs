module Web.Todoist.Builder
    ( setDescription
    , setParentId
    , setViewStyle
    , setWorkspaceId
    , seed
    , runBuilder
    , Builder
    ) where

import Web.Todoist.Builder.Has
    ( Has
    , HasDescription (..)
    , HasParentId (..)
    , HasViewStyle (..)
    , HasWorkspaceId (..)
    )
import Web.Todoist.Domain.Types (ViewStyle)

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

seed :: (Has s) => s -> Builder s
seed s = Builder s mempty

modB :: (s -> s) -> Builder s
modB f =
    Builder
        (error "modifier used without seed; ensure seed function is leftmost")
        (Dual (Endo f))

setDescription :: (HasDescription s) => Text -> Builder s
setDescription desc = modB (hasDescription desc)

setParentId :: (HasParentId s) => Text -> Builder s
setParentId pid = modB (hasParentId pid)

setViewStyle :: (HasViewStyle s) => ViewStyle -> Builder s
setViewStyle style = modB (hasViewStyle style)

setWorkspaceId :: (HasWorkspaceId s) => Int -> Builder s
setWorkspaceId wid = modB (hasWorkspaceId wid)
