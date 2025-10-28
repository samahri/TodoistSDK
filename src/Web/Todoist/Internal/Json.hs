module Web.Todoist.Internal.Json
    ( jsonOpts
    ) where

import Data.Aeson (Options, defaultOptions, fieldLabelModifier, omitNothingFields)
import Data.Bool (Bool (True))
import qualified Data.List as L

-- | Standard JSON options for Todoist API types.
-- Drops the first 2 characters from field names (typically the 'p_' prefix)
-- and omits Nothing fields from JSON output.
jsonOpts :: Options
jsonOpts = defaultOptions {fieldLabelModifier = L.drop 2, omitNothingFields = True}
