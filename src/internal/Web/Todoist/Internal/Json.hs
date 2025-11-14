{- |
Module      : Web.Todoist.Internal.Json
Description : JSON serialization utilities for Todoist API types
Copyright   : (c) 2025 Sam S. Almahri
License     : MIT
Maintainer  : sam.salmahri@gmail.com

Internal module providing JSON serialization utilities for Todoist API types.

The codebase uses a two-layer JSON serialization strategy:

1. HTTP Response Types use @p_@ prefix (e.g., @p_id@, @p_name@) and serialize
   using 'jsonOpts' which drops 2 characters
2. Domain Request/Response Types use @_@ prefix (e.g., @_id@, @_name@) and
   serialize using @fieldLabelModifier = drop 1@

This module provides the standardized options for the HTTP layer.
-}
module Web.Todoist.Internal.Json
    ( jsonOpts
    ) where

import Data.Aeson (Options, defaultOptions, fieldLabelModifier, omitNothingFields)
import Data.Bool (Bool (True))
import qualified Data.List as L

{- | Standard JSON options for HTTP response types

Applies two transformations:

1. Drops first 2 characters from field names (the @p_@ prefix)
2. Omits @Nothing@ fields from JSON output for partial updates

Example: @p_id@ becomes @\"id\"@ in JSON, @p_name@ becomes @\"name\"@
-}
jsonOpts :: Options
jsonOpts = defaultOptions {fieldLabelModifier = L.drop 2, omitNothingFields = True}
