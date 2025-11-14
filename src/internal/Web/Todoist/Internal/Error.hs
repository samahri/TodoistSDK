{- |
Module      : Web.Todoist.Internal.Error
Description : Error types for Todoist API operations
Copyright   : (c) 2025 Sam S. Almahri
License     : MIT
Maintainer  : sam.salmahri@gmail.com

Internal module defining error types that can occur during Todoist API operations.
These errors are returned in the Left side of 'Either TodoistError a' results.

This is an internal module. End users will see these errors but don't need to
import this module directly - they're re-exported from the main API.
-}
module Web.Todoist.Internal.Error
    ( TodoistError (..)
    ) where

import Data.String (String)
import Text.Show (Show)

{- | Errors that can occur when interacting with the Todoist API

Covers common HTTP error codes and general HTTP exceptions:

- 'BadRequest': HTTP 400 - Invalid request parameters
- 'NotFound': HTTP 404 - Resource not found
- 'Forbidden': HTTP 403 - Access denied to resource
- 'Unauthorized': HTTP 401 - Invalid or missing authentication token
- 'HttpError': Other HTTP errors (network issues, unexpected status codes)
-}
data TodoistError
    = -- | HTTP 400 - Invalid request parameters or malformed request
      BadRequest
    | -- | HTTP 404 - Requested resource (project, task, etc.) not found
      NotFound
    | -- | HTTP 403 - Access denied (insufficient permissions)
      Forbidden
    | -- | HTTP 401 - Invalid or missing API authentication token
      Unauthorized
    | -- | Other HTTP errors (network issues, unexpected status codes, etc.)
      HttpError String
    deriving (Show)
