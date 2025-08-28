module TodoistSDK.Interpreter.Types (
  Token(..), -- make it opaque
  TodoistEnv(..),
  TodoistError(..)
) where

import Data.Text

newtype Token = Token Text deriving Show

data TodoistEnv = TodoistEnv {
  authToken :: Token,
  baseUrl   :: String
} deriving Show

data TodoistError = NotTwoHunderd deriving Show
