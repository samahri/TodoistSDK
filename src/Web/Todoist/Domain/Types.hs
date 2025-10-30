{-# LANGUAGE InstanceSigs #-}

module Web.Todoist.Domain.Types
    ( ViewStyle (..)
    , parseViewStyle
    ) where

import Control.Monad (return)
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON (toJSON)
    , Value
    )
import Data.Aeson.Types (Parser)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Base (undefined)
import Text.Show (Show)

data ViewStyle = List | Board | Calendar deriving (Show, Eq)

instance ToJSON ViewStyle where
    toJSON :: ViewStyle -> Value
    toJSON List = toJSON ("list" :: Text)
    toJSON Board = toJSON ("board" :: Text)
    toJSON Calendar = toJSON ("calendar" :: Text)

instance FromJSON ViewStyle where
    parseJSON :: Value -> Parser ViewStyle
    parseJSON v = do
        str <- parseJSON v
        return $ parseViewStyle str

-- | Parse a Text string into ViewStyle, defaulting to undefined for unrecognized values
parseViewStyle :: Text -> ViewStyle
parseViewStyle txt = case T.toLower txt of
    "list" -> List
    "board" -> Board
    "calendar" -> Calendar
    _ -> undefined -- Default to undefined for unrecognized values
