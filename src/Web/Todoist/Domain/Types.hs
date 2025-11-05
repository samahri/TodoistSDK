{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Web.Todoist.Domain.Types
    ( Attachment
    , ViewStyle (..)
    , parseViewStyle
    ) where

import Control.Monad (return)
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value
    , defaultOptions
    , fieldLabelModifier
    , genericParseJSON
    , genericToJSON
    )
import Data.Aeson.Types (Parser)
import Data.Eq (Eq)
import Data.Function (($))
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Base (undefined)
import GHC.Generics (Generic)
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

-- | Attachment metadata for comment
data Attachment = Attachment
    { _resource_type :: Text
    , _file_name :: Text
    , _file_type :: Text
    , _file_url :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON Attachment where
    toJSON :: Attachment -> Value
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = L.drop 1}

instance FromJSON Attachment where
    parseJSON :: Value -> Parser Attachment
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = L.drop 1}
