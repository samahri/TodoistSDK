{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Web.Todoist.Domain.Types
    ( Attachment
    , ViewStyle (..)
    , ProjectId (..)
    , TaskId (..)
    , Uid (..)
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
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Aeson.Types (Parser)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor ((<$>))
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

-- TODO: DOCUMENTATION - Add Haddock documentation for all exported types
newtype ProjectId = ProjectId
    { getProjectId :: Text
    }
    deriving (Show, Eq, Generic)

-- Custom JSON instances for ProjectId - parse from {"id": "..."} object
instance FromJSON ProjectId where
    parseJSON :: Value -> Parser ProjectId
    parseJSON = withObject "ProjectId" $ \obj ->
        ProjectId <$> obj .: "id"

instance ToJSON ProjectId where
    toJSON :: ProjectId -> Value
    toJSON (ProjectId txt) = object ["id" .= txt]

newtype TaskId = TaskId
    { getTaskId :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON TaskId where
    parseJSON :: Value -> Parser TaskId
    parseJSON v = TaskId <$> parseJSON v

instance ToJSON TaskId where
    toJSON :: TaskId -> Value
    toJSON (TaskId txt) = toJSON txt

newtype Uid = Uid {getUid :: Text} deriving (Show, Eq, Generic)

instance FromJSON Uid where
    parseJSON :: Value -> Parser Uid
    parseJSON v = Uid <$> parseJSON v

instance ToJSON Uid where
    toJSON :: Uid -> Value
    toJSON (Uid txt) = toJSON txt
