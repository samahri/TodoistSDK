{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Todoist.Runner.TodoistIO.Label () where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import System.IO (IO)

import Control.Applicative (pure)
import Control.Monad (fmap)
import Data.Function (($))
import Data.Monoid ((<>))
import Web.Todoist.Domain.Label
    ( Label (..)
    , LabelCreate
    , LabelId (..)
    , LabelParam (..)
    , LabelUpdate
    , SharedLabelParam (..)
    , SharedLabelRemove
    , SharedLabelRename
    , TodoistLabelM (..)
    )
import Web.Todoist.Internal.Config (TodoistConfig)
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Internal.HTTP (PostResponse (..), apiDelete, apiGet, apiPost)
import Web.Todoist.Internal.Request (mkTodoistRequest)
import Web.Todoist.Internal.Types (LabelResponse (..), TodoistReturn (..))
import Web.Todoist.QueryParam (toQueryParam)
import Web.Todoist.Runner.TodoistIO.Core (TodoistIO (..))

-- | Convert HTTP response to domain type
labelResponseToLabel :: LabelResponse -> Label
labelResponseToLabel LabelResponse {..} =
    Label
        { _id = p_id
        , _name = p_name
        , _color = p_color
        , _order = p_order
        , _is_favorite = p_is_favorite
        }

instance TodoistLabelM TodoistIO where
    getLabels :: LabelParam -> TodoistIO [Label]
    getLabels _ = TodoistIO $ do
        config <- ask
        let loop :: Maybe Text -> [Label] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Label]
            loop cursorVal acc = do
                let params = LabelParam {_cursor = cursorVal, _limit = Nothing}
                    apiRequest = mkTodoistRequest @Void ["labels"] (Just $ toQueryParam params) Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn LabelResponse)) config apiRequest
                case resp of
                    Right res -> do
                        let newAcc = acc <> fmap labelResponseToLabel (results res)
                        case next_cursor res of
                            Nothing -> pure newAcc
                            Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    getLabel :: LabelId -> TodoistIO Label
    getLabel LabelId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["labels", getLabelId] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @LabelResponse) config apiRequest
        case resp of
            Right res -> pure $ labelResponseToLabel res
            Left err -> lift $ except (Left err)

    addLabel :: LabelCreate -> TodoistIO LabelId
    addLabel label = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @LabelCreate ["labels"] Nothing Nothing
        resp <- liftIO $ apiPost (Just label) (JsonResponse (Proxy @LabelId)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    updateLabel :: LabelId -> LabelUpdate -> TodoistIO Label
    updateLabel LabelId {..} labelUpdate = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @LabelUpdate ["labels", getLabelId] Nothing Nothing
        resp <-
            liftIO $ apiPost (Just labelUpdate) (JsonResponse (Proxy @LabelResponse)) config apiRequest
        case resp of
            Right res -> pure $ labelResponseToLabel res
            Left err -> lift $ except (Left err)

    deleteLabel :: LabelId -> TodoistIO ()
    deleteLabel LabelId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["labels", getLabelId] Nothing Nothing
        resp <- liftIO $ apiDelete config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    getLabelsPaginated :: LabelParam -> TodoistIO ([Label], Maybe Text)
    getLabelsPaginated labelParams = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["labels"] (Just $ toQueryParam labelParams) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn LabelResponse)) config apiRequest
        case resp of
            Right res -> pure (fmap labelResponseToLabel (results res), fmap T.pack (next_cursor res))
            Left err -> lift $ except (Left err)

    getSharedLabels :: SharedLabelParam -> TodoistIO [Text]
    getSharedLabels (SharedLabelParam initialOmit _ _) = TodoistIO $ do
        config <- ask
        let loop :: Maybe Text -> [Text] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Text]
            loop cursorVal acc = do
                let params = SharedLabelParam {_omit_personal = initialOmit, _cursor = cursorVal, _limit = Nothing}
                    apiRequest =
                        mkTodoistRequest @Void
                            ["labels", "shared"]
                            (Just $ toQueryParam (params :: SharedLabelParam))
                            Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn Text)) config apiRequest
                case resp of
                    Right res -> do
                        let newAcc = acc <> results res
                        case next_cursor res of
                            Nothing -> pure newAcc
                            Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    getSharedLabelsPaginated :: SharedLabelParam -> TodoistIO ([Text], Maybe Text)
    getSharedLabelsPaginated sharedParams = TodoistIO $ do
        config <- ask
        let apiRequest =
                mkTodoistRequest @Void
                    ["labels", "shared"]
                    (Just $ toQueryParam (sharedParams :: SharedLabelParam))
                    Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn Text)) config apiRequest
        case resp of
            Right res -> pure (results res, fmap T.pack (next_cursor res))
            Left err -> lift $ except (Left err)

    removeSharedLabels :: SharedLabelRemove -> TodoistIO ()
    removeSharedLabels removeReq = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @SharedLabelRemove ["labels", "shared", "remove"] Nothing Nothing
        resp <- liftIO $ apiPost (Just removeReq) IgnoreResponse config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    renameSharedLabels :: SharedLabelRename -> TodoistIO ()
    renameSharedLabels renameReq = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @SharedLabelRename ["labels", "shared", "rename"] Nothing Nothing
        resp <- liftIO $ apiPost (Just renameReq) IgnoreResponse config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)
