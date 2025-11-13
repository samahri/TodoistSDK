{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Todoist.Runner.TodoistIO.Section () where

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
import Web.Todoist.Domain.Section
    ( Section (..)
    , SectionCreate
    , SectionId (..)
    , SectionParam (..)
    , SectionUpdate
    , TodoistSectionM (..)
    )
import Web.Todoist.Domain.Types (IsCollapsed (..), Name (..), Order (..), ProjectId (..))
import Web.Todoist.Internal.Config (TodoistConfig)
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Internal.HTTP (PostResponse (..), apiDelete, apiGet, apiPost)
import Web.Todoist.Internal.Request (mkTodoistRequest)
import Web.Todoist.Internal.Types (SectionResponse (..), TodoistReturn (..))
import Web.Todoist.QueryParam (toQueryParam)
import Web.Todoist.Runner.TodoistIO.Core (TodoistIO (..))

-- | Convert HTTP response to domain type
sectionResponseToSection :: SectionResponse -> Section
sectionResponseToSection SectionResponse {..} =
    Section
        { _id = SectionId {_id = p_id}
        , _name = Name p_name
        , _project_id = ProjectId p_project_id
        , _is_collapsed = IsCollapsed p_is_collapsed
        , _order = Order p_section_order
        }

instance TodoistSectionM TodoistIO where
    getSections :: SectionParam -> TodoistIO [Section]
    getSections initialParams = TodoistIO $ do
        config <- ask
        let loop :: Maybe Text -> [Section] -> ReaderT TodoistConfig (ExceptT TodoistError IO) [Section]
            loop cursorVal acc = do
                let params = initialParams {cursor = cursorVal, limit = Nothing}
                    apiRequest = mkTodoistRequest @Void ["sections"] (Just $ toQueryParam params) Nothing
                resp <- liftIO $ apiGet (Proxy @(TodoistReturn SectionResponse)) config apiRequest
                case resp of
                    Right res -> do
                        let newAcc = acc <> fmap sectionResponseToSection (results res)
                        case next_cursor res of
                            Nothing -> pure newAcc
                            Just c -> loop (Just $ T.pack c) newAcc
                    Left err -> lift $ except (Left err)
        loop Nothing []

    getSection :: SectionId -> TodoistIO Section
    getSection SectionId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["sections", _id] Nothing Nothing
        resp <- liftIO $ apiGet (Proxy @SectionResponse) config apiRequest
        case resp of
            Right res -> pure $ sectionResponseToSection res
            Left err -> lift $ except (Left err)

    addSection :: SectionCreate -> TodoistIO SectionId
    addSection section = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @SectionCreate ["sections"] Nothing Nothing
        resp <- liftIO $ apiPost (Just section) (JsonResponse (Proxy @SectionId)) config apiRequest
        case resp of
            Right res -> pure res
            Left err -> lift $ except (Left err)

    updateSection :: SectionId -> SectionUpdate -> TodoistIO Section
    updateSection SectionId {..} sectionUpdate = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @SectionUpdate ["sections", _id] Nothing Nothing
        resp <-
            liftIO $ apiPost (Just sectionUpdate) (JsonResponse (Proxy @SectionResponse)) config apiRequest
        case resp of
            Right res -> pure $ sectionResponseToSection res
            Left err -> lift $ except (Left err)

    deleteSection :: SectionId -> TodoistIO ()
    deleteSection SectionId {..} = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["sections", _id] Nothing Nothing
        resp <- liftIO $ apiDelete config apiRequest
        case resp of
            Right _ -> pure ()
            Left err -> lift $ except (Left err)

    getSectionsPaginated :: SectionParam -> TodoistIO ([Section], Maybe Text)
    getSectionsPaginated sectionParams = TodoistIO $ do
        config <- ask
        let apiRequest = mkTodoistRequest @Void ["sections"] (Just $ toQueryParam sectionParams) Nothing
        resp <- liftIO $ apiGet (Proxy @(TodoistReturn SectionResponse)) config apiRequest
        case resp of
            Right res -> pure (fmap sectionResponseToSection (results res), fmap T.pack (next_cursor res))
            Left err -> lift $ except (Left err)
