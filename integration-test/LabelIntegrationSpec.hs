{-# LANGUAGE DisambiguateRecordFields #-}

module LabelIntegrationSpec (spec) where

import Control.Applicative (Applicative (pure), (<$>))
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Bool (Bool (..))
import Data.Foldable (traverse_)
import Data.Function (($), (.))
import Data.Functor (Functor (..))
import Data.List (elem, length)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..), (<>))
import Data.Ord ((>=))
import Data.Text (Text, pack)
import Data.Traversable (traverse)
import Helpers
    ( assertSucceeds
    , generateUniqueName
    , getTestConfig
    , liftTodoist
    )
import System.IO (IO, putStrLn)
import Test.Hspec (Spec, describe, it, pendingWith, runIO, shouldBe, shouldSatisfy)
import Text.Show (show)
import Web.Todoist.Builder (runBuilder)
import Web.Todoist.Domain.Label
    ( Label (..)
    , LabelId (..)
    , LabelParam (..)
    , LabelUpdate (..)
    , SharedLabelParam (..)
    , SharedLabelRemove (..)
    , SharedLabelRename (..)
    , addLabel
    , deleteLabel
    , getLabel
    , getLabels
    , getSharedLabels
    , newLabel
    , removeSharedLabels
    , renameSharedLabels
    , updateLabel
    )
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Runner (todoist)
import Web.Todoist.Runner.TodoistIO (TodoistConfig)

spec :: Spec
spec = do
    maybeConfig <- runIO getTestConfig
    case maybeConfig of
        Nothing ->
            it "requires TODOIST_TEST_API_TOKEN" $
                pendingWith "TODOIST_TEST_API_TOKEN not set"
        Just config -> do
            labelLifecycleSpec config
            getLabelsSpec config
            updateLabelSpec config
            sharedLabelsSpec config

labelLifecycleSpec :: TodoistConfig -> Spec
labelLifecycleSpec config =
    describe "Label CRUD lifecycle" $ do
        it "creates, retrieves, and deletes a label" $ do
            labelName <- pack <$> generateUniqueName "IntegTest-Label"

            withTestLabel config labelName $ \labelId -> do
                -- Get label and verify fields
                label <- liftTodoist config (getLabel labelId)
                let Label {_name = labName} = label
                liftIO $ labName `shouldBe` labelName

                -- Delete label
                liftTodoist config (deleteLabel labelId)

                -- Verify deletion by checking label is not in active labels list
                let params = LabelParam {_cursor = Nothing, _limit = Nothing}
                labels <- liftTodoist config (getLabels params)
                let LabelId {_id = expectedId} = labelId
                    labelIds = fmap (\(Label {_id = lid}) -> lid) labels
                liftIO $ expectedId `elem` labelIds `shouldBe` False

getLabelsSpec :: TodoistConfig -> Spec
getLabelsSpec config =
    describe "Get multiple labels" $ do
        it "creates multiple labels and retrieves them with getLabels" $ do
            -- Create 3 labels
            label1Name <- pack <$> generateUniqueName "Label1"
            label2Name <- pack <$> generateUniqueName "Label2"
            label3Name <- pack <$> generateUniqueName "Label3"

            withMultipleTestLabels config [label1Name, label2Name, label3Name] $ \_ -> do
                -- Get labels
                let params = LabelParam {_cursor = Nothing, _limit = Nothing}
                labels <- liftTodoist config (getLabels params)

                -- Verify count and names
                let labelNames = fmap (\(Label {_name = n}) -> n) labels
                liftIO $ label1Name `elem` labelNames `shouldBe` True
                liftIO $ label2Name `elem` labelNames `shouldBe` True
                liftIO $ label3Name `elem` labelNames `shouldBe` True

updateLabelSpec :: TodoistConfig -> Spec
updateLabelSpec config =
    describe "Update label" $ do
        it "updates a label name and color" $ do
            labelName <- pack <$> generateUniqueName "IntegTest-Update-Label"

            withTestLabel config labelName $ \labelId -> do
                -- Update label name and favorite status
                let newName = labelName <> "-Updated"
                    update =
                        LabelUpdate
                            { _name = Just newName
                            , _order = Nothing
                            , _color = Just "berry_red"
                            , _is_favorite = Just True
                            }
                updatedLabel <- liftTodoist config (updateLabel labelId update)

                -- Verify update
                let Label {_name = updatedName, _id = updatedId, _color = updatedColor, _is_favorite = updatedFav} = updatedLabel
                    LabelId {_id = expectedId} = labelId
                liftIO $ updatedName `shouldBe` newName
                liftIO $ updatedId `shouldBe` expectedId
                liftIO $ updatedColor `shouldBe` "berry_red"
                liftIO $ updatedFav `shouldBe` True

sharedLabelsSpec :: TodoistConfig -> Spec
sharedLabelsSpec config =
    describe "Shared labels operations" $ do
        it "gets shared labels" $ do
            let params = SharedLabelParam {_omit_personal = Nothing, _cursor = Nothing, _limit = Just 10}
            sharedLabels <- assertSucceeds $ liftTodoist config (getSharedLabels params)
            -- Just verify we can call the endpoint without errors
            -- Actual content depends on user's account
            liftIO $ length sharedLabels `shouldSatisfy` (>= 0)

        it "renames and removes shared labels" $ do
            -- Create a label to work with
            labelName <- pack <$> generateUniqueName "SharedTest"

            withTestLabel config labelName $ \_ -> do
                -- Rename shared label
                let renameReq = SharedLabelRename {_name = labelName, _new_name = labelName <> "-Renamed"}
                liftTodoist config (renameSharedLabels renameReq)

                -- Remove shared label
                let removeReq = SharedLabelRemove {_name = labelName <> "-Renamed"}
                liftTodoist config (removeSharedLabels removeReq)

                -- If we got here without errors, operations succeeded
                pure ()

-- | Bracket helper for creating test label
withTestLabel :: TodoistConfig -> Text -> (LabelId -> ExceptT TodoistError IO a) -> IO ()
withTestLabel config labelName action = do
    let createLabel = do
            liftIO $ putStrLn $ "Creating test label: " <> show labelName
            liftTodoist config (addLabel $ runBuilder (newLabel labelName) mempty)

    let deleteLabel' labelId = do
            liftIO $ putStrLn $ "Cleaning up test label: " <> show labelName
            void $ todoist config (deleteLabel labelId)

    let runAction labelId = void $ assertSucceeds $ action labelId

    bracket (assertSucceeds createLabel) deleteLabel' runAction

-- | Bracket helper for creating multiple test labels
withMultipleTestLabels ::
    TodoistConfig -> [Text] -> ([LabelId] -> ExceptT TodoistError IO a) -> IO ()
withMultipleTestLabels config labelNames action = do
    let createLabels = do
            liftIO $ putStrLn $ "Creating test labels: " <> show labelNames
            liftTodoist config $ traverse (\name -> addLabel $ runBuilder (newLabel name) mempty) labelNames

    let deleteLabels' labelIds = do
            liftIO $ putStrLn $ "Cleaning up test labels: " <> show labelNames
            traverse_ (void . todoist config . deleteLabel) labelIds

    let runAction labelIds = void $ assertSucceeds $ action labelIds

    bracket (assertSucceeds createLabels) deleteLabels' runAction
