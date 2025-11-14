{-# LANGUAGE DisambiguateRecordFields #-}

module SectionIntegrationSpec (spec) where

import Control.Applicative (Applicative (pure), (<$>))
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Bool (Bool (..))
import Data.Function (($))
import Data.Functor (Functor (..))
import Data.List (elem)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..), (<>))
import Data.Text (Text, pack)
import Helpers
    ( assertSucceeds
    , generateUniqueName
    , getTestConfig
    , liftTodoist
    )
import System.IO (IO, putStrLn)
import Test.Hspec (Spec, describe, it, pendingWith, runIO, shouldBe)
import Text.Show (show)
import Web.Todoist.Util.Builder (runBuilder)
import qualified Web.Todoist.Domain.Project as P
import Web.Todoist.Domain.Section
    ( Section (..)
    , SectionId (..)
    , SectionParam (..)
    , SectionUpdate (..)
    , addSection
    , deleteSection
    , getSection
    , getSections
    , newSection
    , updateSection
    )
import Web.Todoist.Domain.Types (Name (..), ProjectId (..))
import Web.Todoist.Internal.Error (TodoistError)
import Web.Todoist.Runner (todoist)
import Web.Todoist.Runner.IO (TodoistConfig)

spec :: Spec
spec = do
    maybeConfig <- runIO getTestConfig
    case maybeConfig of
        Nothing ->
            it "requires TODOIST_TEST_API_TOKEN" $
                pendingWith "TODOIST_TEST_API_TOKEN not set"
        Just config -> do
            sectionLifecycleSpec config
            getSectionsSpec config
            updateSectionSpec config

sectionLifecycleSpec :: TodoistConfig -> Spec
sectionLifecycleSpec config =
    describe "Section CRUD lifecycle" $ do
        it "creates, retrieves, and deletes a section" $ do
            projectName <- pack <$> generateUniqueName "IntegTest-Section-Project"
            sectionName <- pack <$> generateUniqueName "IntegTest-Section"

            withTestProjectAndSection config projectName sectionName $ \projectId sectionId -> do
                -- Get section and verify fields
                section <- liftTodoist config (getSection sectionId)
                let Section {_name = secName, _project_id = secProjectId} = section
                liftIO $ secName `shouldBe` Name sectionName
                liftIO $ secProjectId `shouldBe` projectId

                -- Delete section
                liftTodoist config (deleteSection sectionId)

                -- Verify deletion by checking section is not in active sections list
                let params = SectionParam {project_id = Just projectId, cursor = Nothing, limit = Nothing}
                sections <- liftTodoist config (getSections params)
                let sectionIds = fmap (\(Section {_id = sid}) -> sid) sections
                liftIO $ sectionId `elem` sectionIds `shouldBe` False

getSectionsSpec :: TodoistConfig -> Spec
getSectionsSpec config =
    describe "Get multiple sections" $ do
        it "creates multiple sections and retrieves them with getSections" $ do
            projectName <- pack <$> generateUniqueName "IntegTest-GetSections-Project"

            withTestProject config projectName $ \projectId -> do
                let ProjectId {getProjectId = projId} = projectId

                -- Create 3 sections
                section1Name <- pack <$> liftIO (generateUniqueName "Section1")
                section2Name <- pack <$> liftIO (generateUniqueName "Section2")
                section3Name <- pack <$> liftIO (generateUniqueName "Section3")

                sectionId1 <- liftTodoist config (addSection $ runBuilder (newSection section1Name projId) mempty)
                sectionId2 <- liftTodoist config (addSection $ runBuilder (newSection section2Name projId) mempty)
                sectionId3 <- liftTodoist config (addSection $ runBuilder (newSection section3Name projId) mempty)

                -- Get sections for project
                let params = SectionParam {project_id = Just projectId, cursor = Nothing, limit = Nothing}
                sections <- liftTodoist config (getSections params)

                -- Verify count and names
                let sectionNames = fmap (\(Section {_name = n}) -> n) sections
                liftIO $ Name section1Name `elem` sectionNames `shouldBe` True
                liftIO $ Name section2Name `elem` sectionNames `shouldBe` True
                liftIO $ Name section3Name `elem` sectionNames `shouldBe` True

                -- Cleanup
                liftTodoist config (deleteSection sectionId1)
                liftTodoist config (deleteSection sectionId2)
                liftTodoist config (deleteSection sectionId3)

updateSectionSpec :: TodoistConfig -> Spec
updateSectionSpec config =
    describe "Update section" $ do
        it "updates a section name" $ do
            projectName <- pack <$> generateUniqueName "IntegTest-Update-Project"
            sectionName <- pack <$> generateUniqueName "IntegTest-Update-Section"

            withTestProjectAndSection config projectName sectionName $ \_ sectionId -> do
                -- Update section name
                let newName = sectionName <> "-Updated"
                    update = SectionUpdate {_name = Just (Name newName)}
                updatedSection <- liftTodoist config (updateSection sectionId update)

                -- Verify update
                let Section {_name = updatedName, _id = updatedId} = updatedSection
                liftIO $ updatedName `shouldBe` Name newName
                liftIO $ updatedId `shouldBe` sectionId

-- | Bracket helper for creating test project
withTestProject :: TodoistConfig -> Text -> (ProjectId -> ExceptT TodoistError IO a) -> IO ()
withTestProject config projectName action = do
    let createProject = do
            liftIO $ putStrLn $ "Creating test project: " <> show projectName
            liftTodoist config (P.addProject $ runBuilder (P.newProject projectName) mempty)

    let deleteProject' projectId = do
            liftIO $ putStrLn $ "Cleaning up test project: " <> show projectName
            void $ todoist config (P.deleteProject projectId)

    let runAction projectId = void $ assertSucceeds $ action projectId

    bracket (assertSucceeds createProject) deleteProject' runAction

-- | Bracket helper for creating test project and section
withTestProjectAndSection ::
    TodoistConfig ->
    Text ->
    Text ->
    (ProjectId -> SectionId -> ExceptT TodoistError IO a) ->
    IO ()
withTestProjectAndSection config projectName sectionName action = do
    let createResources = do
            liftIO $
                putStrLn $
                    "Creating test project and section: " <> show projectName <> " / " <> show sectionName
            projectId <- liftTodoist config (P.addProject $ runBuilder (P.newProject projectName) mempty)
            let ProjectId {getProjectId = projId} = projectId
                sectionCreate = runBuilder (newSection sectionName projId) mempty
            sectionId <- liftTodoist config (addSection sectionCreate)
            pure (projectId, sectionId)

    let deleteResources (projectId, sectionId) = do
            liftIO $
                putStrLn $
                    "Cleaning up test section and project: " <> show sectionName <> " / " <> show projectName
            void $ todoist config (deleteSection sectionId)
            void $ todoist config (P.deleteProject projectId)

    let runAction (projectId, sectionId) = void $ assertSucceeds $ action projectId sectionId

    bracket (assertSucceeds createResources) deleteResources runAction
