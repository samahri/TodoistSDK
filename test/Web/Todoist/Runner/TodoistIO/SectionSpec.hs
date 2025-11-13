{-# LANGUAGE DisambiguateRecordFields #-}

module Web.Todoist.Runner.TodoistIO.SectionSpec (spec) where

import Data.Aeson (decode, eitherDecode)
import Data.Either (Either, isRight)
import Data.Maybe (Maybe (Just, Nothing), fromJust, isJust)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Data.Bool (Bool (False))
import Data.Function (($))
import Data.String (String)
import Web.Todoist.Domain.Section (Section (..), SectionId (..))
import Web.Todoist.Domain.Types (IsCollapsed (..), Name (..), Order (..), ProjectId (..))
import Web.Todoist.Internal.Types (SectionResponse (..), TodoistReturn (..))
import Web.Todoist.TestHelpers
    ( sampleSection
    , sampleSectionResponseJson
    , sampleSectionsJson
    )

spec :: Spec
spec = do
    describe "SectionResponse JSON parsing" $ do
        it "parses valid SectionResponse JSON" $ do
            let result = eitherDecode sampleSectionResponseJson :: Either String SectionResponse
            result `shouldSatisfy` isRight

        it "correctly parses all fields from JSON" $ do
            let decoded = decode sampleSectionResponseJson :: Maybe SectionResponse
            decoded `shouldSatisfy` isJust
            let response = fromJust decoded
                SectionResponse
                    { p_id = pId
                    , p_name = pName
                    , p_project_id = pProjectId
                    , p_section_order = pOrder
                    , p_is_collapsed = pCollapsed
                    , p_is_archived = pArchived
                    , p_is_deleted = pDeleted
                    , p_updated_at = pUpdatedAt
                    , p_archived_at = pArchivedAt
                    } = response
            pId `shouldBe` ("section123" :: Text)
            pName `shouldBe` ("Test Section" :: Text)
            pProjectId `shouldBe` ("project789" :: Text)
            pOrder `shouldBe` 1
            pCollapsed `shouldBe` False
            pArchived `shouldBe` False
            pDeleted `shouldBe` False
            pUpdatedAt `shouldBe` Just "2024-01-02T14:30:00Z"
            pArchivedAt `shouldBe` Nothing

    describe "TodoistReturn SectionResponse JSON parsing" $ do
        it "parses paginated sections response" $ do
            let result = eitherDecode sampleSectionsJson :: Either String (TodoistReturn SectionResponse)
            result `shouldSatisfy` isRight

        it "correctly parses results and cursor" $ do
            let decoded = decode sampleSectionsJson :: Maybe (TodoistReturn SectionResponse)
            decoded `shouldSatisfy` isJust
            let response = fromJust decoded
                TodoistReturn {next_cursor = cursor} = response
            cursor `shouldBe` Nothing

    describe "SectionId JSON serialization" $ do
        it "serializes SectionId correctly" $ do
            let encoded = eitherDecode "{\"id\":\"section123\"}" :: Either String SectionId
            encoded `shouldSatisfy` isRight
            let sectionId = fromJust $ decode "{\"id\":\"section123\"}"
                SectionId {_id = sidId} = sectionId
            sidId `shouldBe` ("section123" :: Text)

    describe "Section domain type" $ do
        it "has correct field values" $ do
            let Section
                    { _id = secId
                    , _name = secName
                    , _project_id = secProjId
                    , _order = secOrder
                    , _is_collapsed = secCollapsed
                    } = sampleSection
            secId `shouldBe` SectionId {_id = "section123"}
            secName `shouldBe` Name "Test Section"
            secProjId `shouldBe` ProjectId "project789"
            secOrder `shouldBe` Order 1
            secCollapsed `shouldBe` IsCollapsed False
