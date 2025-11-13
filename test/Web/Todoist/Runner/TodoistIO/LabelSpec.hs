{-# LANGUAGE DisambiguateRecordFields #-}

module Web.Todoist.Runner.TodoistIO.LabelSpec (spec) where

import Data.Aeson (decode, eitherDecode)
import Data.Bool (Bool (False))
import Data.Either (Either, isRight)
import Data.Function (($))
import Data.Maybe (Maybe (Just, Nothing), fromJust, isJust)
import Data.String (String)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Web.Todoist.Domain.Label (Label (..), LabelId (..))
import Web.Todoist.Domain.Types (Color (..), IsFavorite (..), Name (..), Order (..))
import Web.Todoist.Internal.Types (LabelResponse (..), TodoistReturn (..))
import Web.Todoist.TestHelpers
    ( sampleLabel
    , sampleLabelResponseJson
    , sampleLabelsJson
    , sampleSharedLabelsJson
    )

spec :: Spec
spec = do
    describe "LabelResponse JSON parsing" $ do
        it "parses valid LabelResponse JSON" $ do
            let result = eitherDecode sampleLabelResponseJson :: Either String LabelResponse
            result `shouldSatisfy` isRight

        it "correctly parses all fields from JSON" $ do
            let decoded = decode sampleLabelResponseJson :: Maybe LabelResponse
            decoded `shouldSatisfy` isJust
            let response = fromJust decoded
                LabelResponse
                    { p_id = pId
                    , p_name = pName
                    , p_color = pColor
                    , p_order = pOrder
                    , p_is_favorite = pFavorite
                    } = response
            pId `shouldBe` ("label123" :: Text)
            pName `shouldBe` ("Test Label" :: Text)
            pColor `shouldBe` ("charcoal" :: Text)
            pOrder `shouldBe` Just 1
            pFavorite `shouldBe` False

    describe "TodoistReturn LabelResponse JSON parsing" $ do
        it "parses paginated labels response" $ do
            let result = eitherDecode sampleLabelsJson :: Either String (TodoistReturn LabelResponse)
            result `shouldSatisfy` isRight

        it "correctly parses results and cursor" $ do
            let decoded = decode sampleLabelsJson :: Maybe (TodoistReturn LabelResponse)
            decoded `shouldSatisfy` isJust
            let response = fromJust decoded
                TodoistReturn {next_cursor = cursor} = response
            cursor `shouldBe` Nothing

    describe "TodoistReturn Text JSON parsing (shared labels)" $ do
        it "parses shared labels response" $ do
            let result = eitherDecode sampleSharedLabelsJson :: Either String (TodoistReturn Text)
            result `shouldSatisfy` isRight

        it "correctly parses string array results" $ do
            let decoded = decode sampleSharedLabelsJson :: Maybe (TodoistReturn Text)
            decoded `shouldSatisfy` isJust
            let response = fromJust decoded
                TodoistReturn {results = names, next_cursor = cursor} = response
            names `shouldBe` ["Label1", "Label2", "Label3"]
            cursor `shouldBe` Nothing

    describe "LabelId JSON serialization" $ do
        it "deserializes LabelId correctly from object" $ do
            let encoded = eitherDecode "{\"id\":\"label123\"}" :: Either String LabelId
            encoded `shouldSatisfy` isRight
            let labelId = fromJust $ decode "{\"id\":\"label123\"}"
                LabelId {getLabelId = lidId} = labelId
            lidId `shouldBe` ("label123" :: Text)

    describe "Label domain type" $ do
        it "has correct field values" $ do
            let Label
                    { _id = labId
                    , _name = labName
                    , _color = labColor
                    , _order = labOrder
                    , _is_favorite = labFavorite
                    } = sampleLabel
            labId `shouldBe` LabelId "label123"
            labName `shouldBe` Name "Test Label"
            labColor `shouldBe` Color "charcoal"
            labOrder `shouldBe` Just (Order 1)
            labFavorite `shouldBe` IsFavorite False
