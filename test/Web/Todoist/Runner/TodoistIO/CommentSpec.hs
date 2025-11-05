{-# LANGUAGE RecordWildCards #-}

module Web.Todoist.Runner.TodoistIO.CommentSpec (spec) where

import Web.Todoist.Domain.Comment (Comment (..))
import Web.Todoist.Internal.Types (CommentResponse (..))
import Web.Todoist.Runner.TodoistIO.Comment (commentResponseToComment)
import Web.Todoist.TestHelpers
    ( sampleCommentResponse
    , sampleCommentResponseJson
    , sampleCommentResponseWithAttachment
    , sampleCommentResponseWithAttachmentJson
    )

import Data.Aeson (decode, eitherDecode)
import Data.Bool (Bool (False))
import Data.Either (Either (Right), isLeft, isRight)
import Data.Function (($))
import Data.Maybe (Maybe (..), fromJust, isJust)
import Data.String (String)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
    describe "Comment JSON Parsing" $ do
        commentResponseJsonParsingSpec
        commentResponseWithAttachmentJsonParsingSpec
        commentResponseToCommentSpec
        commentResponseToCommentValidationSpec

commentResponseJsonParsingSpec :: Spec
commentResponseJsonParsingSpec = describe "CommentResponse JSON parsing" $ do
    it "parses CommentResponse JSON correctly" $ do
        let decodedEither = eitherDecode sampleCommentResponseJson :: Either String CommentResponse
        decodedEither `shouldSatisfy` isRight

        let decoded = decode sampleCommentResponseJson :: Maybe CommentResponse
        decoded `shouldSatisfy` isJust

        let CommentResponse {..} = fromJust decoded
        p_id `shouldBe` "3012345678"
        p_content `shouldBe` "This is a test comment"
        p_posted_uid `shouldBe` Just "2671355"
        p_posted_at `shouldBe` Just "2023-10-15T14:30:00Z"
        p_item_id `shouldBe` Nothing
        p_project_id `shouldBe` Just "2203306141"
        p_file_attachment `shouldBe` Nothing
        p_is_deleted `shouldBe` False

commentResponseWithAttachmentJsonParsingSpec :: Spec
commentResponseWithAttachmentJsonParsingSpec = describe "CommentResponse with attachment JSON parsing" $ do
    it "parses CommentResponse with attachment JSON correctly" $ do
        let decodedEither = eitherDecode sampleCommentResponseWithAttachmentJson :: Either String CommentResponse
        decodedEither `shouldSatisfy` isRight

        let decoded = decode sampleCommentResponseWithAttachmentJson :: Maybe CommentResponse
        decoded `shouldSatisfy` isJust

        let CommentResponse {..} = fromJust decoded
        p_id `shouldBe` "3012345679"
        p_content `shouldBe` "Comment with attachment"
        p_file_attachment `shouldSatisfy` isJust
        p_item_id `shouldBe` Just "2995104339"
        p_project_id `shouldBe` Nothing

commentResponseToCommentSpec :: Spec
commentResponseToCommentSpec = describe "commentResponseToComment conversion" $ do
    it "converts CommentResponse to Comment correctly" $ do
        let result = commentResponseToComment sampleCommentResponse
        result `shouldSatisfy` isRight

        let Right (Comment {..}) = result
        _id `shouldBe` "3012345678"
        _content `shouldBe` "This is a test comment"
        _poster_id `shouldBe` Just "2671355"
        _posted_at `shouldBe` Just "2023-10-15T14:30:00Z"
        _task_id `shouldBe` Nothing
        _project_id `shouldBe` Just "2203306141"
        _attachment `shouldBe` Nothing

    it "converts CommentResponse with attachment to Comment correctly" $ do
        let result = commentResponseToComment sampleCommentResponseWithAttachment
        result `shouldSatisfy` isRight

        let Right (Comment {..}) = result
        _id `shouldBe` "3012345679"
        _task_id `shouldBe` Just "2995104339"
        _project_id `shouldBe` Nothing
        _attachment `shouldSatisfy` isJust

commentResponseToCommentValidationSpec :: Spec
commentResponseToCommentValidationSpec = describe "commentResponseToComment validation" $ do
    it "fails conversion when both task_id and project_id are Nothing" $ do
        let invalidResponse = sampleCommentResponse {p_item_id = Nothing, p_project_id = Nothing}
            result = commentResponseToComment invalidResponse
        result `shouldSatisfy` isLeft
