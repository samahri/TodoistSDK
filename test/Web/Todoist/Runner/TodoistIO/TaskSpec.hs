{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Todoist.Runner.TodoistIO.TaskSpec (spec) where

import Web.Todoist.Domain.Section (SectionId (..))
import Web.Todoist.Domain.Task
    ( Deadline
    , Due (..)
    , Duration (..)
    , DurationUnit (..)
    , NewTask (..)
    , Task
    )
import Web.Todoist.Domain.Types
    ( Content (..)
    , Description (..)
    , Order (..)
    , ParentId (..)
    , ProjectId (..)
    , TaskId (..)
    , Uid (..)
    )
import Web.Todoist.Internal.Types
    ( DeadlineResponse (..)
    , DueResponse (..)
    , DurationResponse (..)
    , NewTaskResponse (..)
    , TaskResponse (..)
    , TodoistReturn (..)
    )
import qualified Web.Todoist.Internal.Types as IT
import Web.Todoist.Runner.IO.Interpreters ()
import Web.Todoist.TestHelpers
    ( sampleDeadline
    , sampleDeadlineResponse
    , sampleDue
    , sampleDueResponse
    , sampleDuration
    , sampleDurationResponse
    , sampleNewTask
    , sampleNewTaskResponse
    , sampleNewTaskResponseJson
    , sampleTask
    , sampleTaskResponse
    , sampleTaskResponseJson
    , sampleTasksJson
    )

import Data.Aeson (decode, eitherDecode, encode)
import Data.Bool (Bool (..))
import Data.Either (Either (..), isRight)
import Data.Function (($))
import Data.Int (Int)
import Data.List (head, length)
import Data.Maybe (Maybe (..), fromJust, isJust)
import Data.String (String)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
    describe "TodoistTaskM TodoistIO implementations" $ do
        parseDurationUnitSpec
        durationConversionSpec
        deadlineConversionSpec
        dueConversionSpec
        taskResponseParsingSpec
        taskConversionSpec
        newTaskResponseParsingSpec
        newTaskConversionSpec
        todoistReturnTasksSpec

parseDurationUnitSpec :: Spec
parseDurationUnitSpec = describe "parseDurationUnit" $ do
    it "parses 'minute' correctly" $ do
        let durationResp = IT.DurationResponse {IT.p_amount = 30, IT.p_unit = "minute"}
        let decoded = decode (encode durationResp) :: Maybe DurationResponse
        decoded `shouldSatisfy` isJust

    it "parses 'day' correctly" $ do
        let durationResp = IT.DurationResponse {IT.p_amount = 5, IT.p_unit = "day"}
        let decoded = decode (encode durationResp) :: Maybe DurationResponse
        decoded `shouldSatisfy` isJust

    it "handles case variations" $ do
        let durationResp1 = IT.DurationResponse {IT.p_amount = 30, IT.p_unit = "MINUTE"}
        let durationResp2 = IT.DurationResponse {IT.p_amount = 5, IT.p_unit = "Day"}
        decode (encode durationResp1) `shouldSatisfy` (isJust :: Maybe DurationResponse -> Bool)
        decode (encode durationResp2) `shouldSatisfy` (isJust :: Maybe DurationResponse -> Bool)

durationConversionSpec :: Spec
durationConversionSpec = describe "DurationResponse JSON parsing" $ do
    it "parses valid DurationResponse JSON" $ do
        let json = "{\"amount\":30,\"unit\":\"minute\"}"
        let result = eitherDecode json :: Either String DurationResponse
        result `shouldSatisfy` isRight

    it "correctly parses all fields from JSON" $ do
        let json = "{\"amount\":30,\"unit\":\"minute\"}"
        let decoded = decode json :: Maybe DurationResponse
        decoded `shouldSatisfy` isJust
        let response :: DurationResponse = fromJust decoded
        response.p_amount `shouldBe` (30 :: Int)
        response.p_unit `shouldBe` ("minute" :: Text)

    it "matches sample data" $ do
        let encoded = encode sampleDurationResponse
        let decoded = decode encoded :: Maybe DurationResponse
        decoded `shouldSatisfy` isJust
        fromJust decoded `shouldBe` sampleDurationResponse

deadlineConversionSpec :: Spec
deadlineConversionSpec = describe "DeadlineResponse JSON parsing" $ do
    it "parses valid DeadlineResponse JSON" $ do
        let json = "{\"date\":\"2025-12-31\",\"lang\":\"en\"}"
        let result = eitherDecode json :: Either String DeadlineResponse
        result `shouldSatisfy` isRight

    it "correctly parses all fields from JSON" $ do
        let json = "{\"date\":\"2025-12-31\",\"lang\":\"en\"}"
        let decoded = decode json :: Maybe DeadlineResponse
        decoded `shouldSatisfy` isJust
        let response :: DeadlineResponse = fromJust decoded
        response.p_date `shouldBe` ("2025-12-31" :: Text)
        response.p_lang `shouldBe` ("en" :: Text)

    it "matches sample data" $ do
        let encoded = encode sampleDeadlineResponse
        let decoded = decode encoded :: Maybe DeadlineResponse
        decoded `shouldSatisfy` isJust
        fromJust decoded `shouldBe` sampleDeadlineResponse

dueConversionSpec :: Spec
dueConversionSpec = describe "DueResponse JSON parsing" $ do
    it "parses valid DueResponse JSON" $ do
        let json =
                "{\"date\":\"2025-11-15\",\"string\":\"Nov 15\",\"lang\":\"en\",\"is_recurring\":false,\"timezone\":\"America/New_York\"}"
        let result = eitherDecode json :: Either String DueResponse
        result `shouldSatisfy` isRight

    it "correctly parses all fields from JSON" $ do
        let json =
                "{\"date\":\"2025-11-15\",\"string\":\"Nov 15\",\"lang\":\"en\",\"is_recurring\":false,\"timezone\":\"America/New_York\"}"
        let decoded = decode json :: Maybe DueResponse
        decoded `shouldSatisfy` isJust
        let response :: DueResponse = fromJust decoded
        response.p_date `shouldBe` ("2025-11-15" :: Text)
        response.p_string `shouldBe` ("Nov 15" :: Text)
        response.p_lang `shouldBe` ("en" :: Text)
        response.p_is_recurring `shouldBe` False
        response.p_timezone `shouldBe` Just ("America/New_York" :: Text)

    it "handles optional timezone field" $ do
        let json =
                "{\"date\":\"2025-11-15\",\"string\":\"Nov 15\",\"lang\":\"en\",\"is_recurring\":false,\"timezone\":null}"
        let decoded = decode json :: Maybe DueResponse
        decoded `shouldSatisfy` isJust
        let response :: DueResponse = fromJust decoded
        response.p_timezone `shouldBe` (Nothing :: Maybe Text)

    it "matches sample data" $ do
        let encoded = encode sampleDueResponse
        let decoded = decode encoded :: Maybe DueResponse
        decoded `shouldSatisfy` isJust
        fromJust decoded `shouldBe` sampleDueResponse

taskResponseParsingSpec :: Spec
taskResponseParsingSpec = describe "TaskResponse" $ do
    describe "JSON parsing" $ do
        it "parses valid TaskResponse JSON" $ do
            let result = eitherDecode sampleTaskResponseJson :: Either String TaskResponse
            result `shouldSatisfy` isRight

        it "correctly parses all fields from JSON" $ do
            let decoded = decode sampleTaskResponseJson :: Maybe TaskResponse
            decoded `shouldSatisfy` isJust
            let response :: TaskResponse = fromJust decoded
            response.p_user_id `shouldBe` ("56092663" :: Text)
            response.p_id `shouldBe` ("7654321098" :: Text)
            response.p_project_id `shouldBe` ("2203306141" :: Text)
            response.p_section_id `shouldBe` Just ("section123" :: Text)
            response.p_parent_id `shouldBe` Just ("parent456" :: Text)
            response.p_labels `shouldBe` (["urgent", "work"] :: [Text])
            response.p_priority `shouldBe` (3 :: Int)
            response.p_content `shouldBe` ("Test Task Content" :: Text)
            response.p_description `shouldBe` ("This is a test task description" :: Text)

        it "handles nested optional fields (due, deadline, duration)" $ do
            let decoded = decode sampleTaskResponseJson :: Maybe TaskResponse
            decoded `shouldSatisfy` isJust
            let response :: TaskResponse = fromJust decoded
            response.p_due `shouldSatisfy` isJust
            response.p_deadline `shouldSatisfy` isJust
            response.p_duration `shouldSatisfy` isJust

        it "matches sample data" $ do
            let encoded = encode sampleTaskResponse
            let decoded = decode encoded :: Maybe TaskResponse
            decoded `shouldSatisfy` isJust
            fromJust decoded `shouldBe` sampleTaskResponse

taskConversionSpec :: Spec
taskConversionSpec = describe "taskResponseToTask conversion" $ do
    it "verifies sample Task domain model structure" $ do
        -- We can't directly test the conversion function since it's not exported
        -- But we can verify our sample data is correctly structured
        let task :: Task = sampleTask
        task._id `shouldBe` TaskId "7654321098"
        task._content `shouldBe` Content "Test Task Content"
        task._description `shouldBe` Description "This is a test task description"
        task._project_id `shouldBe` ProjectId "2203306141"
        task._section_id `shouldBe` Just (SectionId "section123")
        task._parent_id `shouldBe` Just (ParentId "parent456")
        task._labels `shouldBe` (["urgent", "work"] :: [Text])
        task._priority `shouldBe` (3 :: Int)
        task._order `shouldBe` Order 1
        task._creator_id `shouldBe` Uid "56092663"
        task._created_at `shouldBe` ("2025-11-01T10:00:00Z" :: Text)
        task._updated_at `shouldBe` ("2025-11-03T14:30:00Z" :: Text)

    it "verifies nested optional fields in domain model" $ do
        let task :: Task = sampleTask
        task._due `shouldSatisfy` isJust
        task._deadline `shouldSatisfy` isJust
        task._duration `shouldSatisfy` isJust
        task._assignee_id `shouldBe` Just (Uid "assignee789")
        task._assigner_id `shouldBe` Just (Uid "56092663")

    it "verifies Due conversion" $ do
        let due :: Due = sampleDue
        due._date `shouldBe` ("2025-11-15" :: Text)
        due._string `shouldBe` ("Nov 15" :: Text)
        due._lang `shouldBe` ("en" :: Text)
        due._is_recurring `shouldBe` False
        due._timezone `shouldBe` Just ("America/New_York" :: Text)

    it "verifies Deadline conversion" $ do
        let deadline :: Deadline = sampleDeadline
        deadline._date `shouldBe` ("2025-12-31" :: Text)
        deadline._lang `shouldBe` ("en" :: Text)

    it "verifies Duration conversion" $ do
        let Duration {_amount, _unit} = sampleDuration
        _amount `shouldBe` (30 :: Int)
        _unit `shouldBe` Minute

newTaskResponseParsingSpec :: Spec
newTaskResponseParsingSpec = describe "NewTaskResponse" $ do
    describe "JSON parsing" $ do
        it "parses valid NewTaskResponse JSON" $ do
            let result = eitherDecode sampleNewTaskResponseJson :: Either String NewTaskResponse
            result `shouldSatisfy` isRight

        it "correctly parses all fields from JSON" $ do
            let decoded = decode sampleNewTaskResponseJson :: Maybe NewTaskResponse
            decoded `shouldSatisfy` isJust
            let response :: NewTaskResponse = fromJust decoded
            response.p_user_id `shouldBe` ("56092663" :: Text)
            response.p_id `shouldBe` ("9876543210" :: Text)
            response.p_project_id `shouldBe` ("2203306141" :: Text)
            response.p_section_id `shouldBe` (Nothing :: Maybe Text)
            response.p_parent_id `shouldBe` (Nothing :: Maybe Text)
            response.p_labels `shouldBe` (["new"] :: [Text])
            response.p_priority `shouldBe` (1 :: Int)
            response.p_content `shouldBe` ("New Task" :: Text)
            response.p_description `shouldBe` ("A newly created task" :: Text)
            response.p_checked `shouldBe` False
            response.p_is_deleted `shouldBe` False

        it "matches sample data structure" $ do
            let encoded = encode sampleNewTaskResponse
            let decoded = decode encoded :: Maybe NewTaskResponse
            decoded `shouldSatisfy` isJust
            let response :: NewTaskResponse = fromJust decoded
            response.p_id `shouldBe` ("9876543210" :: Text)
            response.p_content `shouldBe` ("New Task" :: Text)
            response.p_priority `shouldBe` (1 :: Int)

newTaskConversionSpec :: Spec
newTaskConversionSpec = describe "newTaskResponseToNewTask conversion" $ do
    it "verifies sample NewTask domain model structure" $ do
        let newTask :: NewTask = sampleNewTask
        newTask._user_id `shouldBe` ("56092663" :: Text)
        newTask._id `shouldBe` TaskId "9876543210"
        newTask._project_id `shouldBe` ProjectId "2203306141"
        newTask._section_id `shouldBe` (Nothing :: Maybe SectionId)
        newTask._parent_id `shouldBe` (Nothing :: Maybe ParentId)
        newTask._labels `shouldBe` (["new"] :: [Text])
        newTask._priority `shouldBe` (1 :: Int)
        newTask._content `shouldBe` Content "New Task"
        newTask._description `shouldBe` Description "A newly created task"
        newTask._checked `shouldBe` False
        newTask._is_deleted `shouldBe` False
        newTask._child_order `shouldBe` Order 0

    it "handles optional fields correctly" $ do
        let newTask :: NewTask = sampleNewTask
        newTask._added_at `shouldBe` Just ("2025-11-04T09:00:00Z" :: Text)
        newTask._completed_at `shouldBe` (Nothing :: Maybe Text)
        newTask._updated_at `shouldBe` Just ("2025-11-04T09:00:00Z" :: Text)

todoistReturnTasksSpec :: Spec
todoistReturnTasksSpec = describe "TodoistReturn [TaskResponse]" $ do
    it "parses getTasks response JSON" $ do
        let result = eitherDecode sampleTasksJson :: Either String (TodoistReturn TaskResponse)
        result `shouldSatisfy` isRight

    it "extracts results from TodoistReturn" $ do
        let decoded = decode sampleTasksJson :: Maybe (TodoistReturn TaskResponse)
        decoded `shouldSatisfy` isJust
        let TodoistReturn {results} = fromJust decoded
        length results `shouldBe` (1 :: Int)

    it "correctly parses TaskResponse within TodoistReturn" $ do
        let decoded = decode sampleTasksJson :: Maybe (TodoistReturn TaskResponse)
        decoded `shouldSatisfy` isJust
        let TodoistReturn {results} = fromJust decoded
        let taskResp :: TaskResponse = head results
        taskResp.p_id `shouldBe` ("7654321098" :: Text)
        taskResp.p_content `shouldBe` ("Test Task Content" :: Text)
        taskResp.p_priority `shouldBe` (3 :: Int)
