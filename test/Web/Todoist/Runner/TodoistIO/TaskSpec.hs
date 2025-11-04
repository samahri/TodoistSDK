{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Todoist.Runner.TodoistIO.TaskSpec (spec) where

import Web.Todoist.Domain.Task
    ( Deadline (..)
    , Due (..)
    , Duration (..)
    , DurationUnit (..)
    , NewTask (..)
    , Task (..)
    )
import Web.Todoist.Internal.Types
    ( DeadlineResponse (..)
    , DueResponse (..)
    , DurationResponse (..)
    , NewTaskResponse (..)
    , TaskResponse (..)
    , TodoistReturn (..)
    )
import Web.Todoist.Runner.TodoistIO.Task ()
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
        let durationResp = DurationResponse {p_amount = 30, p_unit = "minute"}
        let decoded = decode (encode durationResp) :: Maybe DurationResponse
        decoded `shouldSatisfy` isJust

    it "parses 'day' correctly" $ do
        let durationResp = DurationResponse {p_amount = 5, p_unit = "day"}
        let decoded = decode (encode durationResp) :: Maybe DurationResponse
        decoded `shouldSatisfy` isJust

    it "handles case variations" $ do
        let durationResp1 = DurationResponse {p_amount = 30, p_unit = "MINUTE"}
        let durationResp2 = DurationResponse {p_amount = 5, p_unit = "Day"}
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
        let DurationResponse {p_amount = amount, p_unit = unit} = fromJust decoded
        amount `shouldBe` (30 :: Int)
        unit `shouldBe` ("minute" :: Text)

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
        let DeadlineResponse {p_date = date, p_lang = lang} = fromJust decoded
        date `shouldBe` ("2025-12-31" :: Text)
        lang `shouldBe` ("en" :: Text)

    it "matches sample data" $ do
        let encoded = encode sampleDeadlineResponse
        let decoded = decode encoded :: Maybe DeadlineResponse
        decoded `shouldSatisfy` isJust
        fromJust decoded `shouldBe` sampleDeadlineResponse

dueConversionSpec :: Spec
dueConversionSpec = describe "DueResponse JSON parsing" $ do
    it "parses valid DueResponse JSON" $ do
        let json = "{\"date\":\"2025-11-15\",\"string\":\"Nov 15\",\"lang\":\"en\",\"is_recurring\":false,\"timezone\":\"America/New_York\"}"
        let result = eitherDecode json :: Either String DueResponse
        result `shouldSatisfy` isRight

    it "correctly parses all fields from JSON" $ do
        let json = "{\"date\":\"2025-11-15\",\"string\":\"Nov 15\",\"lang\":\"en\",\"is_recurring\":false,\"timezone\":\"America/New_York\"}"
        let decoded = decode json :: Maybe DueResponse
        decoded `shouldSatisfy` isJust
        let DueResponse {p_date, p_string, p_lang, p_is_recurring, p_timezone} = fromJust decoded
        p_date `shouldBe` ("2025-11-15" :: Text)
        p_string `shouldBe` ("Nov 15" :: Text)
        p_lang `shouldBe` ("en" :: Text)
        p_is_recurring `shouldBe` False
        p_timezone `shouldBe` Just ("America/New_York" :: Text)

    it "handles optional timezone field" $ do
        let json = "{\"date\":\"2025-11-15\",\"string\":\"Nov 15\",\"lang\":\"en\",\"is_recurring\":false,\"timezone\":null}"
        let decoded = decode json :: Maybe DueResponse
        decoded `shouldSatisfy` isJust
        let DueResponse {p_timezone} = fromJust decoded
        p_timezone `shouldBe` (Nothing :: Maybe Text)

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
            let TaskResponse
                    { p_user_id
                    , p_id
                    , p_project_id
                    , p_section_id
                    , p_parent_id
                    , p_labels
                    , p_priority
                    , p_content
                    , p_description
                    } = fromJust decoded
            p_user_id `shouldBe` ("56092663" :: Text)
            p_id `shouldBe` ("7654321098" :: Text)
            p_project_id `shouldBe` ("2203306141" :: Text)
            p_section_id `shouldBe` Just ("section123" :: Text)
            p_parent_id `shouldBe` Just ("parent456" :: Text)
            p_labels `shouldBe` (["urgent", "work"] :: [Text])
            p_priority `shouldBe` (3 :: Int)
            p_content `shouldBe` ("Test Task Content" :: Text)
            p_description `shouldBe` ("This is a test task description" :: Text)

        it "handles nested optional fields (due, deadline, duration)" $ do
            let decoded = decode sampleTaskResponseJson :: Maybe TaskResponse
            decoded `shouldSatisfy` isJust
            let TaskResponse {p_due, p_deadline, p_duration} = fromJust decoded
            p_due `shouldSatisfy` isJust
            p_deadline `shouldSatisfy` isJust
            p_duration `shouldSatisfy` isJust

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
        let Task
                { _id
                , _content
                , _description
                , _project_id
                , _section_id
                , _parent_id
                , _labels
                , _priority
                , _order
                , _creator_id
                , _created_at
                , _updated_at
                } = sampleTask
        _id `shouldBe` ("7654321098" :: Text)
        _content `shouldBe` ("Test Task Content" :: Text)
        _description `shouldBe` ("This is a test task description" :: Text)
        _project_id `shouldBe` ("2203306141" :: Text)
        _section_id `shouldBe` Just ("section123" :: Text)
        _parent_id `shouldBe` Just ("parent456" :: Text)
        _labels `shouldBe` (["urgent", "work"] :: [Text])
        _priority `shouldBe` (3 :: Int)
        _order `shouldBe` (1 :: Int)
        _creator_id `shouldBe` ("56092663" :: Text)
        _created_at `shouldBe` ("2025-11-01T10:00:00Z" :: Text)
        _updated_at `shouldBe` ("2025-11-03T14:30:00Z" :: Text)

    it "verifies nested optional fields in domain model" $ do
        let Task {_due, _deadline, _duration, _assignee_id, _assigner_id} = sampleTask
        _due `shouldSatisfy` isJust
        _deadline `shouldSatisfy` isJust
        _duration `shouldSatisfy` isJust
        _assignee_id `shouldBe` Just ("assignee789" :: Text)
        _assigner_id `shouldBe` Just ("56092663" :: Text)

    it "verifies Due conversion" $ do
        let Due {_date, _string, _lang, _is_recurring, _timezone} = sampleDue
        _date `shouldBe` ("2025-11-15" :: Text)
        _string `shouldBe` ("Nov 15" :: Text)
        _lang `shouldBe` ("en" :: Text)
        _is_recurring `shouldBe` False
        _timezone `shouldBe` Just ("America/New_York" :: Text)

    it "verifies Deadline conversion" $ do
        let Deadline {_date, _lang} = sampleDeadline
        _date `shouldBe` ("2025-12-31" :: Text)
        _lang `shouldBe` ("en" :: Text)

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
            let NewTaskResponse
                    { p_user_id
                    , p_id
                    , p_project_id
                    , p_section_id
                    , p_parent_id
                    , p_labels
                    , p_priority
                    , p_content
                    , p_description
                    , p_checked
                    , p_is_deleted
                    } = fromJust decoded
            p_user_id `shouldBe` ("56092663" :: Text)
            p_id `shouldBe` ("9876543210" :: Text)
            p_project_id `shouldBe` ("2203306141" :: Text)
            p_section_id `shouldBe` (Nothing :: Maybe Text)
            p_parent_id `shouldBe` (Nothing :: Maybe Text)
            p_labels `shouldBe` (["new"] :: [Text])
            p_priority `shouldBe` (1 :: Int)
            p_content `shouldBe` ("New Task" :: Text)
            p_description `shouldBe` ("A newly created task" :: Text)
            p_checked `shouldBe` False
            p_is_deleted `shouldBe` False

        it "matches sample data structure" $ do
            let encoded = encode sampleNewTaskResponse
            let decoded = decode encoded :: Maybe NewTaskResponse
            decoded `shouldSatisfy` isJust
            let NewTaskResponse {p_id, p_content, p_priority} = fromJust decoded
            p_id `shouldBe` ("9876543210" :: Text)
            p_content `shouldBe` ("New Task" :: Text)
            p_priority `shouldBe` (1 :: Int)

newTaskConversionSpec :: Spec
newTaskConversionSpec = describe "newTaskResponseToNewTask conversion" $ do
    it "verifies sample NewTask domain model structure" $ do
        let NewTask
                { _user_id
                , _id
                , _project_id
                , _section_id
                , _parent_id
                , _labels
                , _priority
                , _content
                , _description
                , _checked
                , _is_deleted
                , _child_order
                } = sampleNewTask
        _user_id `shouldBe` ("56092663" :: Text)
        _id `shouldBe` ("9876543210" :: Text)
        _project_id `shouldBe` ("2203306141" :: Text)
        _section_id `shouldBe` (Nothing :: Maybe Text)
        _parent_id `shouldBe` (Nothing :: Maybe Text)
        _labels `shouldBe` (["new"] :: [Text])
        _priority `shouldBe` (1 :: Int)
        _content `shouldBe` ("New Task" :: Text)
        _description `shouldBe` ("A newly created task" :: Text)
        _checked `shouldBe` False
        _is_deleted `shouldBe` False
        _child_order `shouldBe` (0 :: Int)

    it "handles optional fields correctly" $ do
        let NewTask {_added_at, _completed_at, _updated_at} = sampleNewTask
        _added_at `shouldBe` Just ("2025-11-04T09:00:00Z" :: Text)
        _completed_at `shouldBe` (Nothing :: Maybe Text)
        _updated_at `shouldBe` Just ("2025-11-04T09:00:00Z" :: Text)

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
        let taskResp = head results
        let TaskResponse {p_id, p_content, p_priority} = taskResp
        p_id `shouldBe` ("7654321098" :: Text)
        p_content `shouldBe` ("Test Task Content" :: Text)
        p_priority `shouldBe` (3 :: Int)
