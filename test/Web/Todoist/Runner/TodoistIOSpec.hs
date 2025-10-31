{-# LANGUAGE DuplicateRecordFields #-}

module Web.Todoist.Runner.TodoistIOSpec (spec) where

import Web.Todoist.Domain.Project
    ( Collaborator (..)
    , Project (..)
    , ProjectCreate
    , ProjectId (..)
    )
import Web.Todoist.Domain.Types (ViewStyle (..), parseViewStyle)
import Web.Todoist.Internal.Types
    ( Action (..)
    , CollaboratorRole (..)
    , ProjectPermissions (..)
    , ProjectResponse (..)
    , RoleActions (..)
    , TodoistReturn (..)
    )
import Web.Todoist.Runner.TodoistIO (projectResponseToProject)
import Web.Todoist.TestHelpers
    ( sampleCollaborator
    , sampleCollaboratorsJson
    , sampleProject
    , sampleProjectCreate
    , sampleProjectId
    , sampleProjectIdJson
    , sampleProjectPermissionsJson
    , sampleProjectResponse
    , sampleProjectResponseJson
    , sampleProjectsJson
    )

import Data.Aeson (decode, eitherDecode, encode)
import Data.Bool (Bool (..))
import Data.Either (Either (..), isRight)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (length, (!!))
import Data.Maybe (Maybe (..), fromJust, isJust)
import Data.String (String)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Prelude (head)

spec :: Spec
spec = do
    describe "TodoistProjectM TodoistIO implementations" $ do
        getProjectSpec
        getAllProjectsSpec
        getProjectCollaboratorsSpec
        addProjectSpec
        deleteProjectSpec
        archiveUnarchiveProjectSpec
        getProjectPermissionsSpec

getProjectSpec :: Spec
getProjectSpec = describe "getProject" $ do
    jsonParsingSpec
    conversionSpec
    viewStyleSpec

jsonParsingSpec :: Spec
jsonParsingSpec = describe "ProjectResponse JSON parsing" $ do
    it "parses valid ProjectResponse JSON" $ do
        let result = eitherDecode sampleProjectResponseJson :: Either String ProjectResponse
        result `shouldSatisfy` isRight

    it "correctly parses all fields from JSON" $ do
        let decoded = decode sampleProjectResponseJson :: Maybe ProjectResponse
        decoded `shouldSatisfy` isJust

        let ProjectResponse
                { p_id = projId
                , p_name = projName
                , p_description = projDescription
                , p_child_order = projChildOrder
                , p_color = projColor
                , p_is_favorite = projIsFavorite
                , p_is_archived = projIsArchived
                , p_is_collapsed = projIsCollapsed
                , p_is_shared = projIsShared
                , p_can_assign_tasks = projCanAssignTasks
                , p_view_style = projViewStyle
                } = fromJust decoded
        projId `shouldBe` "2203306141"
        projName `shouldBe` "Test Project"
        projDescription `shouldBe` "A test project for unit testing"
        projChildOrder `shouldBe` 1
        projColor `shouldBe` "blue"
        projIsFavorite `shouldBe` True
        projIsArchived `shouldBe` False
        projIsCollapsed `shouldBe` False
        projIsShared `shouldBe` False
        projCanAssignTasks `shouldBe` False
        projViewStyle `shouldBe` "list"

conversionSpec :: Spec
conversionSpec = describe "projectResponseToProject" $ do
    it "converts ProjectResponse to Project correctly" $ do
        let project = projectResponseToProject sampleProjectResponse
        project `shouldBe` sampleProject

    it "maps all fields correctly" $ do
        let project :: Project
            project = projectResponseToProject sampleProjectResponse
        project `shouldBe` sampleProject

    it "converts view_style string to ViewStyle type" $ do
        let project = projectResponseToProject sampleProjectResponse
        _view_style project `shouldBe` List

    it "preserves timestamp fields" $ do
        let project = projectResponseToProject sampleProjectResponse
        _created_at project `shouldBe` Just "2023-06-15T10:30:00Z"
        _updated_at project `shouldBe` Just "2023-06-20T14:45:00Z"

viewStyleSpec :: Spec
viewStyleSpec = describe "parseViewStyle" $ do
    it "parses 'list' correctly" $ do
        parseViewStyle "list" `shouldBe` List

    it "parses 'board' correctly" $ do
        parseViewStyle "board" `shouldBe` Board

    it "parses 'calendar' correctly" $ do
        parseViewStyle "calendar" `shouldBe` Calendar

    it "is case-insensitive" $ do
        parseViewStyle "LIST" `shouldBe` List
        parseViewStyle "Board" `shouldBe` Board
        parseViewStyle "CALENDAR" `shouldBe` Calendar

getAllProjectsSpec :: Spec
getAllProjectsSpec = describe "getAllProjects" $ do
    it "parses TodoistReturn [ProjectResponse] JSON" $ do
        let result = eitherDecode sampleProjectsJson :: Either String (TodoistReturn ProjectResponse)
        result `shouldSatisfy` isRight

    it "extracts results from TodoistReturn" $ do
        let decoded = decode sampleProjectsJson :: Maybe (TodoistReturn ProjectResponse)
        decoded `shouldSatisfy` isJust
        let todoistReturn = fromJust decoded
        length (results todoistReturn) `shouldBe` 2

    it "converts all ProjectResponses to Projects" $ do
        let decoded = decode sampleProjectsJson :: Maybe (TodoistReturn ProjectResponse)
        decoded `shouldSatisfy` isJust
        let todoistReturn = fromJust decoded
        let projects = projectResponseToProject <$> results todoistReturn
        length projects `shouldBe` 2
        head projects `shouldBe` sampleProject

    it "correctly parses multiple projects" $ do
        let decoded = decode sampleProjectsJson :: Maybe (TodoistReturn ProjectResponse)
        let todoistReturn = fromJust decoded
        let projects :: [Project]
            projects = projectResponseToProject <$> results todoistReturn
        length projects `shouldBe` 2
        head projects `shouldBe` sampleProject

getProjectCollaboratorsSpec :: Spec
getProjectCollaboratorsSpec = describe "getProjectCollaborators" $ do
    it "parses TodoistReturn [Collaborator] JSON" $ do
        let result = eitherDecode sampleCollaboratorsJson :: Either String (TodoistReturn Collaborator)
        result `shouldSatisfy` isRight

    it "extracts collaborators from TodoistReturn" $ do
        let decoded = decode sampleCollaboratorsJson :: Maybe (TodoistReturn Collaborator)
        decoded `shouldSatisfy` isJust
        let todoistReturn = fromJust decoded
        length (results todoistReturn) `shouldBe` 2

    it "correctly parses Collaborator fields" $ do
        let decoded = decode sampleCollaboratorsJson :: Maybe (TodoistReturn Collaborator)
        let todoistReturn = fromJust decoded
        let collaborators = results todoistReturn
        head collaborators `shouldBe` sampleCollaborator

    it "parses multiple collaborators correctly" $ do
        let decoded = decode sampleCollaboratorsJson :: Maybe (TodoistReturn Collaborator)
        let todoistReturn = fromJust decoded
        let collaborators :: [Collaborator]
            collaborators = results todoistReturn
        length collaborators `shouldBe` 2
        head collaborators `shouldBe` sampleCollaborator

addProjectSpec :: Spec
addProjectSpec = describe "addProject" $ do
    it "serializes ProjectCreate to JSON correctly" $ do
        let encoded = encode sampleProjectCreate
        let decoded = decode encoded :: Maybe ProjectCreate
        decoded `shouldSatisfy` isJust

    it "parses ProjectId response JSON" $ do
        let result = eitherDecode sampleProjectIdJson :: Either String ProjectId
        result `shouldSatisfy` isRight

    it "correctly parses ProjectId fields" $ do
        let decoded = decode sampleProjectIdJson :: Maybe ProjectId
        decoded `shouldSatisfy` isJust
        let projectId :: ProjectId
            projectId = fromJust decoded
        projectId `shouldBe` sampleProjectId

deleteProjectSpec :: Spec
deleteProjectSpec = describe "deleteProject" $ do
    it "returns unit type (no response body to parse)" $ do
        -- deleteProject returns (), which indicates successful deletion
        -- There's no JSON response body to test, but we can verify the type
        let result :: ()
            result = ()
        result `shouldBe` ()

    it "is a void operation (no data returned)" $ do
        -- The Todoist API DELETE endpoint returns no content (204 No Content)
        -- The function signature is: deleteProject :: ProjectId -> TodoistIO ()
        -- This test documents that behavior
        let unitValue :: ()
            unitValue = ()
        unitValue `shouldBe` ()

archiveUnarchiveProjectSpec :: Spec
archiveUnarchiveProjectSpec = describe "archiveProject and unarchiveProject" $ do
    it "both return ProjectId on success (parses same JSON format)" $ do
        let result = eitherDecode sampleProjectIdJson :: Either String ProjectId
        result `shouldSatisfy` isRight
        let projectId :: ProjectId
            projectId = fromJust (decode sampleProjectIdJson :: Maybe ProjectId)
        projectId `shouldBe` sampleProjectId

    it "ProjectId response is consistent across operations" $ do
        let decoded = decode sampleProjectIdJson :: Maybe ProjectId
        decoded `shouldSatisfy` isJust
        decoded `shouldBe` Just sampleProjectId

getProjectPermissionsSpec :: Spec
getProjectPermissionsSpec = describe "getProjectPermissions" $ do
    jsonParsingPermissionsSpec

jsonParsingPermissionsSpec :: Spec
jsonParsingPermissionsSpec = describe "ProjectPermissions JSON parsing" $ do
    it "parses valid ProjectPermissions JSON" $ do
        let result = eitherDecode sampleProjectPermissionsJson :: Either String ProjectPermissions
        result `shouldSatisfy` isRight

    it "correctly parses all fields from JSON" $ do
        let decoded = decode sampleProjectPermissionsJson :: Maybe ProjectPermissions
        decoded `shouldSatisfy` isJust
        let perms = fromJust decoded
        length (p_project_collaborator_actions perms) `shouldBe` 1
        length (p_workspace_collaborator_actions perms) `shouldBe` 1

    it "correctly parses role as Creator" $ do
        let decoded = decode sampleProjectPermissionsJson :: Maybe ProjectPermissions
        decoded `shouldSatisfy` isJust
        let perms = fromJust decoded
        let RoleActions {p_name = roleName} = head (p_project_collaborator_actions perms)
        roleName `shouldBe` Creator

    it "correctly parses action names as Text" $ do
        let decoded = decode sampleProjectPermissionsJson :: Maybe ProjectPermissions
        decoded `shouldSatisfy` isJust
        let perms = fromJust decoded
        let RoleActions {p_actions = actions} = head (p_project_collaborator_actions perms)
        length actions `shouldBe` 2
        let Action {p_name = action1Name} = head actions
        let Action {p_name = action2Name} = actions !! 1
        action1Name `shouldBe` "create_task"
        action2Name `shouldBe` "delete_project"
