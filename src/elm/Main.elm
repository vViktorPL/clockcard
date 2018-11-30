port module Main exposing (main)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (id)
import IssueList exposing (..)
import Timesheet
import Json.Decode exposing (Decoder, decodeValue, field, at)
import Json.Encode exposing (Value, int)
import Time exposing (Posix)
import Integrations
import Task


port save : Value -> Cmd msg


type Msg
    = TimesheetMsg Timesheet.Msg
    | IssueListMsg IssueList.Msg
    | IntegrationConfigManagerMsg Integrations.ConfigsMsg
    | Tick Posix


type alias Model =
    { issues : IssueList.Model
    , integrations: Integrations.Configs
    , currentTime : Posix
    }


type alias IssueId =
    Int


type alias Flags =
    Value


getCurrentTimesheet : Model -> Timesheet.Model
getCurrentTimesheet model =
    model.issues
        |> IssueList.getSelectedIssue
        |> .timesheet

view : Model -> Html Msg
view model =
    div
        [ id "container" ]
        [ Html.map IssueListMsg (IssueList.view model.issues)
        , Html.map TimesheetMsg (Timesheet.view model.integrations (getCurrentTimesheet model) model.currentTime)
        , Html.map IntegrationConfigManagerMsg (Integrations.viewConfigManagers model.integrations)
        ]


encodeState : Model -> Value
encodeState model =
    Json.Encode.object
        [ ( "version", int 1 )
        , ( "issues", IssueList.normalize model.issues )
        , ( "integrations", Integrations.encodeConfigs model.integrations )
        ]


decoder : Decoder Model
decoder =
    Json.Decode.map3 Model
        ( field "issues" IssueList.decoder )
        ( field "integrations" Integrations.decodeConfigs )
        ( Json.Decode.succeed ( Time.millisToPosix 0 ) )


saveNormalized model =
    model |> encodeState |> save


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IssueListMsg submsg ->
            let
                ( updatedIssues, cmd ) =
                    IssueList.update submsg model.issues

                wrappedCmd =
                    Cmd.map IssueListMsg cmd

                updatedModel =
                    { model | issues = updatedIssues }
            in
            ( updatedModel
            , case submsg of
                IssueList.SelectIssue _ ->
                    Cmd.batch [ wrappedCmd, refreshTime ]

                IssueList.NewIssue ->
                    Cmd.batch [ wrappedCmd, saveNormalized updatedModel ]

                _ ->
                    wrappedCmd
            )

        Tick time ->
            ( { model | currentTime = time }, Cmd.none )

        TimesheetMsg submsg ->
            let
                selectedIssue = getSelectedIssue model.issues
                (updatedTimesheet, cmd) = Timesheet.update model.integrations submsg selectedIssue.timesheet
                wrappedCmd = Cmd.map TimesheetMsg cmd
                updatedIssue = { selectedIssue | timesheet = updatedTimesheet }
                updatedModel = { model | issues = IssueList.updateSelectedIssue model.issues updatedIssue }
            in
                ( updatedModel
                , case Timesheet.saveStateAdvised submsg of
                    True -> Cmd.batch [ wrappedCmd, saveNormalized updatedModel ]
                    False -> wrappedCmd
                )

        IntegrationConfigManagerMsg submsg ->
            let
                ( newIntegrationModel, integrationCmd)  = (Integrations.updateConfigManagers submsg model.integrations)
                wrappedCmd = Cmd.map IntegrationConfigManagerMsg integrationCmd
                updatedModel = { model | integrations = newIntegrationModel }
            in
                ( updatedModel
                , case Integrations.configsSaveAdvised submsg of
                    True -> Cmd.batch [ wrappedCmd, saveNormalized updatedModel ]
                    False -> wrappedCmd
                )


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( case decodeValue decoder flags of
        Ok model ->
            model

        Err _ ->
            { issues = IssueList.init
            , integrations = Integrations.initConfigs
            , currentTime = Time.millisToPosix 0
            }

    , refreshTime
    )

refreshTime : Cmd Msg
refreshTime = Task.perform Tick Time.now


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ (Time.every 1000 Tick)
                    , (Sub.map IntegrationConfigManagerMsg Integrations.subscriptions)
                    ]
        }
