port module Main exposing (main)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (class, id, style)
import Integrations
import IssueList exposing (..)
import Json.Decode exposing (Decoder, at, decodeValue, field)
import Json.Encode exposing (Value, int)
import Task
import Time exposing (Posix)
import Timesheet
import Http


type Msg
    = TimesheetMsg Timesheet.Msg
    | IssueListMsg IssueList.Msg
    | IssueListFetched (Result Http.Error IssueList.Model)
    | IntegrationConfigManagerMsg Integrations.ConfigsMsg
    | Tick Posix


type alias Model =
    { issues : Maybe IssueList.Model
    , integrations : Integrations.Configs
    , currentTime : Posix
    }


type alias IssueId =
    Int


type alias Flags =
    Value



view : Model -> Html Msg
view model =
    div
        [ id "container" ]
        ( case model.issues of
            Just issues ->
                [ Html.map IssueListMsg (IssueList.view issues)
                , viewIssuePanel issues model.integrations model.currentTime
                , Html.map IntegrationConfigManagerMsg (Integrations.viewConfigManagers model.integrations)
                ]

            Nothing ->
                [ Html.div [ class "big-placeholder-message" ]
                      [ Html.div [ class "big-icon" ] [ Html.text "⌛" ]
                      , Html.strong [] [ Html.text "Loading issues..." ]
                      ]
                ]
        )


viewIssuePanel : IssueList.Model -> Integrations.Configs -> Posix -> Html Msg
viewIssuePanel issues integrations currentTime =
    case getSelectedIssueTimesheet issues of
        Ok currentTimesheet ->
            Html.map TimesheetMsg (Timesheet.view integrations currentTimesheet currentTime)

        Err IssueList.ListIsEmpty ->
            Html.div [ class "big-placeholder-message" ]
                [ Html.div [ class "big-icon" ] [ Html.text "👋" ]
                , Html.strong [] [ Html.text "Hello there!" ]
                , Html.p []
                    [ Html.text "To create your first time-tracked issue"
                    , Html.br [] []
                    , Html.text "please click \""
                    , Html.strong [] [ Html.text "+ New issue" ]
                    , Html.text "\" on the left pane."
                    ]
                ]

        Err IssueList.FetchingIssueDetailsInProgress ->
            Html.div [ class "big-placeholder-message" ]
                [ Html.div [ class "big-icon" ] [ Html.text "⌛" ]
                , Html.strong [] [ Html.text "Loading issue details..." ]
                ]




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model.issues) of
        (IssueListFetched (Ok issues), Nothing) ->
            ( { model | issues = Just issues }
            , Cmd.none
            )

        (IssueListMsg submsg, Just issues) ->
            let
                ( updatedIssues, cmd ) =
                    IssueList.update submsg issues

                wrappedCmd =
                    Cmd.map IssueListMsg cmd

                updatedModel =
                    { model | issues = Just updatedIssues }
            in
            ( updatedModel
            , case submsg of
                IssueList.SelectIssue _ ->
                    Cmd.batch [ wrappedCmd, refreshTime ]

                _ ->
                    wrappedCmd
            )

        (Tick time, _) ->
            ( { model | currentTime = time }, Cmd.none )

        (TimesheetMsg submsg, Just issues) ->
            case getSelectedIssueTimesheet issues of
                Ok selectedIssueTimesheet ->
                    let
                        ( updatedTimesheet, cmd ) =
                            Timesheet.update model.integrations submsg selectedIssueTimesheet
                    in
                    ( { model | issues = Just (updateSelectedIssueTimesheet issues updatedTimesheet) }
                    , Cmd.map TimesheetMsg cmd
                    )

                Err _ ->
                    ( model, Cmd.none )

        (IntegrationConfigManagerMsg submsg, _) ->
            let
                ( newIntegrationModel, integrationCmd ) =
                    Integrations.updateConfigManagers submsg model.integrations
            in
            ( { model | integrations = newIntegrationModel }
            , Cmd.map IntegrationConfigManagerMsg integrationCmd
            )

        _ -> (model, Cmd.none)

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { issues = Nothing
      , integrations = Integrations.initConfigs
      , currentTime = Time.millisToPosix 0
      }
    , Cmd.batch
        [ refreshTime
        , IssueList.fetchAllIssues IssueListFetched
        ]
    )


refreshTime : Cmd Msg
refreshTime =
    Task.perform Tick Time.now


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Time.every 1000 Tick
                    , Sub.map IntegrationConfigManagerMsg Integrations.subscriptions
                    ]
        }
