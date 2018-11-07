port module Main exposing (main)

import Browser
import Html exposing (Html, div)
import IssueList exposing (..)
import Json.Decode exposing (Decoder, decodeValue, field)
import Json.Encode exposing (Value, int)
import Stopwatch exposing (Msg)


port save : Value -> Cmd msg


type Msg
    = StopwatchMsg Stopwatch.Msg
    | IssueListMsg IssueList.Msg


type alias Model =
    { issues : IssueList.Model
    }


type alias IssueId =
    Int


type alias Issue =
    { id : IssueId
    , name : String
    , stopwatch : Stopwatch.Model
    }


type alias Flags =
    Value


getCurrentStopwatch : Model -> Stopwatch.Model
getCurrentStopwatch model =
    model.issues
        |> IssueList.getSelectedIssue
        |> .stopwatch


view : Model -> Html Msg
view model =
    div
        []
        [ Html.map IssueListMsg (IssueList.view model.issues)
        , Html.map StopwatchMsg (Stopwatch.view (getCurrentStopwatch model))
        ]


normalizeState : Model -> Value
normalizeState model =
    Json.Encode.object
        [ ( "version", int 1 )
        , ( "issues", IssueList.normalize model.issues )
        ]


decoder : Decoder Model
decoder =
    Json.Decode.map Model <|
        field "issues" IssueList.decoder


saveNormalized model =
    model |> normalizeState |> save


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
                    Cmd.batch [ wrappedCmd, Cmd.map StopwatchMsg Stopwatch.refresh ]

                IssueList.NewIssue ->
                    Cmd.batch [ wrappedCmd, saveNormalized updatedModel ]

                _ ->
                    wrappedCmd
            )

        StopwatchMsg submsg ->
            let
                selectedIssue =
                    IssueList.getSelectedIssue model.issues

                ( updatedStopwatch, cmd ) =
                    Stopwatch.update submsg (getCurrentStopwatch model)

                wrappedCmd =
                    Cmd.map StopwatchMsg cmd

                updatedModel =
                    { model | issues = updateSelectedIssue model.issues { selectedIssue | stopwatch = updatedStopwatch } }
            in
            ( updatedModel
            , case Stopwatch.stateSaveAdvised submsg of
                True ->
                    Cmd.batch [ wrappedCmd, saveNormalized updatedModel ]

                False ->
                    wrappedCmd
            )


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( case decodeValue decoder flags of
        Ok model ->
            model

        Err _ ->
            { issues = IssueList.init }
    , Cmd.map StopwatchMsg Stopwatch.refresh
    )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.map StopwatchMsg (Stopwatch.subscriptions (getCurrentStopwatch model))
        }
