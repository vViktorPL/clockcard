module IssueList exposing (Model, Msg(..), decoder, getSelectedIssue, init, normalize, update, updateSelectedIssue, view)

import Browser.Dom exposing (focus)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, classList, id, placeholder, type_)
import Html.Events exposing (onBlur, onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import Issue exposing (IssueId, Model)
import Json.Decode exposing (Decoder, field, int)
import Json.Encode exposing (Value, object)
import SelectableList exposing (..)
import Stopwatch exposing (Model)
import Task exposing (attempt)


type alias Model =
    { list : SelectableList Issue.Model
    , newIssueName : Maybe String
    , currentId : IssueId
    }


type Msg
    = SelectIssue Issue.Model
    | PromptNewIssueName
    | NewIssue
    | NewIssueNameChange String
    | NewIssueInputFocus (Result Browser.Dom.Error ())
    | NewIssueInputBlur


init : Model
init =
    { list =
        SelectableList
            { head = []
            , selected =
                { id = 1
                , name = "Example issue"
                , stopwatch = Stopwatch.blank
                }
            , tail = []
            }
    , newIssueName = Nothing
    , currentId = 2
    }


view : Model -> Html Msg
view model =
    div
        [ class "issue-list" ]
        ([ viewAddButton model.newIssueName ]
            ++ viewIssues model.list
        )


isIssueRunning : Issue.Model -> Bool
isIssueRunning issue =
    Stopwatch.isRunning issue.stopwatch


viewIssues : SelectableList Issue.Model -> List (Html Msg)
viewIssues issueList =
    issueList
        |> SelectableList.getItems
        |> List.map
            (\issue ->
                div
                    [ classList
                        [ ( "issue-list__item", True )
                        , ( "issue-list__item--selected", SelectableList.isSelected issue issueList )
                        , ( "issue-list__item--running", isIssueRunning issue )
                        ]
                    , onClick (SelectIssue issue)
                    ]
                    [ text issue.name ]
            )


viewAddButton : Maybe String -> Html Msg
viewAddButton newIssueName =
    case newIssueName of
        Just name ->
            div
                [ class "issue-list__add-button" ]
                [ input
                    [ type_ "text"
                    , id "issue-list__new-issue-input"
                    , placeholder "Enter new issue name"
                    , onInput NewIssueNameChange
                    , onEnter NewIssue
                    , onBlur NewIssueInputBlur
                    ]
                    []
                ]

        Nothing ->
            div
                [ class "issue-list__add-button"
                , onClick PromptNewIssueName
                ]
                [ text "+ New issue"
                ]


updateListInModel : Model -> (SelectableList Issue.Model -> SelectableList Issue.Model) -> Model
updateListInModel model updateList =
    { model | list = updateList model.list }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectIssue issue ->
            ( updateListInModel model (\list -> Maybe.withDefault model.list (SelectableList.select issue list))
            , Cmd.none
            )

        PromptNewIssueName ->
            ( { model | newIssueName = Just "" }
            , focus "issue-list__new-issue-input" |> Task.attempt NewIssueInputFocus
            )

        NewIssueInputFocus _ ->
            ( model, Cmd.none )

        NewIssueInputBlur ->
            ( { model | newIssueName = Nothing }
            , Cmd.none
            )

        NewIssueNameChange name ->
            ( { model | newIssueName = Just (String.trim name) }
            , Cmd.none
            )

        NewIssue ->
            case model.newIssueName of
                Just "" ->
                    ( model
                    , Cmd.none
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just newIssueName ->
                    let
                        newIssue : Issue.Model
                        newIssue =
                            { id = model.currentId
                            , name = newIssueName
                            , stopwatch = Stopwatch.blank
                            }

                        updatedList =
                            SelectableList.prepend model.list newIssue
                    in
                    ( { model
                        | list = Maybe.withDefault updatedList (SelectableList.select newIssue updatedList)
                        , currentId = model.currentId + 1
                        , newIssueName = Nothing
                      }
                    , Cmd.none
                    )


getSelectedIssue : Model -> Issue.Model
getSelectedIssue model =
    SelectableList.getSelected model.list


updateSelectedIssue : Model -> Issue.Model -> Model
updateSelectedIssue model updatedIssue =
    { model | list = SelectableList.mapSelected (\_ -> updatedIssue) model.list }


normalize : Model -> Value
normalize model =
    object
        [ ( "list", SelectableList.normalize Issue.normalize model.list )
        , ( "currentId", Json.Encode.int model.currentId )
        ]


decoder : Decoder Model
decoder =
    Json.Decode.map2
        (\list currentId ->
            { list = list
            , currentId = currentId
            , newIssueName = Nothing
            }
        )
        (field "list" (SelectableList.decoder Issue.decoder))
        (field "currentId" int)
