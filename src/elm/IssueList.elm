module IssueList exposing (Model, Msg(..), init, update, view, getSelectedIssue, updateSelectedIssue)

import Html exposing (Html, div, text, input)
import Html.Attributes exposing (classList, class, type_, id, placeholder)
import Html.Events exposing (onClick, onInput, onBlur)
import Html.Events.Extra exposing (onEnter)
import Dom exposing (focus)
import Task exposing (attempt)

import SelectableList exposing (..)
import Issue exposing (Model, IssueId)
import Stopwatch exposing (Model)

type alias Model =
    { list: SelectableList Issue.Model
    , newIssueName: Maybe String
    , currentId: IssueId
    }

type Msg
        = SelectIssue Issue.Model
        | PromptNewIssueName
        | NewIssue
        | NewIssueNameChange String
        | NewIssueInputFocus (Result Dom.Error ())
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
        (
            [ (viewAddButton model.newIssueName) ] ++
            (viewIssues model.list)
        )

isIssueRunning : Issue.Model -> Bool
isIssueRunning issue = case issue.stopwatch of
    Stopwatch.RunningStopwatch _ -> True
    Stopwatch.PausedStopwatch _ -> False

viewIssues : SelectableList Issue.Model -> List (Html Msg)
viewIssues issueList = issueList
    |> SelectableList.items
    |> List.map
        (\issue ->
            div
                [ classList
                    [ ("issue-list__item", True)
                    , ("issue-list__item--selected", SelectableList.isSelected issue issueList)
                    , ("issue-list__item--running", isIssueRunning issue)
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
updateListInModel model updateList = { model | list = updateList model.list }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SelectIssue issue ->
            ( updateListInModel model (\list -> (Maybe.withDefault model.list (SelectableList.select issue list)))
            , Cmd.none
            )

        PromptNewIssueName ->
            ( { model | newIssueName = Just "" } ) !
            [ (Dom.focus "issue-list__new-issue-input") |> Task.attempt NewIssueInputFocus ]

        NewIssueInputFocus _ ->
            (model, Cmd.none)

        NewIssueInputBlur ->
            ( { model | newIssueName = Nothing } ) ! [ Cmd.none ]

        NewIssueNameChange name ->
            ( { model | newIssueName = Just (String.trim name) } ) ! [ Cmd.none ]

        NewIssue ->
            case model.newIssueName of
                Just "" -> model ! [ Cmd.none ]
                Nothing -> model ! [ Cmd.none ]
                Just newIssueName ->
                    let
                        newIssue: Issue.Model
                        newIssue =
                            { id = model.currentId
                            , name = newIssueName
                            , stopwatch = Stopwatch.blank
                            }
                        updatedList = SelectableList.prepend model.list newIssue
                    in
                        ( { model
                            | list = Maybe.withDefault updatedList (SelectableList.select newIssue updatedList)
                            , currentId = model.currentId + 1
                            , newIssueName = Nothing
                          }
                        , Cmd.none
                        )

getSelectedIssue : Model -> Issue.Model
getSelectedIssue model = SelectableList.selected model.list

updateSelectedIssue : Model -> Issue.Model -> Model
updateSelectedIssue model updatedIssue =
    { model | list = SelectableList.mapSelected (\_ -> updatedIssue) model.list }