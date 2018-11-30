module IssueList exposing (Model, Msg(..), decoder, getSelectedIssue, init, normalize, update, updateSelectedIssue, view)

import Browser.Dom exposing (focus)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, classList, id, placeholder, type_)
import Html.Events exposing (onBlur, onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import Issue exposing (IssueId, Model)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import SelectableList exposing (..)
import Task exposing (attempt)
import Timesheet


type alias Model =
    { list : Maybe (SelectableList Issue.Model)
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
    { list = Nothing
    , newIssueName = Nothing
    , currentId = 1
    }


view : Model -> Html Msg
view model =
    div
        [ class "issue-list" ]
        ([ viewAddButton model.newIssueName ]
            ++ (Maybe.map viewIssues model.list |> Maybe.withDefault [])
        )


isIssueRunning : Issue.Model -> Bool
isIssueRunning issue =
    case Timesheet.getCurrentlyRunningPeriodStart issue.timesheet of
        Just _ -> True
        Nothing -> False


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
    { model | list = Maybe.map updateList model.list }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectIssue issue ->
            case model.list of
                Just issueList ->
                    ( updateListInModel model (\list -> Maybe.withDefault issueList (SelectableList.select issue list))
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

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
                            , timesheet = Timesheet.init
                            }

                        updatedList =
                            case model.list of
                                Just selectableList ->
                                    (SelectableList.prepend selectableList newIssue)
                                        |> SelectableList.select newIssue

                                Nothing -> SelectableList.fromList [ newIssue ]
                    in
                    ( { model
                        | list = updatedList
                        , currentId = model.currentId + 1
                        , newIssueName = Nothing
                      }
                    , Cmd.none
                    )


getSelectedIssue : Model -> Maybe Issue.Model
getSelectedIssue model =
    Maybe.map SelectableList.getSelected model.list


updateSelectedIssue : Model -> Issue.Model -> Model
updateSelectedIssue model updatedIssue =
    { model | list = Maybe.map (SelectableList.mapSelected (\_ -> updatedIssue)) model.list }


normalize : Model -> Value
normalize model =
    E.object
        [ ( "list", Maybe.map (SelectableList.normalize Issue.normalize) model.list |> Maybe.withDefault E.null )
        , ( "currentId", E.int model.currentId )
        ]


decoder : Decoder Model
decoder =
    D.map2
        (\list currentId ->
            { list = list
            , currentId = currentId
            , newIssueName = Nothing
            }
        )
        (D.field "list" (D.nullable (SelectableList.decoder Issue.decoder)))
        (D.field "currentId" D.int)
