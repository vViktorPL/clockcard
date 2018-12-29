module IssueList exposing
    ( Model
    , Msg(..)
    , GetTimesheetError(..)
    , GetTimesheetResult
    , fetchAllIssues
    , getSelectedIssueTimesheet
    , update
    , updateSelectedIssue
    , updateSelectedIssueTimesheet
    , view
    )

import Browser.Dom exposing (focus)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, classList, id, placeholder, type_)
import Html.Events exposing (onBlur, onClick, onInput)
import Html.Events.Extra exposing (onEnter)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import SelectableList exposing (..)
import Task exposing (attempt)
import Timesheet
import Time exposing (Posix)
import Maybe.Extra
import Iso8601
import Http

type alias MinimalIssueData =
    { id: Int
    , name: String
    , periodInProgress: Maybe Posix
    }

type alias FullIssueData =
    { id: Int
    , name: String
    , timesheet : Timesheet.Model
    }

type Issue = Entry MinimalIssueData | Full FullIssueData


type alias Model =
    { list : Maybe (SelectableList Issue)
    , newIssueName : Maybe String
    }


type Msg
    = SelectIssue Issue
    | PromptNewIssueName
    | RequestNewIssue
    | IssueCreated (Result Http.Error Issue)
    | NewIssueNameChange String
    | NewIssueInputFocus (Result Browser.Dom.Error ())
    | NewIssueInputBlur
    | IssueDetails (Result Http.Error Issue)


fetchAllIssues : (Result Http.Error Model -> msg) -> Cmd msg
fetchAllIssues msg =
    Http.get "backend:/db/issues" decoder
        |> Http.send msg


view : Model -> Html Msg
view model =
    div
        [ class "issue-list" ]
        ([ viewAddButton model.newIssueName ]
            ++ (Maybe.map viewIssues model.list |> Maybe.withDefault [])
        )


isIssueRunning : Issue -> Bool
isIssueRunning issue =
    let
        currentPeriodStart =
            case issue of
                Entry { periodInProgress } ->
                    periodInProgress

                Full { timesheet } ->
                    Timesheet.getCurrentlyRunningPeriodStart timesheet
    in
    Maybe.Extra.isJust currentPeriodStart


viewIssues : SelectableList Issue -> List (Html Msg)
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
                    [ viewIssue issue ]
            )

viewIssue : Issue -> Html Msg
viewIssue issue =
    case issue of
        Full { name } -> Html.text name
        Entry { name } -> Html.text name

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
                    , onEnter RequestNewIssue
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


updateListInModel : Model -> (SelectableList Issue -> SelectableList Issue) -> Model
updateListInModel model updateList =
    { model | list = Maybe.map updateList model.list }

getIssueId : Issue -> Int
getIssueId issue =
    case issue of
        Full { id } -> id
        Entry { id } -> id

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectIssue issue ->
            case model.list of
                Just issueList ->
                    ( updateListInModel model (\list -> Maybe.withDefault issueList (SelectableList.select issue list))
                    , fetchIssueDetails issue
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

        IssueDetails (Ok newIssue) ->
            case model.list of
                Just selectableList ->
                    ({ model | list =
                        Just
                            ( SelectableList.map
                                (\currentIssue ->
                                    if getIssueId currentIssue == getIssueId newIssue then
                                        newIssue
                                    else
                                        currentIssue
                                )
                                selectableList
                            )
                    }
                    , Cmd.none
                    )

                Nothing ->
                    (model, Cmd.none)

        IssueCreated (Ok issue) ->
            ( { model | list =
                case model.list of
                    Just selectableList ->
                        SelectableList.prepend selectableList issue
                            |> SelectableList.select issue

                    Nothing ->
                        SelectableList.fromList [ issue ]
              }
            , Cmd.none
            )

        RequestNewIssue ->
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
                    ( model
                    , createNewIssue newIssueName
                    )
        _ -> (model, Cmd.none)

fetchIssueDetails : Issue -> Cmd Msg
fetchIssueDetails issue =
    case issue of
        Full _ -> Cmd.none
        Entry { id } ->
            Http.get ("backend:/db/issues/" ++ (String.fromInt id)) fullIssueDecoder
                |> Http.send IssueDetails

createNewIssue : String -> Cmd Msg
createNewIssue name =
    Http.post "backend:/db/issues" (Http.jsonBody (E.object [ ("name", E.string name) ])) fullIssueDecoder
        |> Http.send IssueCreated


type GetTimesheetError = ListIsEmpty | FetchingIssueDetailsInProgress
type alias GetTimesheetResult = Result GetTimesheetError Timesheet.Model

getSelectedIssueTimesheet : Model -> GetTimesheetResult
getSelectedIssueTimesheet model =
    model.list
        |> Maybe.map SelectableList.getSelected
        |> Result.fromMaybe ListIsEmpty
        |> Result.andThen
            (\issue ->
                case issue of
                    Full { timesheet } -> Ok timesheet
                    Entry _ -> Err FetchingIssueDetailsInProgress
            )

updateSelectedIssueTimesheet : Model -> Timesheet.Model -> Model
updateSelectedIssueTimesheet model timesheet =
    { model | list = Maybe.map (SelectableList.mapSelected (updateIssueTimesheet timesheet) ) model.list }


updateIssueTimesheet : Timesheet.Model -> Issue -> Issue
updateIssueTimesheet timesheet issue =
    case issue of
        Full issueData -> Full { issueData | timesheet = timesheet }
        Entry issueData -> Full { id = issueData.id, name = issueData.name, timesheet = timesheet }

updateSelectedIssue : Model -> Issue -> Model
updateSelectedIssue model updatedIssue =
    { model | list = Maybe.map (SelectableList.mapSelected (\_ -> updatedIssue)) model.list }


decoder : Decoder Model
decoder =
    D.list issueEntryDecoder
        |> D.map
            ( \list ->
                 { list = SelectableList.fromList list
                 , newIssueName = Nothing
                 }
            )


issueEntryDecoder : Decoder Issue
issueEntryDecoder =
    D.map Entry <|
    D.map3 MinimalIssueData
        (D.field "id" D.int)
        (D.field "name" D.string)
        (D.at ["timesheet", "periodInProgress"] (D.nullable Iso8601.decoder))

fullIssueDecoder : Decoder Issue
fullIssueDecoder =
    D.map Full <|
    D.map3 FullIssueData
        (D.field "id" D.int)
        (D.field "name" D.string)
        (D.field "timesheet" Timesheet.decoder)

