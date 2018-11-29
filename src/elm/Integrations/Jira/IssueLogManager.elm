module IssueLogManager exposing
    ( Model, LogRef
    , Msg, OutputMsg(..)
    , init
    , update
    , viewLogTable, viewForm
    , openForm, isFormOpened
    )

import Time exposing (Posix)
import List.Extra exposing (find)
import Json.Encode as E exposing (Value)
import Json.Decode as D exposing (Decoder)
import Html exposing (Html)
import Html.Attributes exposing (src, value, selected, class, placeholder)
import Html.Events exposing (onClick, on, targetValue, onInput)
import Task
import Iso8601
import Debounce exposing (Debounce)
import Jira.Api
import Jira.Pagination exposing (PageRequest, pageRequest, paginationConfig)
import Jira.Jql exposing (fieldEqualsExpression, literalStringToExpression)

import Assets exposing (getImageUrl)
import Integrations.Jira.Config exposing (getValidDestinations, ValidDestination, ProjectData)
import MessageBox exposing (showErrorBox)
import Time.Extra exposing (humanReadableDurationToSecs)

-- MODEL

type Model = Model (List Log) (Maybe NewEntryForm) Int

type alias Config = Integrations.Jira.Config.Model

type alias Log =
    { ref: LogRef
    , issueKey: String
    , issueUrl: String
    , loggedTime: Int
    , commitTime: Posix
    }

type LogRef = LogRef Int


type NewEntryForm
    = NewEntryForm
        { submitting: Bool

        -- Destination
        , selectedDestination: Maybe ValidDestination

        -- Project
        , selectedProject: Maybe ProjectData

        -- Issues
        , selectedIssue: Maybe Jira.Api.Issue
        , issueQuery: String
        , issueQueryResult: List Jira.Api.Issue
        , issueSearchDebouncer: Debounce (FormMsg IssueSearchQuery)

        -- Date & time
        , startDate: String
        , duration: String
        }

type alias FormInitData =
    { logStartDate: Posix
    , logDuration: String
    }

debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 1000
    , transform = FormMsg << SearchIssuesDebounce
    }

-- MESSAGES

type Msg
    = FormMsg FormMsg
    | TableMsg TableMsg
    | NewLog

type TableMsg
    = NoOp

type FormMsg
    = SelectDestination String
    | SelectProject String
    | SelectIssue Jira.Api.Issue
    | ClearIssue
    | ChangeStartDate String
    | ChangeIssueSearchQuery String
    | SearchIssuesDebounce Debounce.Msg
    | IssuesQueryResult (Result Jira.Api.ApiCallError (List Jira.Api.Issue))
    | ChangeDuration String
    | Submit
    | SubmitResponse (Result Jira.Api.ApiCallError (Jira.Api.Issue, Jira.Api.Worklog))
    | RequestConfigManagerToShow


type OutputMsg
    = OutputNoOp
    | WorkLogAdded LogRef
    | ShowConfigManager

-- INIT

init : Model
init = Model [] Nothing 1

-- UPDATE

openForm : Model -> FormInitData -> Model
openForm (Model logs _ currentIndex) { logStartDate, logDuration } =
    let
        form =
            NewEntryForm
                { submitting = False
                , selectedDestination = Nothing
                , selectedProject = Nothing
                , selectedIssue = Nothing
                , issueQuery = ""
                , issueQueryResult = []
                , issueSearchDebouncer = Debounce.init
                , startDate = Iso8601.fromTime logStartDate
                , duration = logDuration
                }
    in
    (Model logs (Just form) currentIndex)


update : Config -> Msg -> Model -> (Model, Cmd Msg, OutputMsg)
update config msg model =
    case (msg, model) of
        (FormMsg formMsg, (Model logs (Just form) currentLogSequence)) ->
            let
                (updatedForm, cmd, outputMsg) = updateForm config formMsg form
            in
                ( (Model logs (Just updatedForm) currentLogSequence)
                , cmd
                , outputMsg
                )

        _ ->
            (model, Cmd.none, OutputNoOp)


updateForm : Config -> FormMsg -> NewEntryForm -> (NewEntryForm, Cmd Msg, OutputMsg)
updateForm config msg (NewEntryForm form) =
    case msg of
        SelectDestination destinationId ->
            ( NewEntryForm
                { form
                | selectedDestination =
                    find (\dest -> dest.id == destinationId) (getValidDestinations config)
                , selectedProject = Nothing
                , selectedIssue = Nothing
                , issueQuery = ""
                , issueQueryResult = []
                , issueSearchDebouncer = Debounce.init
                }
            , Cmd.none
            , OutputNoOp
            )

        SelectProject projectId -> -- TODO UPDATE DEBOUNCER ALSO IN THIS CASE
            ( NewEntryForm
                { form
                | selectedProject =
                    form.selectedDestination
                        |> Maybe.map .projects
                        |> Maybe.andThen (find \proj -> proj.id == projectId)
                }
            , Cmd.none
            , OutputNoOp
            )

        SelectIssue jiraIssue ->
            ( NewEntryForm { form | selectedIssue = Just jiraIssue }
            , Cmd.none
            , OutputNoOp
            )

        ClearIssue ->
            ( NewEntryForm { form | selectedIssue = Nothing }
            , Cmd.none
            , OutputNoOp
            )

        ChangeStartDate startDate ->
            ( NewEntryForm { form | startDate = startDate }
            , Cmd.none
            , OutputNoOp
            )

        ChangeDuration duration ->
            ( NewEntryForm { form | duration = duration }
            , Cmd.none
            , OutputNoOp
            )

        ChangeIssueSearchQuery query ->
            let
                ( updatedDebouncer, debouncerCmd ) =
                    Debounce.push debounceConfig query form.issueSearchDebouncer
            in
            ( NewEntryForm
                { form
                | issueQuery = query
                , issueSearchDebouncer = updatedDebouncer
                }
            , debouncerCmd
            , OutputNoOp
            )

        SearchIssuesDebounce debouncerMsg ->
            case form.selectedDestination of
                Just destination ->
                    let
                        ( updatedDebouncer, debouncerCmd ) =
                            Debounce.update
                                debounceConfig
                                (Debounce.takeLast (searchIssues destination form.selectedProject) )
                                debouncerMsg
                                form.issueSearchDebouncer
                    in
                    ( NewEntryForm { form | issueSearchDebouncer = updatedDebouncer }
                    , debouncerCmd
                    , OutputNoOp
                    )

                Nothing -> (NewEntryForm form, Cmd.none, OutputNoOp)

        IssuesQueryResult (Ok (List issues)) ->
            ( NewEntryForm
                { form
                | issueQueryResult = issues
                }
            , Cmd.none
            , OutputNoOp
            )

        Submit ->
            case logToJira (NewEntryForm form) of
                Ok cmd ->
                    ( NewEntryForm { form | submitting = True }
                    , cmd
                    , OutputNoOp
                    )

                Err errorMessage ->
                    ( NewEntryForm form
                    , showErrorBox "JIRA worklog form error" errorMessage
                    , OutputNoOp
                    )


        _ ->
            ( NewEntryForm form
            , Cmd.none
            , OutputNoOp
            )


type ValidatedFormValues =
    ValidatedFormValues
        (Maybe ValidDestination)
        (Maybe Jira.Api.Issue)
        (Maybe Posix)
        (Maybe Int)


logToJira : NewEntryForm -> Result String (Cmd Msg)
logToJira (NewEntryForm form) =
    case
        ValidatedFormValues
            (form.selectedDestination |> Maybe.map Tuple.second)
            (form.selectedIssue)
            (Iso8601.toTime form.startDate |> Result.toMaybe)
            (humanReadableDurationToSecs form.duration)
    of
        ValidatedFormValues (Just cred) (Just issue) (Just startDate) (Just durationSecs) ->
            Jira.Api.addWorklog cred issue
                { started = startDate
                , timeSpentSeconds = durationSecs
                , comment = Nothing
                }
            |> Task.map (Tuple.pair issue)
            |> Task.attempt SubmitResponse
            |> Ok

        ValidatedFormValues Nothing _ _ _ ->
            Err "You have to select destination JIRA"

        ValidatedFormValues _ Nothing _ _ ->
            Err "You have to select JIRA issue"

        ValidatedFormValues _ _ Nothing _ ->
            Err "Invalid start date"

        ValidatedFormValues _ _ _ Nothing ->
            Err "Invalid duration"



-- VIEW


viewLogRef : LogRef -> Html msg
viewLogRef (LogRef logRef) =
    Html.div []
        [ Html.img [src (getImageUrl "jira-icon.svg")] []
        , Html.text ("#" ++ String.fromInt logRef)
        ]

viewLogTable : Model -> Html Msg
viewLogTable (Model logs _ _) =
    Html.text ""

viewForm : Config -> Model -> Html Msg
viewForm config (Model _ maybeForm _) =
    case maybeForm of
        Just form ->
            viewFormInternal (getValidDestinations config) form

        Nothing ->
            Html.text ""


viewFormInternal : List ValidDestination -> NewEntryForm -> Html Msg
viewFormInternal availableDestinations (NewEntryForm form) =
    case availableDestinations of
        [] ->
            Html.div []
                [ Html.text "There is no JIRA destination available. "
                , Html.span [ onClick RequestConfigManagerToShow ] [ Html.text "Configure one" ]
                , Html.text "."
                ]

        _ ->
            Html.div []
                [ Html.h2 [] [ Html.text "Log work to JIRA" ]
                , viewFormInput "Select JIRA"
                    (viewDestinationsSelect availableDestinations form.selectedDestination)
                , viewFormInput "Select project"
                    (viewProjectsSelect form.selectedDestination form.selectedProject)
                , viewFormInput "Select issue"
                    (viewIssueSelect form.issueQuery form.issueQueryResult form.selectedIssue)
                , viewFormInput "Start date"
                    (viewStartDatePicker form.startDate)
                , viewFormInput "Duration"
                    (viewDuration form.duration)
                , Html.button [ onClick Submit ] [ Html.text "Submit worklog" ]
                ]

viewFormInput : String -> Html msg -> Html msg
viewFormInput label input =
    Html.div []
        [ Html.label [] [ Html.text (label ++ ":") ]
        , Html.div [ Html.Attributes.style "display" "block" ] [ input ]
        ]


viewDestinationsSelect : List ValidDestination -> Maybe ValidDestination -> Html Msg
viewDestinationsSelect availableDestinations selectedDestination =
    Html.select [ onChange SelectDestination ]
        (
            ( Html.option
                [ value "", selected (selectedDestination == Nothing) ]
                [ Html.text "-"]
            ) ::
            List.map
                ( \destination ->
                    Html.option
                        [ value destination.id
                        , selected (selectedDestination == destination)
                        ]
                        [ Html.text destination.name ]
                )
                availableDestinations
        )

viewProjectsSelect : Maybe ValidDestination -> Maybe ProjectData -> Html Msg
viewProjectsSelect selectedDestination selectedProject =
    Html.select [ onChange SelectProject ]
        (
            ( Html.option
                [ value "", selected (selectedProject == Nothing) ]
                [ Html.text "Any"]
            ) ::
            case selectedDestination of
                Just destination ->
                    ( List.map
                        (\project ->
                            Html.option
                                [ value project.id
                                , selected
                                    ( selectedProject
                                        |> Maybe.map (\projectId -> project.id == projectId)
                                        |> Maybe.withDefault False
                                    )
                                ]
                                [ Html.text (project.name ++ " (" ++ project.key ++ ")")]
                        )
                        destination.projects
                    )

                Nothing -> []
        )

viewIssueSelect : String -> List Jira.Api.Issue -> Maybe Jira.Api.Issue -> Html Msg
viewIssueSelect issueQuery issueQueryResult selectedIssue =
    case selectedIssue of
        Just issue ->
            Html.div [ onClick ClearIssue ] [ viewIssue issue ]

        Nothing ->
            Html.div [ class "prompt-input" ]
                [ Html.input [ onInput ChangeIssueSearchQuery, value issueQuery, placeholder "Search issue" ] []
                , Html.div []
                    [ Html.div [ class "prompt" ]
                        (List.map viewPromptItem issueQueryResult)
                    ]
                ]


viewPromptItem : Jira.Api.Issue -> Html Msg
viewPromptItem issue =
    Html.div [ onClick (SelectIssue issue), class "prompt__item" ] [ viewIssue issue ]

viewIssue : Jira.Api.Issue -> Html Msg
viewIssue issue =
    Jira.Api.getIssueFields issue
        |> D.decodeValue
            ( D.map3
                ( \icon key summary -> Html.div [ class "jira-issue"]
                    [ Html.img [ src icon ] []
                    , Html.text ("[" ++ key ++ "] " ++ summary)
                    ]
                )
                (D.at ["issuetype", "iconUrl"] D.string)
                (D.succeed (Jira.Api.getIssueKey issue))
                (D.field "summary" D.string)
            )
        |> Result.withDefault (Html.text "")


onLocalizedDateChange : (String -> msg) -> Html.Attribute msg
onLocalizedDateChange msg =
    on "localizedChange" (D.at ["detail", "value"] D.string |> D.map msg)

viewStartDatePicker : String -> Html Msg
viewStartDatePicker startDate =
    Html.node "datetime-picker" [ value startDate, onLocalizedDateChange ChangeStartDate ] []

viewDuration : String -> Html Msg
viewDuration duration =
    Html.input [ value duration, onInput ChangeDuration ] []

onChange : (String -> msg) -> Html.Attribute msg
onChange msg =
    on "change" (D.map msg targetValue)


-- OTHER

type alias IssueSearchQuery =
    { jiraCred: Maybe Jira.Api.Cred
    , project: Maybe String
    , query: String
    }

isFormOpened : Model -> Bool
isFormOpened (Model _ maybeForm _) =
    case maybeForm of
        Just _ -> True
        Nothing -> False


first20items : PageRequest
first20items = pageRequest (paginationConfig 20) 1

searchIssues : ValidDestination -> Maybe ProjectData -> String -> Cmd Msg
searchIssues destination project query =
    let
        jql =
            ( case project of
                Just projectData -> ((fieldEqualsExpression "project" projectData.id) ++ " AND ")
                Nothing -> ""
            )
            ++ "text ~ " ++ (literalStringToExpression (query ++ "*"))
    in
    Jira.Api.getIssues destination.cred first20items jql ["self"]
        |> Task.map Jira.Pagination.getItems
        |> Task.attempt IssuesQueryResult

