module Integrations.Jira.SubmitLogForm exposing (Model, Msg, update, view, init)

import Time exposing (Posix)
import Html exposing (Html)
import Html.Events exposing (onClick, on, targetValue, onInput)
import Html.Attributes exposing (value, selected, placeholder, class, src)
import Json.Decode as D exposing (Decoder)
import Jira.Api
import Jira.Pagination exposing (PageRequest, paginationConfig, pageRequest)
import Jira.Jql exposing (Jql, fieldEqualsExpression, literalStringToExpression)
import Task
import Iso8601
import Debounce exposing (Debounce)
import Json.Decode as D

import Integrations
import Integrations.Jira.Config exposing (getValidDestinations, ValidDestination, ProjectData, projectData)

type alias Config = Integrations.Jira.Config.Model


type Model
    = EmptyConfig
    | SubmitLogForm

        -- Destination
        { selectedDestination: Maybe ValidDestination
        , availableDestinations: List ValidDestination

        -- Project
        , selectedProject: Maybe String
        , availableProjects: List ProjectData

        -- Issues
        , selectedIssue: Maybe Jira.Api.Issue
        , issueQuery: String
        , issueQueryResult: List Jira.Api.Issue
        , issueSearchDebouncer: Debounce IssueSearchQuery

        -- Date & time
        , startDate: String
        , duration: Int
        }

type alias IssueSearchQuery =
    { jiraCred: Maybe Jira.Api.Cred
    , project: Maybe String
    , query: String
    }

type Msg
    = SelectDestination String
    | SelectProject String
    | SelectIssue Jira.Api.Issue
    | ClearIssue
    | ShowConfigManager
    | AvailableProjectsList (Result Jira.Api.ApiCallError (List ProjectData))
    | SearchIssuesQueryChange String
    | SearchIssuesDebounce Debounce.Msg
    | IssuesQueryResult (Result Jira.Api.ApiCallError (List Jira.Api.Issue))

init : Config -> Posix -> Int -> Model
init config startDate duration =
    let
        availableDestinations = getValidDestinations config
    in
    case availableDestinations of
        [] -> EmptyConfig
        firstDestination :: _ -> SubmitLogForm
            { selectedDestination = Nothing
            , availableDestinations = availableDestinations
            , selectedProject = Nothing
            , availableProjects = []
            , selectedIssue = Nothing
            , issueQuery = ""
            , issueQueryResult = []
            , issueSearchDebouncer = Debounce.init
            , startDate = Iso8601.fromTime startDate
            , duration = duration
            }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model) of
        ( SelectDestination value, SubmitLogForm form ) ->
            let
                selectedDestination =
                      getItemByIndexValue value form.availableDestinations
            in
            ( SubmitLogForm { form | selectedDestination = selectedDestination }
            , case selectedDestination of
                Just destination -> fetchProjects (Tuple.second destination)
                Nothing -> Cmd.none
            )

        ( AvailableProjectsList (Ok projects), SubmitLogForm form ) ->
            ( SubmitLogForm
                { form
                | selectedProject =
                    projects
                        |> List.head
                        |> Maybe.map .id
                , availableProjects = projects
                }
            , Cmd.none
            )

        ( IssuesQueryResult (Ok issues), SubmitLogForm form ) ->
            ( SubmitLogForm { form | issueQueryResult = issues }
            , Cmd.none
            )

        ( SelectIssue issue, SubmitLogForm form ) ->
            ( SubmitLogForm { form | selectedIssue = Just issue }
            , Cmd.none
            )

        ( ClearIssue, SubmitLogForm form ) ->
            ( SubmitLogForm { form | selectedIssue = Nothing }
            , Cmd.none
            )

        ( SearchIssuesQueryChange query, SubmitLogForm form ) ->
            let
                issueQuery : IssueSearchQuery
                issueQuery =
                    { jiraCred =
                        form.selectedDestination
                            |> Maybe.map Tuple.second
                    , project = form.selectedProject
                    , query = query
                    }

                ( debouncer, cmd ) =
                    Debounce.push debounceConfig issueQuery form.issueSearchDebouncer
            in
            ( SubmitLogForm
                { form
                | issueQuery = query
                , issueSearchDebouncer = debouncer
                }
            , cmd
            )

        ( SearchIssuesDebounce submsg, SubmitLogForm form ) ->
            let
                ( debouncer, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast searchIssues)
                        submsg
                        form.issueSearchDebouncer
            in
            ( SubmitLogForm { form | issueSearchDebouncer = debouncer }, cmd )

        _ ->
            let
                x = Debug.log "_MSG" msg
            in
            (model, Cmd.none)

first20items : PageRequest
first20items = pageRequest (paginationConfig 20) 1

searchIssues : IssueSearchQuery -> Cmd Msg
searchIssues { jiraCred, project, query } =
    case jiraCred of
        Just cred ->
            let
                jql =
                    ( case project of
                        Just projectId -> ((fieldEqualsExpression "project" projectId) ++ " AND ")
                        Nothing -> ""
                    )
                    ++ "text ~ " ++ (literalStringToExpression (query ++ "*"))
            in
            Jira.Api.getIssues cred first20items jql []
                |> Task.map Jira.Pagination.getItems
                |> Task.attempt IssuesQueryResult

        _ -> Cmd.none



debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 1000
    , transform = SearchIssuesDebounce
    }


fetchProjects : Jira.Api.Cred -> Cmd Msg
fetchProjects cred =
    Jira.Api.getAllProjects cred
        |> Task.map (List.map projectData)
        |> Task.attempt AvailableProjectsList


getItemByIndexValue : String -> List a -> Maybe a
getItemByIndexValue value list =
    String.toInt value
        |> Maybe.map (\index -> List.take (index+1) list)
        |> Maybe.andThen List.head



view : Model -> Html Msg
view model =
    case model of
        EmptyConfig ->
            Html.div []
                [ Html.text "There is no JIRA destination available. "
                , Html.span [ onClick ShowConfigManager ] [ Html.text "Configure one" ]
                , Html.text "."
                ]

        SubmitLogForm form ->
            Html.div []
                [ Html.h2 [] [ Html.text "Log work to JIRA" ]
                , viewFormInput "Select JIRA"
                    (viewDestinationsSelect form.availableDestinations form.selectedDestination)
                , viewFormInput "Select project"
                    (viewProjectsSelect form.availableProjects form.selectedProject)
                , viewFormInput "Select issue"
                    (viewIssueSelect model)
                ]


viewFormInput : String -> Html msg -> Html msg
viewFormInput label input =
    Html.div []
        [ Html.label [] [ Html.text (label ++ ":") ]
        , Html.div [ Html.Attributes.style "display" "block" ] [ input ]
        ]


onChange : (String -> msg) -> Html.Attribute msg
onChange msg =
    on "change" (D.map msg targetValue)

viewDestinationsSelect : List ValidDestination -> Maybe ValidDestination -> Html Msg
viewDestinationsSelect availableDestinations selectedDestination =
    Html.select [ onChange SelectDestination ]
        (
            ( Html.option
                [ value "", selected (selectedDestination == Nothing) ]
                [ Html.text "-"]
            ) ::
            ( List.indexedMap
                (\index destination ->
                    Html.option
                        [ value (String.fromInt index)
                        , selected
                            ( selectedDestination
                                |> Maybe.map (\d -> d == destination)
                                |> Maybe.withDefault False
                            )
                        ]
                        [ Html.text (Tuple.first destination) ]
                )
                availableDestinations
            )
        )

viewProjectsSelect : List ProjectData -> Maybe String -> Html Msg
viewProjectsSelect availableProjects selectedProject =
    Html.select [ onChange SelectProject ]
        (
            ( Html.option
                [ value "", selected (selectedProject == Nothing) ]
                [ Html.text "Any"]
            ) ::
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
                availableProjects
            )
        )

viewIssueSelect : Model -> Html Msg
viewIssueSelect model =
    case model of
        SubmitLogForm form ->
            case form.selectedIssue of
                Just issue ->
                    Html.div [ onClick ClearIssue ] [ viewIssue issue ]

                Nothing ->
                    Html.div [ class "prompt-input" ]
                        [ Html.input [ onInput SearchIssuesQueryChange, value form.issueQuery, placeholder "Search issue" ] []
                        , Html.div []
                            [ Html.div [ class "prompt" ]
                                (List.map viewPromptItem form.issueQueryResult)
                            ]
                        ]


        EmptyConfig -> Html.text ""


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

--viewStartDatePicker :