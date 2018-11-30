module Timesheet exposing
    ( Msg
    , Model
    , Period
    , update
    , view
    , init
    , decoder
    , encode
    , getCurrentlyRunningPeriodStart
    , saveStateAdvised
    )

import Time exposing (Posix)
import Html exposing (Html)
import Html.Attributes exposing (class, classList, type_, checked, disabled, attribute, datetime)
import Iso8601
import Json.Encode as E exposing (Value)
import Json.Decode as D exposing (Decoder)
import Html.Events exposing (onClick)

import Time.Extra exposing (posixDecoder, encodePosix, viewPosix, durationHumanReadable)
import Integrations exposing (LogRef, getJiraConfig)
import Integrations.Jira.Config
import Integrations.Jira.IssueLogManager as JiraIssueLogManager
import Stopwatch
import Task

type Msg
    = SwitchTab Tab
    | StartNewPeriodRequested
    | StartNewPeriod Posix
    | EndCurrentPeriodRequested
    | EndCurrentPeriod Posix
    | TogglePeriodSelection Period
    | RemoveSelectedPeriods
    | LogSelectedPeriodsToJira
    | JiraIssueLogManagerMsg JiraIssueLogManager.Msg

type Period = Period Posix Posix (List Integrations.LogRef)

type Tab = PeriodsTab | JiraTab

type Model = Timesheet TimesheetData


type alias TimesheetData =
    { currentTab: Tab
    , finishedPeriods: List (Bool, Period)
    , periodInProgress: Maybe Posix
    , cumulatedTicks: Int
    , integrations: IntegrationsData
    }

type alias IntegrationsData =
    { jira: JiraIssueLogManager.Model
    }


update : Integrations.Configs -> Msg -> Model -> (Model, Cmd Msg)
update integrationConfigs msg model =
    case msg of
        SwitchTab tab -> (switchTab model tab, Cmd.none)

        StartNewPeriodRequested -> ( model, Task.perform StartNewPeriod Time.now )

        StartNewPeriod startTime -> (Maybe.withDefault model (startPeriod model startTime), Cmd.none)

        EndCurrentPeriodRequested -> ( model, Task.perform EndCurrentPeriod Time.now )

        EndCurrentPeriod endTime -> (Maybe.withDefault model (addPeriod model endTime), Cmd.none)

        TogglePeriodSelection period -> (toggleFinishedPeriodSelection model period, Cmd.none)

        RemoveSelectedPeriods -> (removeSelectedPeriods model, Cmd.none)

        LogSelectedPeriodsToJira ->
            let
                modelData = getModelData model
                integrations = modelData.integrations

                periods = (getSelectedPeriods model)
                defaultStartTime =
                    List.head periods
                        |> Maybe.map (\(Period startTime _ _) -> startTime)
                        |> Maybe.withDefault (Time.millisToPosix 0)
                defaultDuration =
                    periods
                        |> List.map periodToDuration
                        |> List.foldl (+) 0
                        |> durationHumanReadable

            in
            ( Timesheet
                { modelData
                | integrations =
                    { integrations
                    | jira =
                        JiraIssueLogManager.openForm
                            integrations.jira
                            ( JiraIssueLogManager.FormInitData
                                defaultStartTime
                                defaultDuration
                            )
                    }
                }
            , Cmd.none
            )


        JiraIssueLogManagerMsg jiraMsg ->
            let
                modelData = getModelData model
                integrations = modelData.integrations

                (updatedJiraLogManager, jiraLogManagerCmd, outputMsg) =
                    JiraIssueLogManager.update
                        (getJiraConfig integrationConfigs)
                        jiraMsg
                        integrations.jira

                updatedPeriods =
                    case outputMsg of
                        JiraIssueLogManager.WorkLogAdded jiraLogRef ->
                            (modelData.finishedPeriods
                                |> List.map
                                    ( \(selected, Period start end logRefs) ->
                                        if selected then
                                            (False, Period start end (logRefs ++ [Integrations.JiraLogRef jiraLogRef]))
                                        else
                                            (False, Period start end logRefs)
                                    )
                            )
                        _ -> modelData.finishedPeriods


            in
                ( Timesheet
                    { modelData
                    | integrations = { integrations | jira = updatedJiraLogManager }
                    , finishedPeriods = updatedPeriods
                    }
                , Cmd.map JiraIssueLogManagerMsg jiraLogManagerCmd
                )

getSelectedPeriods : Model -> List Period
getSelectedPeriods (Timesheet model) =
    model.finishedPeriods
        |> List.filter Tuple.first
        |> List.map Tuple.second

switchTab : Model -> Tab -> Model
switchTab (Timesheet model) tab =
    Timesheet { model | currentTab = tab }

init : Model
init =
    Timesheet
        { currentTab = PeriodsTab
        , finishedPeriods = []
        , periodInProgress = Nothing
        , cumulatedTicks = 0
        , integrations =
            { jira = JiraIssueLogManager.init
            }
        }

getModelData : Model -> TimesheetData
getModelData (Timesheet model) = model



stopwatch : Maybe Posix -> Int -> Stopwatch.Model
stopwatch periodInProgress cumulatedTicks =
    case periodInProgress of
        Just startTime -> Stopwatch.running cumulatedTicks startTime
        Nothing -> Stopwatch.paused cumulatedTicks


view : Integrations.Configs -> Model -> Posix -> Html Msg
view integrationsConfigs (Timesheet model) currentTime =
    let
        formOpened = JiraIssueLogManager.isFormOpened model.integrations.jira
    in
    Html.div [ class "selected-item-form" ]
        [ viewStopwatch model currentTime formOpened
        , Html.div [ classList [("timesheet", True), ("collapsed", List.isEmpty model.finishedPeriods)] ]
            [ Html.div [ class "tabs" ]
                [ viewTab model PeriodsTab "Periods"
                , viewTab model JiraTab "Jira Log"
                ]
            , Html.fieldset [ class "timesheet__tab-content", disabled formOpened ]
                [ case model.currentTab of
                      PeriodsTab ->
                        viewPeriodsTab (Timesheet model)

                      JiraTab ->
                        Html.map JiraIssueLogManagerMsg
                            (JiraIssueLogManager.viewLogTable model.integrations.jira)
                ]
            ]
        ,  Html.map JiraIssueLogManagerMsg
              ( JiraIssueLogManager.viewForm
                  (getJiraConfig integrationsConfigs)
                  model.integrations.jira
              )
        ]

viewStopwatch : TimesheetData -> Posix -> Bool -> Html Msg
viewStopwatch model currentTime collapsed =
    Html.div [ classList [("stopwatch", True), ("collapsed", collapsed)] ]
        [ Stopwatch.view (stopwatch model.periodInProgress model.cumulatedTicks) currentTime
        , Html.div [ class "stopwatch__controls" ]
            [ case model.periodInProgress of
                Nothing -> Html.button [ onClick StartNewPeriodRequested ] [ Html.text "Start ▶" ]
                Just _ -> Html.button [ onClick EndCurrentPeriodRequested ] [ Html.text "❚❚ Pause" ]
            ]

        ]


viewTab : TimesheetData -> Tab -> String -> Html Msg
viewTab ({ currentTab }) tab title =
    Html.div
        [ onClick (SwitchTab tab)
        , classList
            [ ("tab", True)
            , ("active", currentTab == tab)
            ]
        ]
        [ Html.text title ]


startPeriod : Model -> Posix -> Maybe Model
startPeriod (Timesheet model) start =
    case model.periodInProgress of
        Just _ -> Nothing
        Nothing -> Just (Timesheet { model | periodInProgress = Just start })

cancelPeriod : Model -> Maybe Model
cancelPeriod (Timesheet model) =
    model.periodInProgress
        |> Maybe.andThen (\_ -> Just { model | periodInProgress = Nothing })
        |> Maybe.map Timesheet

addPeriod : Model -> Posix -> Maybe Model
addPeriod (Timesheet model) end =
    model.periodInProgress
        |> Maybe.map
            ( \start ->
                { model
                | finishedPeriods = model.finishedPeriods ++ [ (False, Period start end []) ]
                , periodInProgress = Nothing
                , cumulatedTicks = model.cumulatedTicks + ( duration start end )
                }
            )
        |> Maybe.map Timesheet


getCurrentlyRunningPeriodStart : Model -> Maybe Posix
getCurrentlyRunningPeriodStart (Timesheet model) = model.periodInProgress


duration : Posix -> Posix -> Int
duration start end =
     ((Time.posixToMillis end) - (Time.posixToMillis start)) // 1000



viewPeriodsTab : Model -> Html Msg
viewPeriodsTab (Timesheet model) =
    Html.div []
        [ viewPeriodsActions model
        , Html.table []
            ( Html.tr []
              [ Html.th [] [ ]
              , Html.th [] [ Html.text "Duration" ]
              , Html.th [] [ Html.text "Start" ]
              , Html.th [] [ Html.text "End" ]
              , Html.th [] [ Html.text "Integrations" ]
              ]
            :: (List.map viewPeriodRow model.finishedPeriods)
            )
        ]

viewPeriodsActions : TimesheetData -> Html Msg
viewPeriodsActions model =
    let
        actionsDisabled = (countSelectedItems model.finishedPeriods) == 0
        selectedDuration = (countSelectedDuration model.finishedPeriods)
    in
    Html.div []
        [ Html.text ((durationHumanReadable selectedDuration) ++ " selected")
        , Html.button [ onClick RemoveSelectedPeriods, disabled actionsDisabled ] [ Html.text "Remove" ]
        , Html.button [ onClick LogSelectedPeriodsToJira, disabled actionsDisabled ] [ Html.text "Log" ]
        ]

countSelectedItems : List (Bool, a) -> Int
countSelectedItems =
    List.foldl
        (\(selection, _) sum -> if selection then sum + 1 else sum)
        0

countSelectedDuration : List (Bool, Period) -> Int
countSelectedDuration periods =
    periods
        |> List.filter Tuple.first
        |> List.map (Tuple.second >> periodToDuration)
        |> List.foldl (+) 0


viewPeriodRow : (Bool, Period) -> Html Msg
viewPeriodRow (selected, period) =
    let
        (Period start end integrations) = period
    in
    Html.tr [ classList [("selected", selected)] ]
        [ Html.td [] [ Html.input [ type_ "checkbox", checked selected, onClick (TogglePeriodSelection period) ] [] ]
        , Html.td [ onClick (TogglePeriodSelection period) ] [ Html.text (durationHumanReadable (duration start end)) ]
        , Html.td [ onClick (TogglePeriodSelection period) ] [ viewPosix start ]
        , Html.td [ onClick (TogglePeriodSelection period) ] [ viewPosix end ]
        , Html.td [] (List.map Integrations.viewLogRef integrations)
        ]


periodToDuration : Period -> Int
periodToDuration (Period start end _) = duration start end


toggleFinishedPeriodSelection : Model -> Period -> Model
toggleFinishedPeriodSelection (Timesheet model) period =
    Timesheet
        ({
            model
            | finishedPeriods = List.map
                (\(selection, currentPeriod) ->
                    if currentPeriod == period then
                        (not selection, currentPeriod)
                    else
                        (selection, currentPeriod)
                ) model.finishedPeriods
        })

removeSelectedPeriods : Model -> Model
removeSelectedPeriods (Timesheet model) =
    let
        updatedPeriods = List.filter (Tuple.first >> not) model.finishedPeriods
    in
    Timesheet
        { model
        | finishedPeriods = updatedPeriods
        , cumulatedTicks = updatedPeriods
            |> List.map (Tuple.second >> periodToDuration)
            |> List.foldl (+) 0
        }

saveStateAdvised : Msg -> Bool
saveStateAdvised msg =
    case msg of
        StartNewPeriod _ -> True
        EndCurrentPeriod _ -> True
        RemoveSelectedPeriods -> True
        LogSelectedPeriodsToJira -> True
        _ -> False


-- DECODERS

decoder : Decoder Model
decoder =
    D.field "finishedPeriods" (D.list periodDecoder)
        |> D.andThen
            ( \finishedPeriods ->
                let
                    cumulatedTicks =
                        finishedPeriods
                            |> List.map periodToDuration
                            |> List.foldl (+) 0
                in
                    D.map5 TimesheetData
                        ( D.field "currentTab" tabDecoder )
                        ( D.succeed (List.map (\period -> (False, period)) finishedPeriods ))
                        ( D.field "periodInProgress" (D.nullable posixDecoder) )
                        ( D.succeed cumulatedTicks )
                        ( D.field "integrations" integrationsDecoder )
            )
        |> D.map Timesheet


tabDecoder : Decoder Tab
tabDecoder =
    D.string
        |> D.andThen
            ( \tab ->
                case tab of
                    "periods" -> D.succeed PeriodsTab
                    "jira" -> D.succeed JiraTab
                    _ -> D.fail ("Invalid tab: " ++ tab)
            )

periodDecoder : Decoder Period
periodDecoder =
    D.map3 Period
        ( D.field "start" posixDecoder )
        ( D.field "end" posixDecoder )
        ( D.field "logRefs" (D.list Integrations.logRefDecoder) )

integrationsDecoder : Decoder IntegrationsData
integrationsDecoder =
    D.map IntegrationsData
        (D.field "jira" JiraIssueLogManager.decoder)


-- ENCODING

encode : Model -> Value
encode (Timesheet { currentTab, finishedPeriods, periodInProgress, integrations }) =
    E.object
        [ ( "currentTab", encodeTab currentTab )
        , ( "finishedPeriods", E.list encodePeriod (List.map Tuple.second finishedPeriods) )
        , ( "periodInProgress"
          , periodInProgress
                |> Maybe.map encodePosix
                |> Maybe.withDefault E.null
          )
        , ( "integrations", encodeIntegrations integrations)
        ]


encodePeriod : Period -> Value
encodePeriod (Period start end logRefs) =
    E.object
        [ ("start", encodePosix start )
        , ("end", encodePosix end )
        , ("logRefs", E.list Integrations.encodeLogRef logRefs)
        ]


encodeTab : Tab -> Value
encodeTab tab =
    case tab of
        PeriodsTab -> E.string "periods"
        JiraTab -> E.string "jira"

encodeIntegrations : IntegrationsData -> Value
encodeIntegrations { jira } =
    E.object
        [ ("jira", JiraIssueLogManager.encode jira)
        ]