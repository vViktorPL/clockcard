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

import Time.Extra exposing (posixDecoder, encodePosix)
import Integrations exposing (LogRef, getJiraConfig)
import Integrations.Jira.Config
import Integrations.Jira.LogTable
import Integrations.Jira.SubmitLogForm
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
    | JiraFormMsg Integrations.Jira.SubmitLogForm.Msg

type Period = Period Posix Posix (List LogRef)

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
    { jira: JiraIntegrationData
    }

type alias JiraIntegrationData =
    { log: Integrations.Jira.LogTable.Model
    , form: Maybe Integrations.Jira.SubmitLogForm.Model
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
            ( openJiraLogForm
                (getJiraConfig integrationConfigs)
                model
                (getSelectedPeriods model)
            , Cmd.none
            )

        JiraFormMsg jiraMsg -> updateJiraForm jiraMsg model


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
            { jira =
                { log = Integrations.Jira.LogTable.init
                , form = Nothing
                }

            }
        }

getModelData : Model -> TimesheetData
getModelData (Timesheet model) = model

updateJiraForm : Integrations.Jira.SubmitLogForm.Msg -> Model -> (Model, Cmd Msg)
updateJiraForm msg model =
    let
        modelData = getModelData model
        (updatedForm, formCmd) =
            modelData.integrations.jira.form
                |> Maybe.map (Integrations.Jira.SubmitLogForm.update msg)
                |> Maybe.map (Tuple.mapFirst Just)
                |> Maybe.withDefault (Nothing, Cmd.none)
    in
    ( mapJiraIntegration ( \jiraModel -> { jiraModel | form = updatedForm }) model
    , Cmd.map JiraFormMsg formCmd
    )


mapJiraIntegration : (JiraIntegrationData -> JiraIntegrationData) -> Model -> Model
mapJiraIntegration f (Timesheet model) =
    let
        integrations = model.integrations
        jiraIntegration = integrations.jira
    in
        Timesheet { model | integrations = { integrations | jira = f jiraIntegration } }

openJiraLogForm : Integrations.Jira.Config.Model -> Model -> List Period -> Model
openJiraLogForm jiraConfig model periods =
    let
        defaultStartTime = List.head periods
            |> Maybe.map (\(Period startTime _ _) -> startTime)
            |> Maybe.withDefault (Time.millisToPosix 0)
    in
        mapJiraIntegration
            ( \jiraModel ->
                { jiraModel
                | form = Just <|
                    Integrations.Jira.SubmitLogForm.init
                        jiraConfig
                        defaultStartTime
                        ( periods
                            |> List.map periodToDuration
                            |> List.foldl (+) 0
                        )
                }
            )
            model


stopwatch : Maybe Posix -> Int -> Stopwatch.Model
stopwatch periodInProgress cumulatedTicks =
    case periodInProgress of
        Just startTime -> Stopwatch.running cumulatedTicks startTime
        Nothing -> Stopwatch.paused cumulatedTicks

formOpened : TimesheetData -> Bool
formOpened model =
    case model.integrations.jira.form of
        Just _ -> True
        Nothing -> False


view : Model -> Posix -> Html Msg
view (Timesheet model) currentTime =
    Html.div [ class "selected-item-form" ]
        [ case (model.integrations.jira.form) of
            Just jiraForm -> viewJiraForm jiraForm
            Nothing -> viewStopwatch model currentTime

        , Html.div [ class "timesheet" ]
            [ Html.div [ class "tabs" ]
                [ viewTab model PeriodsTab "Periods"
                , viewTab model JiraTab "Jira Log"
                ]
            , Html.fieldset [ class "timesheet__tab-content", disabled (formOpened model) ]
                [ case model.currentTab of
                      PeriodsTab -> viewPeriodsTab (Timesheet model)
                      JiraTab -> Html.div [] [ Html.text "Jira Log tab not implemented yet" ]
                ]
            ]
        ]

viewStopwatch : TimesheetData -> Posix -> Html Msg
viewStopwatch model currentTime =
    Html.div [ class "stopwatch" ]
        [ Stopwatch.view (stopwatch model.periodInProgress model.cumulatedTicks) currentTime
        , Html.div [ class "stopwatch__controls" ]
            [ case model.periodInProgress of
                Nothing -> Html.button [ onClick StartNewPeriodRequested ] [ Html.text "Start ▶" ]
                Just _ -> Html.button [ onClick EndCurrentPeriodRequested ] [ Html.text "❚❚ Pause" ]
            ]

        ]

viewJiraForm jiraForm =
    Html.div [] [ Html.map JiraFormMsg (Integrations.Jira.SubmitLogForm.view jiraForm) ]

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

durationHumanReadable : Int -> String
durationHumanReadable totalSecs =
    if totalSecs == 0 then "0"
    else
        let
            hours = totalSecs // 3600
            minutes = (totalSecs - (hours * 3600)) // 60
            secs =
                if hours == 0 && minutes == 0 then
                    totalSecs - (hours * 3600) - (minutes * 60)
                else
                    0
        in
            [ (hours, "h"), (minutes, "m"), (secs, "s") ]
                |> List.filter (\(count, _) -> count > 0)
                |> List.map (\(count, unit) -> String.fromInt count ++ unit)
                |> String.join " "

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
        , Html.td [] []
        ]

viewPosix : Posix -> Html msg
viewPosix time =
    Html.node "local-time"
        [ datetime (Iso8601.fromTime time)
        , attribute "day" "numeric"
        , attribute "month" "short"
        , attribute "year" "numeric"
        , attribute "hour" "numeric"
        , attribute "minute" "numeric"
        ]
        []

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
        jiraIntegrationDecoder


jiraIntegrationDecoder : Decoder JiraIntegrationData
jiraIntegrationDecoder =
    D.map2 JiraIntegrationData
        (D.field "jira" Integrations.Jira.LogTable.decoder)
        (D.succeed Nothing)


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
        [ ("jira", Integrations.Jira.LogTable.encode jira.log)
        ]