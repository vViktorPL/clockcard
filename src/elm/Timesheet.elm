module Timesheet exposing (Msg, Model, Period, update, view, init, decoder, encode, getCurrentlyRunningPeriodStart)

import Time exposing (Posix)
import Html exposing (Html)
import Html.Attributes exposing (class, classList, type_, checked, disabled)
import Iso8601
import Json.Encode as E exposing (Value)
import Json.Decode as D exposing (Decoder)

import Html.Events exposing (onClick)
import Integrations exposing (LogRef)
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

type Period = Period Posix Posix (List LogRef)

type Tab = PeriodsTab | JiraTab

type Model = Timesheet TimesheetData


type alias TimesheetData =
    { currentTab: Tab
    , finishedPeriods: List (Bool, Period)
    , periodInProgress: Maybe Posix
    , cumulatedTicks: Int
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SwitchTab tab -> (switchTab model tab, Cmd.none)
        StartNewPeriodRequested -> ( model, Task.perform StartNewPeriod Time.now )
        StartNewPeriod startTime -> (Maybe.withDefault model (startPeriod model startTime), Cmd.none)
        EndCurrentPeriodRequested -> ( model, Task.perform EndCurrentPeriod Time.now )
        EndCurrentPeriod endTime -> (Maybe.withDefault model (addPeriod model endTime), Cmd.none)
        TogglePeriodSelection period -> (toggleFinishedPeriodSelection model period, Cmd.none)
        RemoveSelectedPeriods -> (removeSelectedPeriods model, Cmd.none)

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
        }

stopwatch : Maybe Posix -> Int -> Stopwatch.Model
stopwatch periodInProgress cumulatedTicks =
    case periodInProgress of
        Just startTime -> Stopwatch.running cumulatedTicks startTime
        Nothing -> Stopwatch.paused cumulatedTicks


view : Model -> Posix -> Html Msg
view (Timesheet model) currentTime =
    Html.div [ class "selected-item-form" ]
        [ Html.div [ class "stopwatch" ]
            [ Stopwatch.view (stopwatch model.periodInProgress model.cumulatedTicks) currentTime
            , Html.div [ class "stopwatch__controls" ]
                [ case model.periodInProgress of
                    Nothing -> Html.button [ onClick StartNewPeriodRequested ] [ Html.text "Start ▶" ]
                    Just _ -> Html.button [ onClick EndCurrentPeriodRequested ] [ Html.text "❚❚ Pause" ]
                ]

            ]
        , Html.div [ class "timesheet" ]
            [ Html.div [ class "tabs" ]
                [ viewTab model PeriodsTab "Periods"
                , viewTab model JiraTab "Jira Log"
                ]
            , Html.div [ class "timesheet__tab-content" ]
                [ case model.currentTab of
                      PeriodsTab -> viewPeriodsTab (Timesheet model)
                      JiraTab -> Html.div [] [ Html.text "Jira Log tab not implemented yet" ]
                ]
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

durationHumanReadable : Posix -> Posix -> String
durationHumanReadable start end =
    let
        totalSecs = duration start end
        hours = totalSecs // 3600
        minutes = (totalSecs - (hours * 3600)) // 60
        secs = totalSecs - (hours * 3600) - (minutes * 60)
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
        selectedPeriodsCount = (countSelectedItems model.finishedPeriods)
    in
    Html.div []
        [ Html.text ((String.fromInt selectedPeriodsCount) ++ " selected")
        , Html.button [ onClick RemoveSelectedPeriods, disabled (selectedPeriodsCount == 0) ] [ Html.text "Remove" ]
        , Html.button [ ] [ Html.text "Log" ]
        ]

countSelectedItems : List (Bool, a) -> Int
countSelectedItems =
    List.foldl
        (\(selection, _) sum -> if selection then sum + 1 else sum)
        0


viewPeriodRow : (Bool, Period) -> Html Msg
viewPeriodRow (selected, period) =
    let
        (Period start end integrations) = period
    in
    Html.tr [ classList [("selected", selected)] ]
        [ Html.td [] [ Html.input [ type_ "checkbox", checked selected, onClick (TogglePeriodSelection period) ] [] ]
        , Html.td [ onClick (TogglePeriodSelection period) ] [ Html.text (durationHumanReadable start end) ]
        , Html.td [ onClick (TogglePeriodSelection period) ] [ Html.text (Iso8601.fromTime start)]
        , Html.td [ onClick (TogglePeriodSelection period) ] [ Html.text (Iso8601.fromTime end)]
        , Html.td [] []
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
    Timesheet
        { model
        | finishedPeriods = List.filter (Tuple.first >> not) model.finishedPeriods
        }


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
                    D.map4 TimesheetData
                        ( D.field "currentTab" tabDecoder )
                        ( D.succeed (List.map (\period -> (False, period)) finishedPeriods ))
                        ( D.field "periodInProgress" (D.nullable posixDecoder) )
                        ( D.succeed cumulatedTicks )
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

posixDecoder : Decoder Posix
posixDecoder =
    D.map Time.millisToPosix D.int

-- ENCODING

encode : Model -> Value
encode (Timesheet { currentTab, finishedPeriods, periodInProgress }) =
    E.object
        [ ( "currentTab", encodeTab currentTab )
        , ( "finishedPeriods", E.list encodePeriod (List.map Tuple.second finishedPeriods) )
        , ( "periodInProgress"
          , periodInProgress
                |> Maybe.map encodePosix
                |> Maybe.withDefault E.null
          )
        ]

encodePeriod : Period -> Value
encodePeriod (Period start end logRefs) =
    E.object
        [ ("start", encodePosix start )
        , ("end", encodePosix end )
        , ("logRefs", E.list Integrations.encodeLogRef logRefs)
        ]

encodePosix : Posix -> Value
encodePosix posix =
    E.int <| Time.posixToMillis posix

encodeTab : Tab -> Value
encodeTab tab =
    case tab of
        PeriodsTab -> E.string "periods"
        JiraTab -> E.string "jira"