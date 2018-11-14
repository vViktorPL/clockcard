module Timesheet exposing (Msg, Model, Period, update, view, init, decoder, encode, getCurrentlyRunningPeriodStart)

import Time exposing (Posix)
import Html exposing (Html)
import Html.Attributes exposing (class)
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

type Period = Period Posix Posix (List LogRef)

type Tab = PeriodsTab | JiraTab

type Model = Timesheet TimesheetData


type alias TimesheetData =
    { currentTab: Tab
    , finishedPeriods: List Period
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
    Html.div []
        [ Html.div [ class "stopwatch" ]
            [ Stopwatch.view (stopwatch model.periodInProgress model.cumulatedTicks) currentTime
            , Html.div [ class "stopwatch__controls" ]
                [ case model.periodInProgress of
                    Nothing -> Html.button [ onClick StartNewPeriodRequested ] [ Html.text "Start ▶" ]
                    Just _ -> Html.button [ onClick EndCurrentPeriodRequested ] [ Html.text "❚❚ Pause" ]
                ]

            ]
        , Html.div [ class "timesheet" ]
            [ Html.div []
                [ Html.div [ onClick (SwitchTab PeriodsTab) ] [ Html.text "Periods" ]
                , Html.div [ onClick (SwitchTab JiraTab) ] [ Html.text "Jira Log" ]
                ]
            , Html.div []
                [ case model.currentTab of
                      PeriodsTab -> viewPeriodsTab (Timesheet model)
                      JiraTab -> Html.div [] [ Html.text "Jira Log tab not implemented yet" ]
                ]
            ]
        ]


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
                | finishedPeriods = model.finishedPeriods ++ [ Period start end [] ]
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
    Html.table []
        ( Html.tr []
            [ Html.th [] [ Html.text "Duration" ]
            , Html.th [] [ Html.text "Start" ]
            , Html.th [] [ Html.text "End" ]
            , Html.th [] [ Html.text "Integrations" ]
            ]
        :: (List.map viewPeriodRow model.finishedPeriods)
        )

viewPeriodRow : Period -> Html Msg
viewPeriodRow (Period start end integrations) =
    Html.tr []
        [ Html.td [] [ Html.text (durationHumanReadable start end) ]
        , Html.td [] [ Html.text (Iso8601.fromTime start)]
        , Html.td [] [ Html.text (Iso8601.fromTime end)]
        , Html.td [] []
        ]

periodToDuration : Period -> Int
periodToDuration (Period start end _) = duration start end


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
                        ( D.succeed finishedPeriods )
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
        , ( "finishedPeriods", E.list encodePeriod finishedPeriods )
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