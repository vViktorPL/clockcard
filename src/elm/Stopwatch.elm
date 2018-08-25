module Stopwatch exposing (Model(..), Msg, blank, update, view, subscriptions, refresh)

import Time exposing (Time, second)
import Task exposing (perform)
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

type Msg = StartRequested
         | ReadyToStart Time
         | Tick Time
         | PauseRequested
         | ReadyToPause Time
         | Reset

type alias Period = (Time, Time)
type alias Periods = List Period
type alias WithPeriods a = { a | periods: Periods }
type alias StopwatchTicks = Int

type alias RunningStopwatchData = WithPeriods
    { currentPeriodStartTime: Time
    , cumulatedTicks: StopwatchTicks
    , currentTime: Time
    }

type alias PausedStopwatchData = WithPeriods
    { cumulatedTicks: StopwatchTicks
    }

type Model = RunningStopwatch RunningStopwatchData | PausedStopwatch PausedStopwatchData


blank : Model
blank = PausedStopwatch { cumulatedTicks = 0, periods = [] }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (msg, model) of
    (Tick time, RunningStopwatch stopwatchData) ->
        (RunningStopwatch { stopwatchData | currentTime = time }, Cmd.none)

    (StartRequested, PausedStopwatch _) ->
        (model, perform ReadyToStart Time.now)

    (ReadyToStart time, PausedStopwatch stopwatchData) ->
        (RunningStopwatch
            { cumulatedTicks = stopwatchData.cumulatedTicks
            , periods = stopwatchData.periods
            , currentTime = time
            , currentPeriodStartTime = time
            }
            , Cmd.none
        )

    (PauseRequested, RunningStopwatch _) ->
        (model, perform ReadyToPause Time.now)

    (ReadyToPause time, RunningStopwatch stopwatchData) ->
        let
            currentPeriod = (stopwatchData.currentPeriodStartTime, time)
            currentPeriodTicks = timeDiffInSecs time stopwatchData.currentPeriodStartTime
        in
            ( PausedStopwatch
                { periods = stopwatchData.periods ++ [currentPeriod]
                , cumulatedTicks = stopwatchData.cumulatedTicks + currentPeriodTicks
                }
            , Cmd.none
            )

    (Reset, _) ->
        (blank, Cmd.none)

    _ ->
        (model, Cmd.none)

view : Model -> Html Msg
view model = div [class "stopwatch"]
    [ div [class "stopwatch__display"] [display model]
    , div [class "stopwatch__controls"]
        [ case model of
            RunningStopwatch _ -> button [onClick PauseRequested] [text "❚❚ Pause"]
            PausedStopwatch _  -> button [onClick StartRequested] [text "Start ▶"]
        , button [onClick Reset] [text "Reset"]
        ]
    ]

subscriptions : Model -> Sub Msg
subscriptions model = case model of
    RunningStopwatch _ -> Time.every second Tick
    PausedStopwatch _ -> Sub.none

timeDiffInSecs : Time -> Time -> Int
timeDiffInSecs time1 time2 = round ((Time.inSeconds time1) - (Time.inSeconds time2))

display : Model -> Html Msg
display = readTicks >> formatTicks >> text

readTicks : Model -> StopwatchTicks
readTicks model = case model of
    PausedStopwatch data -> data.cumulatedTicks
    RunningStopwatch data -> data.cumulatedTicks + (timeDiffInSecs data.currentTime data.currentPeriodStartTime)

formatTicks : StopwatchTicks -> String
formatTicks stopwatchTicks = let
        hours = stopwatchTicks // 3600
        minutes = (stopwatchTicks % 3600) // 60
        secs = stopwatchTicks - hours * 3600 - minutes * 60
    in
        (toString hours) ++ ":" ++ (formatZero minutes) ++ ":" ++ (formatZero secs)

formatZero : Int -> String
formatZero number = if number < 10 then "0" ++ toString number else toString number

refresh : Cmd Msg
refresh = Task.perform Tick Time.now