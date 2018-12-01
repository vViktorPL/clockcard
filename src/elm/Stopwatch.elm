module Stopwatch exposing (Model, paused, running, view)

import Html exposing (Html)
import Time exposing (Posix)


type Model
    = RunningStopwatch Int Posix
    | PausedStopwatch Int


paused cumulatedTicks =
    PausedStopwatch cumulatedTicks


running cumulatedTicks startTime =
    RunningStopwatch cumulatedTicks startTime


view : Model -> Posix -> Html msg
view model currentTime =
    (case model of
        PausedStopwatch cumulatedTicks ->
            cumulatedTicks

        RunningStopwatch cumulatedTicks startTime ->
            cumulatedTicks + timeDiffInSecs currentTime startTime
    )
        |> formatTicks
        |> Html.text


timeDiffInSecs : Posix -> Posix -> Int
timeDiffInSecs time1 time2 =
    (Time.posixToMillis time1 - Time.posixToMillis time2) // 1000


formatTicks : Int -> String
formatTicks stopwatchTicks =
    let
        hours =
            stopwatchTicks // 3600

        minutes =
            modBy 3600 stopwatchTicks // 60

        secs =
            stopwatchTicks - hours * 3600 - minutes * 60
    in
    String.fromInt hours ++ ":" ++ formatZero minutes ++ ":" ++ formatZero secs


formatZero : Int -> String
formatZero number =
    if number < 10 then
        "0" ++ String.fromInt number

    else
        String.fromInt number
