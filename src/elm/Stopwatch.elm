module Stopwatch exposing
    ( Model
    , Msg
    , Period
    , blank
    , decoder
    , isRunning
    , normalize
    , refresh
    , stateSaveAdvised
    , subscriptions
    , update
    , view
    )

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, andThen, bool, field, int, index, list, map2)
import Json.Encode exposing (Value)
import Task exposing (perform)
import Time exposing (Posix)

type alias Time = Posix

type Msg
    = StartRequested
    | ReadyToStart Time
    | Tick Time
    | PauseRequested
    | ReadyToPause Time
    | Reset


type alias Period =
    ( Time, Time )


type alias Periods =
    List Period


type alias StopwatchTicks =
    Int


type alias RunningStopwatchData =
    { currentPeriodStartTime : Time
    , cumulatedTicks : StopwatchTicks
    , currentTime : Time
    , periods: Periods
    }


type alias PausedStopwatchData =
    { cumulatedTicks : StopwatchTicks
    , periods: Periods
    }


type Model
    = RunningStopwatch RunningStopwatchData
    | PausedStopwatch PausedStopwatchData


blank : Model
blank =
    PausedStopwatch { cumulatedTicks = 0, periods = [] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Tick time, RunningStopwatch stopwatchData ) ->
            ( RunningStopwatch { stopwatchData | currentTime = time }, Cmd.none )

        ( StartRequested, PausedStopwatch _ ) ->
            ( model, perform ReadyToStart Time.now )

        ( ReadyToStart time, PausedStopwatch stopwatchData ) ->
            ( RunningStopwatch
                { cumulatedTicks = stopwatchData.cumulatedTicks
                , periods = stopwatchData.periods
                , currentTime = time
                , currentPeriodStartTime = time
                }
            , Cmd.none
            )

        ( PauseRequested, RunningStopwatch _ ) ->
            ( model, perform ReadyToPause Time.now )

        ( ReadyToPause time, RunningStopwatch stopwatchData ) ->
            let
                currentPeriod =
                    ( stopwatchData.currentPeriodStartTime, time )

                currentPeriodTicks =
                    timeDiffInSecs time stopwatchData.currentPeriodStartTime
            in
            ( PausedStopwatch
                { periods = stopwatchData.periods ++ [ currentPeriod ]
                , cumulatedTicks = stopwatchData.cumulatedTicks + currentPeriodTicks
                }
            , Cmd.none
            )

        ( Reset, _ ) ->
            ( blank, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "stopwatch" ]
        [ div [ class "stopwatch__display" ] [ display model ]
        , div [ class "stopwatch__controls" ]
            [ case model of
                RunningStopwatch _ ->
                    button [ onClick PauseRequested ] [ text "❚❚ Pause" ]

                PausedStopwatch _ ->
                    button [ onClick StartRequested ] [ text "Start ▶" ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]
        ]


subscriptions : Sub Msg
subscriptions =
    -- Conditional subscriptions removed for now due to some Elm 0.19 issue https://github.com/elm/compiler/issues/1776
    Time.every 1000 Tick


timeDiffInSecs : Time -> Time -> Int
timeDiffInSecs time1 time2 =
    (Time.posixToMillis time1 - Time.posixToMillis time2) // 1000


display : Model -> Html Msg
display =
    readTicks >> formatTicks >> text


readTicks : Model -> StopwatchTicks
readTicks model =
    case model of
        PausedStopwatch data ->
            data.cumulatedTicks

        RunningStopwatch data ->
            data.cumulatedTicks + timeDiffInSecs data.currentTime data.currentPeriodStartTime


formatTicks : StopwatchTicks -> String
formatTicks stopwatchTicks =
    let
        hours =
            stopwatchTicks // 3600

        minutes =
            (modBy 3600 stopwatchTicks) // 60

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


refresh : Cmd Msg
refresh =
    Task.perform Tick Time.now



-- NORMALIZE / DECODE PART START


normalizePeriods : Periods -> Value
normalizePeriods periods =
    periods
        |> Json.Encode.list normalizePeriod


normalizePeriod : Period -> Value
normalizePeriod (start, end) =
    [start, end]
        |> List.map Time.posixToMillis
        |> Json.Encode.list Json.Encode.int

normalize : Model -> Value
normalize model =
    Json.Encode.object
        (case model of
            RunningStopwatch data ->
                [ ( "running", Json.Encode.bool True )
                , ( "currentPeriodStartTime", data.currentPeriodStartTime
                    |> Time.posixToMillis
                    |> Json.Encode.int
                  )
                , ( "periods", normalizePeriods data.periods )
                ]

            PausedStopwatch data ->
                [ ( "running", Json.Encode.bool False )
                , ( "periods", normalizePeriods data.periods )
                ]
        )


periodDecoder =
    Json.Decode.map2 (\a b -> ( Time.millisToPosix a, Time.millisToPosix b )) (index 0 int) (index 1 int)


decoder : Decoder Model
decoder =
    field "running" bool
        |> andThen
            (\running ->
                case running of
                    True ->
                        runningStopwatchDecoder

                    False ->
                        pausedStopwatchDecoder
            )


periodToTicks ( start, end ) =
    timeDiffInSecs end start


periodsToTicks periods =
    List.foldl (\period ticks -> ticks + periodToTicks period) 0 periods


runningStopwatchDecoder : Decoder Model
runningStopwatchDecoder =
    Json.Decode.map2
        (\currentPeriodStartTime periods ->
            RunningStopwatch <|
                RunningStopwatchData
                    (Time.millisToPosix currentPeriodStartTime)
                    (periodsToTicks periods)
                    (Time.millisToPosix 0)
                    periods
        )
        (field "currentPeriodStartTime" int)
        (field "periods" (list periodDecoder))


pausedStopwatchDecoder : Decoder Model
pausedStopwatchDecoder =
    Json.Decode.map
        (\periods ->
            PausedStopwatch
                { cumulatedTicks = periodsToTicks periods
                , periods = periods
                }
        )
        (field "periods" (list periodDecoder))


stateSaveAdvised : Msg -> Bool
stateSaveAdvised msg =
    case msg of
        ReadyToPause _ ->
            True

        ReadyToStart _ ->
            True

        _ ->
            False


isRunning : Model -> Bool
isRunning stopwatch =
    case stopwatch of
        RunningStopwatch _ ->
            True

        PausedStopwatch _ ->
            False
