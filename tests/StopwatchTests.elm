module StopwatchTests exposing (..)

import Stopwatch exposing (Model(..))

import Time exposing (Time)
import Expect exposing (Expectation, fail)
import Fuzz exposing (Fuzzer, int, list, string, constant, floatRange, andThen, tuple)
import Test exposing (..)
import Json.Encode
import Json.Decode exposing (decodeString)

minFuzzTime : Time
minFuzzTime = 1535703959045

maxFuzzTime : Time
maxFuzzTime = 1567240221350

periodFuzzer = floatRange minFuzzTime maxFuzzTime
    |> andThen (\startTime -> tuple (constant startTime, floatRange startTime maxFuzzTime))

periodsFuzzer = list periodFuzzer

periodsToJson periods =
    periods
        |> List.map (\(start, end) -> "[" ++ (toString start) ++ "," ++ (toString end) ++ "]")
        |> String.join ","
        |> \arrayContent -> ("[" ++ arrayContent ++ "]")

suite : Test
suite =
    describe "Stopwatch module"
        [ describe "Stopwatch.normalize"
            [ test "normalizes PausedStopwatch" <|
                \_ ->
                    Stopwatch.normalize Stopwatch.blank
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"running":false,"periods":[]}"""

            , test "normalizes RunningStopwatch" <|
                \_ ->
                    Stopwatch.normalize
                        ( RunningStopwatch
                            { currentPeriodStartTime = 1535715005000
                            , cumulatedTicks = 5400
                            , currentTime = 1535715005000
                            , periods =
                                [ (1535706245960,1535709845960)
                                , (1535713200000,1535715000000)
                                ]
                            }
                        )
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"running":true,"currentPeriodStartTime":1535715005000,"periods":[[1535706245960,1535709845960],[1535713200000,1535715000000]]}"""


            ]
        , describe "Stopwatch.decoder"
            [ test "decodes normalized StoppedStopwatch" <|
                \_ ->
                    decodeString Stopwatch.decoder ("""{"running":false,"periods":[[1535706245960,1535709845960],[1535713200000,1535715000000]]}""")
                        |> Expect.equal (
                            Ok
                                (PausedStopwatch
                                    { cumulatedTicks = 5400
                                    , periods =
                                        [ (1535706245960,1535709845960)
                                        , (1535713200000,1535715000000)
                                        ]
                                    }
                                )
                        )
            , test "decodes normalized RunningStopwatch" <|
                \_ ->
                    decodeString Stopwatch.decoder ("""{"running":true,"periods":[[1535706245960,1535709845960],[1535713200000,1535715000000]],"currentPeriodStartTime":1535715005000}""")
                     |> Expect.equal (
                        Ok
                            (RunningStopwatch
                                { cumulatedTicks = 5400
                                , periods =
                                    [ (1535706245960,1535709845960)
                                    , (1535713200000,1535715000000)
                                    ]
                                , currentPeriodStartTime = 1535715005000
                                , currentTime = 0
                                }
                            )
                    )
            ]
        ]
