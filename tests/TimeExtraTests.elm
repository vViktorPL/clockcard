module TimeExtraTests exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Time.Extra exposing (humanReadableDurationToSecs)

durationExampleData =
    [ ("1h", Just 3600)
    , ("1h 30m", Just 5400)
    , ("5m", Just 300)
    , ("5m1s", Just 301)
    , ("1h1m1s", Just 3661)
    , ("x", Nothing)
    , ("", Nothing)
    , ("123", Nothing)
    ]

suite : Test
suite =
    describe "Time.Extra module"
        [ describe "humanReadableDurationToSecs"
            ( List.map
                ( \(input, expectedOutput) ->
                    test ("value: \"" ++ input ++ "\"") <|
                        \_ ->
                            humanReadableDurationToSecs input
                                |> Expect.equal expectedOutput
                )
                durationExampleData
            )
        ]