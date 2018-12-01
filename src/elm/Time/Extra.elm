module Time.Extra exposing (durationHumanReadable, encodePosix, humanReadableDurationToSecs, posixDecoder, viewPosix)

import Html exposing (Html)
import Html.Attributes exposing (attribute, datetime)
import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Maybe.Extra
import Regex exposing (Regex)
import Time exposing (Posix)


posixDecoder : Decoder Posix
posixDecoder =
    D.map Time.millisToPosix D.int


encodePosix : Posix -> Value
encodePosix posix =
    E.int <| Time.posixToMillis posix


humanReadableDurationToSecs : String -> Maybe Int
humanReadableDurationToSecs durationString =
    Regex.find humanReadableDurationRegex durationString
        |> List.concatMap .submatches
        |> Maybe.Extra.values
        |> List.map parseHumanReadableValueWithUnit
        |> Maybe.Extra.combine
        |> Maybe.map List.sum
        |> Maybe.andThen
            (\sum ->
                if sum > 0 then
                    Just sum

                else
                    Nothing
            )


parseHumanReadableValueWithUnit : String -> Maybe Int
parseHumanReadableValueWithUnit string =
    let
        value =
            String.dropRight 1 string
                |> String.toInt

        unit =
            String.right 1 string
    in
    case ( value, unit ) of
        ( Just hours, "h" ) ->
            Just (hours * 3600)

        ( Just minutes, "m" ) ->
            Just (minutes * 60)

        ( Just seconds, "s" ) ->
            Just seconds

        _ ->
            Nothing


humanReadableDurationRegex : Regex
humanReadableDurationRegex =
    Regex.fromString "^(\\d+h)?\\W?(\\d+m)?\\W?(\\d+s)?$"
        |> Maybe.withDefault Regex.never


durationHumanReadable : Int -> String
durationHumanReadable totalSecs =
    if totalSecs == 0 then
        "0"

    else
        let
            hours =
                totalSecs // 3600

            minutes =
                (totalSecs - (hours * 3600)) // 60

            secs =
                if hours == 0 && minutes == 0 then
                    totalSecs - (hours * 3600) - (minutes * 60)

                else
                    0
        in
        [ ( hours, "h" ), ( minutes, "m" ), ( secs, "s" ) ]
            |> List.filter (\( count, _ ) -> count > 0)
            |> List.map (\( count, unit ) -> String.fromInt count ++ unit)
            |> String.join " "


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
