module Time.Extra exposing (posixDecoder, encodePosix, humanReadableDurationToSecs)

import Time exposing (Posix)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Regex exposing (Regex)
import Maybe.Extra

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
            ( \sum ->
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

        unit = String.right 1 string
    in
    case (value, unit) of
        (Just hours, "h") -> Just (hours * 3600)
        (Just minutes, "m") -> Just (minutes * 60)
        (Just seconds, "s") -> Just seconds
        _ -> Nothing


humanReadableDurationRegex : Regex
humanReadableDurationRegex =
    Regex.fromString "^(\\d+h)?\\W?(\\d+m)?\\W?(\\d+s)?$"
        |> Maybe.withDefault Regex.never

