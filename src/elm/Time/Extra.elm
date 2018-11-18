module Time.Extra exposing (posixDecoder, encodePosix)

import Time exposing (Posix)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)

posixDecoder : Decoder Posix
posixDecoder =
    D.map Time.millisToPosix D.int

encodePosix : Posix -> Value
encodePosix posix =
    E.int <| Time.posixToMillis posix