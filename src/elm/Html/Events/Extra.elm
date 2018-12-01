module Html.Events.Extra exposing (onEnter)

import Html exposing (Attribute)
import Html.Events exposing (keyCode, on)
import Json.Decode as Json


onEnter : msg -> Attribute msg
onEnter onEnterAction =
    on "keyup" <|
        Json.andThen
            (\keyCode ->
                if keyCode == 13 then
                    Json.succeed onEnterAction

                else
                    Json.fail (String.fromInt keyCode)
            )
            keyCode
