module Integrations.Jira.LogTable exposing (Model, init, decoder, encode)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)

import Integrations.Jira.Log as Log
--
type Model = LogTable (List Log.Model) Int
--
--
--
--view : Model -> Html Msg

init : Model
init = LogTable [] 1

decoder : Decoder Model
decoder =
    D.map2 LogTable
        (D.field "entries" (D.list Log.decoder))
        (D.field "currentId" D.int)

encode : Model -> Value
encode (LogTable entries currentId) =
    E.object
        [ ("entries", E.list Log.encode entries)
        , ("currentId", E.int currentId)
        ]
