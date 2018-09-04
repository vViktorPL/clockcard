module Issue exposing (Model, IssueId, normalize, decoder)

import Stopwatch
import Json.Encode exposing (Value)
import Json.Decode exposing (Decoder, field, string, int)

type alias IssueId = Int
type alias Model =
    { id: IssueId
    , name: String
    , stopwatch: Stopwatch.Model
    }

normalize : Model -> Value
normalize model =
    Json.Encode.object
        [ ("id", Json.Encode.int model.id)
        , ("name", Json.Encode.string model.name)
        , ("stopwatch", Stopwatch.normalize model.stopwatch)
        ]

decoder : Decoder Model
decoder = Json.Decode.map3
    Model
    (field "id" int)
    (field "name" string)
    (field "stopwatch" Stopwatch.decoder)