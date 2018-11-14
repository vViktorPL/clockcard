module Issue exposing (IssueId, Model, decoder, normalize)

import Json.Decode exposing (Decoder, field, int, string)
import Json.Encode exposing (Value)
import Timesheet


type alias IssueId =
    Int


type alias Model =
    { id : IssueId
    , name : String
    , timesheet : Timesheet.Model
    }


normalize : Model -> Value
normalize model =
    Json.Encode.object
        [ ( "id", Json.Encode.int model.id )
        , ( "name", Json.Encode.string model.name )
        , ( "timesheet", Timesheet.encode model.timesheet )
        ]


decoder : Decoder Model
decoder =
    Json.Decode.map3
        Model
        (field "id" int)
        (field "name" string)
        (field "timesheet" Timesheet.decoder)
