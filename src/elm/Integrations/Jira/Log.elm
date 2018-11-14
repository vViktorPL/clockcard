module Integrations.Jira.Log exposing (..)

import Json.Encode exposing (Value)
import Json.Decode as D exposing (Decoder)

--import Html exposing (Html)
--import Html.Attributes
--import Time exposing (Posix)
--
--import Assets exposing (getImageUrl)
--import Integrations.Jira.Config
--
--
--type alias Seconds = Int
--
--
type LogRef = LogRef Int
--
--type LogRefMsg = OpenLogRef LogRef
--
--type alias Model =
--    { id: LogRef
--    , issueKey: String
--    , issueUrl: String
--    , loggedTime: Seconds
--    , commitTime: Posix
--    }
--
--
--
--viewLogRef : LogRef -> Html LogRefMsg
--viewLogRef logRef =
--    Html.div []
--        [ Html.img [Html.Attributes.src (getImageUrl "jira-icon.svg")] []
--        , Html.text ("#" ++ logRef)
--        ]

encodeLogRef : LogRef -> Value
encodeLogRef (LogRef logRef) =
    Json.Encode.object
        [ ("index", Json.Encode.int logRef)
        ]

logRefDecoder : Decoder LogRef
logRefDecoder =
    D.map LogRef <|
        D.field "index" D.int
