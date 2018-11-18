module Integrations.Jira.Log exposing (LogRef, Model, encodeLogRef, logRefDecoder, encode, decoder)

import Json.Encode as E exposing (Value)
import Json.Decode as D exposing (Decoder)

--import Html exposing (Html)
--import Html.Attributes
import Time exposing (Posix)
import Time.Extra exposing (posixDecoder, encodePosix)

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
type Model =
    Log LogData

type alias LogData =
    { id: LogRef
    , issueKey: String
    , issueUrl: String
    , loggedTime: Int
    , commitTime: Posix
    }

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
    E.object
        [ ("index", E.int logRef)
        ]

logRefDecoder : Decoder LogRef
logRefDecoder =
    D.map LogRef <|
        D.field "index" D.int

encode : Model -> Value
encode (Log data) =
    E.object
        [ ("id", encodeLogRef data.id)
        , ("issueKey", E.string data.issueKey)
        , ("issueUrl", E.string data.issueUrl)
        , ("loggedTime", E.int data.loggedTime)
        , ("commitTime", encodePosix data.commitTime)
        ]

decoder : Decoder Model
decoder =
    D.map Log <|
    D.map5 LogData
        (D.field "id" logRefDecoder)
        (D.field "issueKey" D.string)
        (D.field "issueUrl" D.string)
        (D.field "loggedTime" D.int)
        (D.field "commitTime" posixDecoder)