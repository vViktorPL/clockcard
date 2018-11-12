module Integrations.Jira.SubmitLogForm exposing (Model, Msg(ShowConfigManager), view, init)

import Time exposing (Posix)
import Html exposing (Html)
import Html.Events exposing (onClick)

import Integrations.Jira.Config exposing (getValidDestinations, ValidDestination)

type alias Config = Integrations.Jira.Config.Model


type Model
    = EmptyConfig
    | SubmitLogForm
        { selectedDestination: ValidDestination
        , selectedProject: Maybe String
        , selectedIssue: Maybe String
        , startDate: Posix
        , duration: Int
        }

type Msg
    = SelectDestination
    | SelectProject
    | SelectIssue
    | ShowConfigManager


init : Config -> Posix -> Int -> Model
init config startDate duration =
    case getValidDestinations config of
        [] -> EmptyConfig
        firstDestination :: _ -> SubmitLogForm
            { selectedDestination = firstDestination
            , selectedProject = Nothing
            , selectIssue = Nothing
            , startDate = startDate
            , duration = duration
            }

view : Model -> Html Msg
view model =
    case model of
        EmptyConfig ->
            Html.div []
                [ Html.text "There is no JIRA destination available. "
                , Html.span [ onClick ShowConfigManager ] [ Html.text "Configure one" ]
                , Html.text "."
                ]

        SubmitLogForm form ->
            Html.div [] []