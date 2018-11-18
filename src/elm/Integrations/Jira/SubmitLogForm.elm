module Integrations.Jira.SubmitLogForm exposing (Model, Msg, update, view, init)

import Time exposing (Posix)
import Html exposing (Html)
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (value, selected)
import Json.Decode as D exposing (Decoder)

import Integrations
import Integrations.Jira.Config exposing (getValidDestinations, ValidDestination, ProjectData)

type alias Config = Integrations.Jira.Config.Model


type Model
    = EmptyConfig
    | SubmitLogForm
        { selectedDestination: ValidDestination
        , availableDestinations: List ValidDestination
        , selectedProject: Maybe String
        , selectedIssue: Maybe String
        , startDate: Posix
        , duration: Int
        }

type Msg
    = SelectDestination String
    | SelectProject
    | SelectIssue
    | ShowConfigManager
    | NoOp


init : Config -> Posix -> Int -> Model
init config startDate duration =
    let
        availableDestinations = getValidDestinations config
    in
    case availableDestinations of
        [] -> EmptyConfig
        firstDestination :: _ -> SubmitLogForm
            { selectedDestination = firstDestination
            , availableDestinations = availableDestinations
            , selectedProject = Nothing
            , selectedIssue = Nothing
            , startDate = startDate
            , duration = duration
            }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model) of
        ( SelectDestination value, SubmitLogForm form ) ->
            ( SubmitLogForm
                { form
                | selectedDestination =
                    Maybe.withDefault
                        form.selectedDestination
                        (getItemByIndexValue value form.availableDestinations)
                }
            , Cmd.none
            )

        _ -> (model, Cmd.none)

getItemByIndexValue : String -> List a -> Maybe a
getItemByIndexValue value list =
    String.toInt value
        |> Maybe.map (\index -> List.take index list)
        |> Maybe.andThen List.head



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
            Html.div []
                [ viewDestinationsSelect form.availableDestinations form.selectedDestination
                ]


onChange : (String -> msg) -> Html.Attribute msg
onChange msg =
    on "change" (D.map msg targetValue)

viewDestinationsSelect : List ValidDestination -> ValidDestination -> Html Msg
viewDestinationsSelect availableDestinations selectedDestination =
    Html.select [ onChange SelectDestination ]
        ( List.indexedMap
            (\index destination ->
                Html.option
                    [ value (String.fromInt index), selected (selectedDestination == destination) ]
                    [ Html.text (Tuple.first destination) ]
            )
            availableDestinations
        )