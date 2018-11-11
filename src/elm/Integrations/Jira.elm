port module Integrations.Jira exposing
    ( Model
    , Msg
    , decoder
    , init
    , normalize
    , stateSaveAdvised
    , subscriptions
    , update
    , view
    )

import Html exposing (Html, div, form)
import Html.Attributes exposing (class, classList, disabled, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Jira.Api exposing (Project, getAllProjects, getProjectData)
import Json.Decode exposing (Decoder, bool, field, keyValuePairs, list, map3, nullable, string)
import Json.Encode as E
import SelectableList exposing (SelectableList)
import Task


port showJIRAManager : (() -> msg) -> Sub msg


type Validity
    = Valid
    | Checking
    | Invalid


type alias Destination =
    { name : String
    , host : String
    , authUsername : String
    , authPassword : String
    , projects : Maybe (List ProjectData)
    , valid : Validity
    }


type alias ProjectData =
    { id : String
    , name : String
    , key : String
    , avatarUrls : List ( String, String )
    }


type DestinationUpdateMsg
    = UpdateName String
    | UpdateHost String
    | UpdateUsername String
    | UpdatePassword String


type Msg
    = ShowManager
    | HideManager
    | NewDestination
    | SelectDestination Destination
    | DestinationUpdate DestinationUpdateMsg
    | ValidateDestination
    | DestinationValidated (Result Jira.Api.ApiCallError (List Project))


type alias Model =
    { destinations : Maybe (SelectableList Destination)
    , managerVisible : Bool
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowManager ->
            ( { model | managerVisible = True }, Cmd.none )

        HideManager ->
            ( { model | managerVisible = False }, Cmd.none )

        SelectDestination destination ->
            ( selectDestination model destination, Cmd.none )

        NewDestination ->
            ( addNewDestination model, Cmd.none )

        DestinationUpdate destUpdateMsg ->
            ( updateSelectedDestination destUpdateMsg model, Cmd.none )

        ValidateDestination ->
            let
                credentialsResult =
                    model.destinations
                        |> Maybe.map SelectableList.getSelected
                        |> Maybe.map
                            (\destination ->
                                Jira.Api.createBasicAuthCred
                                    destination.host
                                    ( destination.authUsername, destination.authPassword )
                            )
                        |> Maybe.withDefault (Err "")
            in
            case credentialsResult of
                Ok cred ->
                    ( mapSelectedDestination (\destination -> { destination | valid = Checking }) model
                    , Task.attempt DestinationValidated (Jira.Api.getAllProjects cred)
                    )

                Err _ ->
                    ( mapSelectedDestination (\destination -> { destination | valid = Invalid }) model
                    , Cmd.none
                    )

        DestinationValidated (Err _) ->
            ( mapSelectedDestination (\destination -> { destination | valid = Invalid }) model, Cmd.none )

        DestinationValidated (Ok projects) ->
            ( mapSelectedDestination
                (\destination ->
                    { destination
                        | projects = Just (List.map projectData projects)
                        , valid = Valid
                    }
                )
                model
            , Cmd.none
            )


projectData : Project -> ProjectData
projectData project =
    let
        data =
            Jira.Api.getProjectData project
    in
    { id = data.id
    , name = data.name
    , key = data.key
    , avatarUrls = data.avatarUrls
    }


selectDestination : Model -> Destination -> Model
selectDestination model destination =
    { model | destinations = Maybe.andThen (SelectableList.select destination) model.destinations }


addNewDestination : Model -> Model
addNewDestination model =
    { model
        | destinations =
            case model.destinations of
                Just selectableList ->
                    Just (SelectableList.prepend selectableList newDestination)

                Nothing ->
                    SelectableList.fromList [ newDestination ]
    }


newDestination : Destination
newDestination =
    { name = ""
    , host = ""
    , authUsername = ""
    , authPassword = ""
    , projects = Nothing
    , valid = Invalid
    }


mapSelectedDestination : (Destination -> Destination) -> Model -> Model
mapSelectedDestination f model =
    { model | destinations = Maybe.map (SelectableList.mapSelected f) model.destinations }


updateSelectedDestination : DestinationUpdateMsg -> Model -> Model
updateSelectedDestination msg model =
    mapSelectedDestination
        (\selectedDestination ->
            case msg of
                UpdateHost newHost ->
                    { selectedDestination | host = newHost, valid = Invalid }

                UpdateName newName ->
                    { selectedDestination | name = newName }

                UpdateUsername newUsername ->
                    { selectedDestination | authUsername = newUsername, valid = Invalid }

                UpdatePassword newPassword ->
                    { selectedDestination | authPassword = newPassword, valid = Invalid }
        )
        model


view : Model -> Html Msg
view model =
    div
        [ classList
            [ ( "manager-window", True )
            , ( "hidden", not model.managerVisible )
            ]
        ]
        ([ viewTitlebar ]
            ++ (case model.destinations of
                    Just selectableList ->
                        [ div [ class "manager-window__destinations-list" ]
                            (let
                                isSelected =
                                    \destination -> SelectableList.isSelected destination selectableList
                             in
                             selectableList
                                |> SelectableList.map (\destination -> viewDestination destination (isSelected destination))
                                |> SelectableList.getItems
                            )
                        , div [ class "manager-window__current-destination-form" ]
                            [ viewDestinationForm (SelectableList.getSelected selectableList) ]
                        ]

                    Nothing ->
                        [ Html.text "No JIRA destinations added. "
                        , Html.a [ onClick NewDestination ] [ Html.text "You can add one" ]
                        , Html.text "."
                        ]
               )
        )


viewTitlebar : Html Msg
viewTitlebar =
    div [ class "manager-window__titlebar" ] [ Html.text "JIRA integration manager", viewCloseButton ]


viewCloseButton =
    div [ onClick HideManager, class "manager-window__close-button" ] [ Html.text "✕" ]


viewDestination : Destination -> Bool -> Html Msg
viewDestination destination isSelected =
    div [ classList [ ( "selected", isSelected ) ] ]
        [ Html.text
            (destination.name
                ++ (if destination.valid == Valid then
                        " ✅"

                    else
                        ""
                   )
            )
        , Html.span []
            [ Html.text
                (case ( destination.valid, destination.projects ) of
                    ( Valid, Just projects ) ->
                        "(" ++ String.fromInt (List.length projects) ++ " projects found)"

                    _ ->
                        ""
                )
            ]
        ]


viewFormInput : String -> Html msg -> Html msg
viewFormInput label input =
    div []
        [ Html.label [] [ Html.text (label ++ ":") ]
        , div [ style "display" "block" ] [ input ]
        ]


canValidateDestination : Destination -> Bool
canValidateDestination destination =
    case destination.valid of
        Invalid ->
            True

        _ ->
            False


enabled : Bool -> Html.Attribute msg
enabled value =
    disabled (not value)


viewDestinationForm : Destination -> Html Msg
viewDestinationForm destination =
    form [ onSubmit ValidateDestination ]
        [ viewFormInput "JIRA URL"
            (Html.input
                [ type_ "text"
                , value destination.host
                , onInput (\s -> DestinationUpdate (UpdateHost s))
                ]
                []
            )
        , viewFormInput "Name"
            (Html.input
                [ type_ "text"
                , value destination.name
                , onInput (\s -> DestinationUpdate (UpdateName s))
                ]
                []
            )
        , viewFormInput "User"
            (Html.input
                [ type_ "text"
                , value destination.authUsername
                , onInput (\s -> DestinationUpdate (UpdateUsername s))
                ]
                []
            )
        , viewFormInput "Password"
            (Html.input
                [ type_ "password"
                , value destination.authPassword
                , onInput (\s -> DestinationUpdate (UpdatePassword s))
                ]
                []
            )
        , Html.button
            [ enabled (canValidateDestination destination) ]
            [ Html.text "Validate & fetch projects" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    showJIRAManager (\_ -> ShowManager)


init : Model
init =
    { destinations = Nothing
    , managerVisible = False
    }


stateSaveAdvised : Msg -> Bool
stateSaveAdvised msg =
    case msg of
        DestinationValidated _ ->
            True

        HideManager ->
            True

        _ ->
            False


normalize : Model -> E.Value
normalize model =
    case model.destinations of
        Just selectableList ->
            SelectableList.normalize normalizeDestination selectableList

        Nothing ->
            E.null


decoder : Decoder Model
decoder =
    Json.Decode.map2 Model
        (nullable (SelectableList.decoder destinationDecoder))
        (Json.Decode.succeed False)


normalizeDestination : Destination -> E.Value
normalizeDestination destination =
    E.object
        [ ( "name", E.string destination.name )
        , ( "host", E.string destination.host )
        , ( "authUsername", E.string destination.authUsername )
        , ( "authPassword", E.string destination.authPassword )
        , ( "projects"
          , case destination.projects of
                Just projects ->
                    E.list normalizeProject projects

                Nothing ->
                    E.null
          )
        , ( "valid"
          , E.bool
                (case destination.valid of
                    Valid ->
                        True

                    _ ->
                        False
                )
          )
        ]


normalizeProject : ProjectData -> E.Value
normalizeProject project =
    E.object
        [ ( "id", E.string project.id )
        , ( "name", E.string project.name )
        , ( "key", E.string project.key )
        , ( "avatarUrls"
          , E.object <|
                List.map
                    (Tuple.mapSecond E.string)
                    project.avatarUrls
          )
        ]


destinationDecoder : Decoder Destination
destinationDecoder =
    Json.Decode.map6 Destination
        (field "name" string)
        (field "host" string)
        (field "authUsername" string)
        (field "authPassword" string)
        (field "projects" (nullable (list projectDecoder)))
        (field "valid" validDecoder)


projectDecoder : Decoder ProjectData
projectDecoder =
    Json.Decode.map4 ProjectData
        (field "id" string)
        (field "name" string)
        (field "key" string)
        (field "avatarUrls" (keyValuePairs string))


validDecoder : Decoder Validity
validDecoder =
    bool
        |> Json.Decode.map
            (\valid ->
                if valid then
                    Valid

                else
                    Invalid
            )
