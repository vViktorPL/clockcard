module Integrations.Jira.Config exposing
    ( Model
    , Msg
    , decoder
    , init
    , encode
    , stateSaveAdvised
    , update
    , view
    , ProjectData
    , getValidDestinations
    , ValidDestination
    )

import Html exposing (Html, div, form)
import Html.Attributes exposing (class, classList, disabled, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Jira.Api exposing (Project, getAllProjects, getProjectData)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import SelectableList exposing (SelectableList)
import Task
import Time exposing (Posix)
import Assets exposing (getImageUrl)

type alias Seconds = Int

type Validity
    = Valid
    | Checking
    | Invalid


type alias ValidDestination =
    { id: String
    , name: String
    , cred: Jira.Api.Cred
    , projects: List ProjectData
    }

type alias Destination =
    { id : Int
    , name : String
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
    = NewDestination
    | SelectDestination Destination
    | DestinationUpdate DestinationUpdateMsg
    | ValidateDestination
    | DestinationValidated (Result Jira.Api.ApiCallError (List Project))


type alias Model =
    { destinations : Maybe (SelectableList Destination)
    , nextDestinationId : Int
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

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
                    Just (SelectableList.prepend selectableList (newDestination model.nextDestinationId) )

                Nothing ->
                    SelectableList.fromList [ (newDestination model.nextDestinationId) ]
        , nextDestinationId = model.nextDestinationId + 1
    }


newDestination : Int -> Destination
newDestination id =
    { id = id
    , name = ""
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
    (case model.destinations of
        Just selectableList ->
            div [ class "manager-window__content" ]
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
            div [ class "big-placeholder-message" ]
                [ Html.div [ class "big-icon" ] [ Html.text "🕸️" ]
                , Html.strong [] [ Html.text "No JIRA destinations added yet. "]
                , Html.p []
                    [ Html.text "Please "
                    , Html.a [ onClick NewDestination ] [ Html.text "add first one" ]
                     , Html.text "."
                    ]
                ]
    )


viewDestination : Destination -> Bool -> Html Msg
viewDestination destination isSelected =
    div [ classList [ ( "selected", isSelected ) ] ]
        [ Html.text
            (
                ( case destination.name of
                    "" -> "New untitled destination"
                    name -> name
                )
                ++
                ( case destination.valid of
                    Valid -> " ✅"
                    Checking -> " ⌛"
                    Invalid -> " ❌"
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


init : Model
init =
    { destinations = Nothing
    , nextDestinationId = 1
    }


stateSaveAdvised : Msg -> Bool
stateSaveAdvised msg =
    case msg of
        DestinationValidated _ ->
            True

        _ ->
            False


decoder : Decoder Model
decoder =
    D.map2 Model
        (D.field "destinations" <| D.nullable (SelectableList.decoder destinationDecoder))
        (D.field "nextDestinationId" D.int)


encode : Model -> Value
encode model =
    E.object
        [
            ("destinations"
            , case model.destinations of
                  Just selectableList ->
                      SelectableList.normalize encodeDestination selectableList

                  Nothing ->
                      E.null
            )
        , ("nextDestinationId", E.int model.nextDestinationId)
        ]



encodeDestination : Destination -> E.Value
encodeDestination destination =
    E.object
        [ ( "id", E.int destination.id )
        , ( "name", E.string destination.name )
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
    D.map7 Destination
        (D.field "id" D.int)
        (D.field "name" D.string)
        (D.field "host" D.string)
        (D.field "authUsername" D.string)
        (D.field "authPassword" D.string)
        (D.field "projects" (D.nullable (D.list projectDecoder)))
        (D.field "valid" validDecoder)


projectDecoder : Decoder ProjectData
projectDecoder =
    D.map4 ProjectData
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "key" D.string)
        (D.field "avatarUrls" (D.keyValuePairs D.string))


validDecoder : Decoder Validity
validDecoder =
    D.bool
        |> D.map
            (\valid ->
                if valid then
                    Valid

                else
                    Invalid
            )

destinationToCred : Destination -> Result String Jira.Api.Cred
destinationToCred destination =
    Jira.Api.createBasicAuthCred destination.host ( destination.authUsername, destination.authPassword )

getValidDestinations : Model -> List ValidDestination
getValidDestinations model =
    model.destinations
        |> Maybe.map SelectableList.getItems
        |> Maybe.map
            ( List.filterMap
                ( \destination ->
                    case (destination.valid, destinationToCred destination) of
                        (Valid, Ok cred) ->
                            Just
                                { id = String.fromInt destination.id
                                , name = destination.name
                                , cred = cred
                                , projects = destination.projects |> Maybe.withDefault []
                                }

                        _ -> Nothing
                )
            )
        |> Maybe.withDefault []

