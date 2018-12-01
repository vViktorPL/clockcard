port module Integrations exposing
    ( Configs
    , ConfigsMsg
    , LogRef(..)
    , configsSaveAdvised
    , decodeConfigs
    , encodeConfigs
    , encodeLogRef
    , getJiraConfig
    , initConfigs
    , logRefDecoder
    , subscriptions
    , updateConfigManagers
    , viewConfigManagers
    , viewLogRef
    )

import Html exposing (Html, div)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Integrations.Jira.Config as JiraConfig
import Integrations.Jira.IssueLogManager as JiraIssueLogManager
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)


port showJiraManager : (() -> msg) -> Sub msg


type ConfigManager
    = None
    | Jira


type Configs
    = Configs ConfigManager JiraConfig.Model


type ConfigsMsg
    = ShowConfigManager ConfigManager
    | JiraConfigMsg JiraConfig.Msg


type LogRef
    = JiraLogRef JiraIssueLogManager.LogRef


viewConfigManagers : Configs -> Html ConfigsMsg
viewConfigManagers (Configs currentManager jiraConfig) =
    div []
        (List.map viewConfigManagerWindow
            [ ( currentManager == Jira
              , "Jira integration manager"
              , Html.map JiraConfigMsg (JiraConfig.view jiraConfig)
              )
            ]
        )


viewConfigManagerWindow : ( Bool, String, Html ConfigsMsg ) -> Html ConfigsMsg
viewConfigManagerWindow ( active, title, view ) =
    div
        [ classList
            [ ( "manager-window", True )
            , ( "hidden", not active )
            ]
        ]
        [ viewTitlebar title, view ]


viewTitlebar : String -> Html ConfigsMsg
viewTitlebar title =
    div [ class "manager-window__titlebar" ] [ Html.text title, viewCloseButton ]


viewCloseButton =
    div [ onClick (ShowConfigManager None), class "manager-window__close-button" ] [ Html.text "✕" ]


updateConfigManagers : ConfigsMsg -> Configs -> ( Configs, Cmd ConfigsMsg )
updateConfigManagers msg (Configs activeManager jiraConfig) =
    case msg of
        ShowConfigManager managerToActivate ->
            ( Configs managerToActivate jiraConfig, Cmd.none )

        JiraConfigMsg jiraMsg ->
            let
                ( newJiraConfig, jiraCmd ) =
                    JiraConfig.update jiraMsg jiraConfig
            in
            ( Configs activeManager newJiraConfig, Cmd.map JiraConfigMsg jiraCmd )


encodeConfigs : Configs -> Value
encodeConfigs (Configs _ jiraConfig) =
    E.object
        [ ( "jira", JiraConfig.encode jiraConfig )
        ]


decodeConfigs : Decoder Configs
decodeConfigs =
    D.map2 Configs
        (D.succeed None)
        (D.field "jira" JiraConfig.decoder)


initConfigs : Configs
initConfigs =
    Configs None JiraConfig.init


subscriptions : Sub ConfigsMsg
subscriptions =
    showJiraManager (\_ -> ShowConfigManager Jira)


configsSaveAdvised : ConfigsMsg -> Bool
configsSaveAdvised msg =
    case msg of
        JiraConfigMsg jiraMsg ->
            JiraConfig.stateSaveAdvised jiraMsg

        ShowConfigManager None ->
            True

        _ ->
            False


viewLogRef : LogRef -> Html msg
viewLogRef logRef =
    case logRef of
        JiraLogRef jiraLogRef ->
            JiraIssueLogManager.viewLogRef jiraLogRef


encodeLogRef : LogRef -> Value
encodeLogRef logRef =
    case logRef of
        JiraLogRef jiraLogRef ->
            E.object
                [ ( "type", E.string "jira" )
                , ( "data", JiraIssueLogManager.encodeLogRef jiraLogRef )
                ]


logRefDecoder : Decoder LogRef
logRefDecoder =
    D.field "type" D.string
        |> D.andThen
            (\integration ->
                D.field "data"
                    (case integration of
                        "jira" ->
                            D.map JiraLogRef JiraIssueLogManager.logRefDecoder

                        _ ->
                            D.fail ("Invalid integration type: " ++ integration)
                    )
            )


getJiraConfig : Configs -> JiraConfig.Model
getJiraConfig (Configs _ jiraConfig) =
    jiraConfig
