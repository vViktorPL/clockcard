port module Integrations exposing
    ( Configs
    , ConfigsMsg
    , viewConfigManagers
    , updateConfigManagers
    , normalizeConfigs
    , decodeConfigs
    , initConfigs
    , subscriptions
    , configsSaveAdvised
    )

import Html exposing (Html, div)
import Html.Attributes exposing (classList, class)
import Html.Events exposing (onClick)
import Json.Encode exposing (Value)
import Json.Decode exposing (Decoder)

import Integrations.Jira.Config as JiraConfig
import Integrations.Jira.Log as JiraLog

port showJiraManager : (() -> msg) -> Sub msg

type ConfigManager = None | Jira
type Configs = Configs ConfigManager JiraConfig.Model
type ConfigsMsg
    = ShowConfigManager ConfigManager
    | JiraConfigMsg JiraConfig.Msg

--type LogRef
--    = JiraLogRef JiraLog.LogRef
--
--type LogTable = JiraTab

viewConfigManagers : Configs -> Html ConfigsMsg
viewConfigManagers (Configs currentManager jiraConfig) =
    div []
        ( List.map viewConfigManagerWindow
            [ ( currentManager == Jira
              , "Jira integration manager"
              , Html.map JiraConfigMsg (JiraConfig.view jiraConfig)
              )
            ]
        )

viewConfigManagerWindow : (Bool, String, Html ConfigsMsg) -> Html ConfigsMsg
viewConfigManagerWindow (active, title, view) =
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


updateConfigManagers : ConfigsMsg -> Configs -> (Configs, Cmd ConfigsMsg)
updateConfigManagers msg (Configs activeManager jiraConfig) =
    case msg of
        ShowConfigManager managerToActivate ->
            (Configs managerToActivate jiraConfig, Cmd.none)

        JiraConfigMsg jiraMsg ->
            let
                ( newJiraConfig, jiraCmd ) =  JiraConfig.update jiraMsg jiraConfig
            in
                ( Configs activeManager newJiraConfig, Cmd.map JiraConfigMsg jiraCmd )



normalizeConfigs : Configs -> Value
normalizeConfigs (Configs _ jiraConfig) =
    Json.Encode.object
        [ ("jira", JiraConfig.normalize jiraConfig)
        ]

decodeConfigs : Decoder Configs
decodeConfigs =
    Json.Decode.map2 Configs
        (Json.Decode.succeed None)
        (Json.Decode.field "jira" JiraConfig.decoder)

initConfigs : Configs
initConfigs =
    Configs None JiraConfig.init

subscriptions : Sub ConfigsMsg
subscriptions =
    showJiraManager (\_ -> ShowConfigManager Jira)

configsSaveAdvised : ConfigsMsg -> Bool
configsSaveAdvised msg =
    case msg of
        JiraConfigMsg jiraMsg -> JiraConfig.stateSaveAdvised jiraMsg
        ShowConfigManager None -> True
        _ -> False

--viewLogRef : LogRef -> Html Msg
--viewLogRef logRef =
--    case logRef of
--        JiraLogRef jiraLogRef -> Integrations.Jira.Log. jiraLogRef
--
--
--viewLogTable : LogTable -> Html Msg
--viewLogTable logTable =
--    case logTable of
--        JiraTab ->


