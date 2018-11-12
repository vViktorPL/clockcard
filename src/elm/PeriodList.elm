module PeriodList exposing (Model, Period)

import Time exposing (Posix)
import Html exposing (Html)
import Integrations exposing (LogRef, viewLogRef)

type Msg
    = IntegrationMsg Integrations.Msg

type Period = Period Posix Posix (List LogRef)

type Tab = PeriodsTab | IntegrationTab Integrations.Tab

type alias Model =
    { currentTab: Tab
    , finishedPeriods: List Period
    , periodInProgress: Maybe Posix
    }

view : Model -> Html Msg
view model =
    Html.div []
        [ case model.currentTab of
            PeriodsTab -> viewPeriodsTab model
            IntegrationTab integrationTab ->
        ]


viewPeriodsTab : Model -> Html Msg