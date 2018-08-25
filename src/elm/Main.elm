import Time exposing (Time)
import Html exposing (Html, div)
import Stopwatch
import IssueList exposing (..)

type Msg = StopwatchMsg Stopwatch.Msg | IssueListMsg IssueList.Msg

type alias Model =
    { issues: IssueList.Model
    }

type alias IssueId = Int
type alias Issue =
    { id: IssueId
    , name: String
    , stopwatch: Stopwatch.Model
    }

getCurrentStopwatch : Model -> Stopwatch.Model
getCurrentStopwatch model =
    model.issues
        |> IssueList.getSelectedIssue
        |> .stopwatch

view : Model -> Html Msg
view model =
    div
        []
        [ Html.map IssueListMsg (IssueList.view model.issues)
        , Html.map StopwatchMsg (Stopwatch.view (getCurrentStopwatch model))
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        IssueListMsg msg ->
            let
                (updatedIssues, cmd) = IssueList.update msg model.issues
                wrappedCmd = Cmd.map IssueListMsg cmd
            in
                ({ model | issues = updatedIssues }
                , case msg of
                    IssueList.SelectIssue _ -> Cmd.batch [ wrappedCmd, Cmd.map StopwatchMsg Stopwatch.refresh ]
                    _ -> wrappedCmd
                )

        StopwatchMsg msg ->
            let
                currentStopwatch = model.issues
                selectedIssue = IssueList.getSelectedIssue model.issues
                (updatedStopwatch, cmd) = Stopwatch.update msg (getCurrentStopwatch model)
            in
                ({ model | issues = updateSelectedIssue model.issues ({ selectedIssue | stopwatch = updatedStopwatch }) }
                , Cmd.map StopwatchMsg cmd
                )

init =
    ({ issues = IssueList.init}
    , Cmd.none
    )

main = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \model -> (Sub.map StopwatchMsg (Stopwatch.subscriptions (getCurrentStopwatch model)))
    }
