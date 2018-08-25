module Issue exposing (Model, IssueId)

import Stopwatch

type alias IssueId = Int
type alias Model =
    { id: IssueId
    , name: String
    , stopwatch: Stopwatch.Model
    }
