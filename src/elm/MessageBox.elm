port module MessageBox exposing (showErrorBox)

import Json.Encode as E exposing (Value)

port showElectronErrorBox : Value -> Cmd msg

showErrorBox : String -> String -> Cmd msg
showErrorBox title content =
    E.object
        [ ("title", E.string title)
        , ("content", E.string content)
        ]
        |> showElectronErrorBox