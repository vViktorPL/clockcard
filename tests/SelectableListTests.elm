module SelectableListTests exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Json.Encode
import Json.Decode exposing (decodeString)
import SelectableList

suite : Test
suite =
    describe "SelectableList module"
        [ describe "SelectableList.normalize"
            [ test "normalizes SelectableList with selected item index stored" <|
                \_ ->
                    SelectableList.fromList [1,2,3,4,5]
                        |> Maybe.andThen (SelectableList.select 3)
                        |> Maybe.map
                            (\selectableList -> selectableList
                                |> SelectableList.normalize Json.Encode.int
                                |> Json.Encode.encode 0
                                |> Expect.equal """{"selectedIndex":2,"items":[1,2,3,4,5]}"""
                            )
                        |> Maybe.withDefault (fail "Invalid SelectableList")
            ]
        , describe "SelectableList.decoder"
            [ test "decodes normalized SelectableList" <|
                \_ ->
                    SelectableList.fromList [1,2,3,4,5]
                        |> Maybe.andThen (SelectableList.select 3)
                        |> Maybe.map
                            (\selectableList ->
                                decodeString (SelectableList.decoder Json.Decode.int) """{"selectedIndex":2,"items":[1,2,3,4,5]}"""
                                    |> Expect.equal (Ok selectableList)
                            )
                        |> Maybe.withDefault (fail "Invalid SelectableList")
            ]
        ]
