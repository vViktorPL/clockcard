module SelectableList exposing
    ( SelectableList(..)
    , fromList, select
    , isSelected, member
    , decoder, getItems, getSelected, map, mapSelected, normalize, prepend
    )

{-| Provides `SelectableList`, a list with a selected item. Because the list
maintains a selected item at all times, it needs to consist of at least one item.


# Types

@docs SelectableList


# Modification

@docs fromList, select


# Membership

@docs isSelected, items, member, selected

-}

import Array
import Json.Decode exposing (Decoder, andThen, array, field, int)
import Json.Encode exposing (Value, object)


{-| An ordered list with a selected item.
-}
type SelectableList a
    = SelectableList
        { head : List a
        , selected : a
        , tail : List a
        }


{-| Tries to create a `SelectableList` from a `List`.
Will fail if the source list is empty.
-}
fromList : List a -> Maybe (SelectableList a)
fromList list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            Just <|
                SelectableList
                    { head = []
                    , selected = head
                    , tail = tail
                    }


{-| Tests if a value is the currently selected item.

    list = selectableList [1,2,3]

    isSelected 1 list == True

-}
isSelected : a -> SelectableList a -> Bool
isSelected item (SelectableList { selected }) =
    item == selected


{-| Returns all items contained in the list.

    list = selectableList [1,2,3]

    items list == [1,2,3]

-}
getItems : SelectableList a -> List a
getItems (SelectableList { head, selected, tail }) =
    head ++ [ selected ] ++ tail


{-| Tests if a value is contained in the list.

    list = selectableList [1,2,3]

    member 1 list == True
    member 4 list == False

-}
member : a -> SelectableList a -> Bool
member item list =
    List.member item <| getItems list


{-| Sets an item to be the currently selected item.
Will fail if the list does not contain the item.
-}
select : a -> SelectableList a -> Maybe (SelectableList a)
select item list =
    case member item list of
        False ->
            Nothing

        True ->
            let
                ( newHead, newSelected, newTail ) =
                    List.foldl
                        (\current ( head, selected, tail ) ->
                            case selected of
                                Nothing ->
                                    if current /= item then
                                        ( head ++ [ current ], selected, tail )

                                    else
                                        ( head, Just current, tail )

                                Just _ ->
                                    ( head, selected, tail ++ [ current ] )
                        )
                        ( [], Nothing, [] )
                    <|
                        getItems list
            in
            case newSelected of
                Nothing ->
                    Nothing

                Just selected ->
                    Just <|
                        SelectableList
                            { head = newHead
                            , selected = selected
                            , tail = newTail
                            }


{-| Returns the currently selected item.

    list = selectableList [1,2,3]

    getSelected list == 1

-}
getSelected : SelectableList a -> a
getSelected (SelectableList { head, selected, tail }) =
    selected


{-| Maps the currently selected item.
-}
mapSelected : (a -> a) -> SelectableList a -> SelectableList a
mapSelected f (SelectableList { head, selected, tail }) =
    SelectableList
        { head = head
        , selected = f selected
        , tail = tail
        }


{-| Appends item to the beginning of the list. |
-}
prepend : SelectableList a -> a -> SelectableList a
prepend (SelectableList listInternal) item =
    SelectableList { listInternal | head = [ item ] ++ listInternal.head }


map : (a -> b) -> SelectableList a -> SelectableList b
map fn (SelectableList listInternal) =
    SelectableList
        { head = List.map fn listInternal.head
        , selected = fn listInternal.selected
        , tail = List.map fn listInternal.tail
        }


{-| Normalizes SelectableList so it can be encoded to JSON without any obstacles.
As first argument, there is item normalization mapping function expected so every item can be customly normalized.

    list = selectableList [1,2,3]

    encode 0 (normalize Json.Encode.int list) == "{\"selectedIndex\":0,\"items\":[1,2,3]}"

|

-}
normalize : (a -> Value) -> SelectableList a -> Value
normalize mapFn list =
    case list of
        SelectableList internalList ->
            object
                [ ( "selectedIndex", List.length internalList.head |> Json.Encode.int )
                , ( "items", list |> getItems |> Json.Encode.list mapFn )
                ]


decoder : Decoder a -> Decoder (SelectableList a)
decoder itemDecoder =
    Json.Decode.map2
        (\selectedIndex items ->
            let
                head =
                    Array.slice 0 selectedIndex items

                selected =
                    Array.get selectedIndex items

                tail =
                    Array.slice (selectedIndex + 1) (Array.length items) items
            in
            ( head, selected, tail )
        )
        (field "selectedIndex" int)
        (field "items" (array itemDecoder))
        |> andThen
            (\( head, maybeSelected, tail ) ->
                case maybeSelected of
                    Just selected ->
                        Json.Decode.succeed
                            (SelectableList
                                { head = Array.toList head
                                , selected = selected
                                , tail = Array.toList tail
                                }
                            )

                    Nothing ->
                        Json.Decode.fail "selectedIndex is invalid"
            )
