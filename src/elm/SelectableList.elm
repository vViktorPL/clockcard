module SelectableList exposing
    (SelectableList(..)
    , fromList
    , isSelected
    , items
    , member
    , select
    , selected
    , mapSelected
    , prepend
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

{-| An ordered list with a selected item.
-}
type SelectableList a =
    SelectableList
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
            Just
                <| SelectableList
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
items : SelectableList a -> List a
items (SelectableList { head, selected, tail }) =
    head ++ [ selected ] ++ tail

{-| Tests if a value is contained in the list.

    list = selectableList [1,2,3]

    member 1 list == True
    member 4 list == False

-}
member : a -> SelectableList a -> Bool
member item list =
    List.member item <| items list

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
                (head, selected, tail) = List.foldl
                    (\current (head, selected, tail) ->
                        case selected of
                            Nothing ->
                                if current /= item then
                                    (head ++ [ current ], selected, tail)
                                else
                                    (head, Just current, tail)

                            Just _ ->
                                (head, selected, tail ++ [ current ])
                    )
                    ([], Nothing, [])
                    <| items list
            in
                case selected of
                    Nothing ->
                        Nothing

                    Just selected ->
                        Just
                            <|SelectableList
                                { head = head
                                , selected = selected
                                , tail = tail
                                }

{-| Returns the currently selected item.

    list = selectableList [1,2,3]

    selected list == 1

-}
selected : SelectableList a -> a
selected (SelectableList { head, selected,
 tail}) =
    selected

{-| Maps the currently selected item. -}
mapSelected : (a -> a) -> SelectableList a -> SelectableList a
mapSelected f (SelectableList { head, selected, tail }) = SelectableList
    { head = head
    , selected = f selected
    , tail = tail
    }

{-| Appends item to the beginning of the list. |-}
prepend : SelectableList a -> a -> SelectableList a
prepend (SelectableList listInternal) item =
    SelectableList { listInternal | head = [ item ] ++ listInternal.head }