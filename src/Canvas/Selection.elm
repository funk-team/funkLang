module Canvas.Selection exposing (..)

{-| Allows the user to select targets for modification or inspection
-}

import Json.Decode as Decode
import Json.Encode as Encode
import Spec.Element.Id


toRoot : Selection -> Selection
toRoot selection =
    let
        trim : SelectionItem -> SelectionItem
        trim i =
            case removeOne i of
                Nothing ->
                    i

                Just i_ ->
                    trim i_
    in
    case selection of
        Nothing ->
            Nothing

        Just sel ->
            trim sel
                |> Just


isSelectedElementId : Selection -> Spec.Element.Id.Id -> Bool
isSelectedElementId rootSelection id =
    Maybe.map getTargetId rootSelection == Just id


{-| -}
type TargetSelection
    = TargetScreen Spec.Element.Id.Id
    | TargetElement PathToElement


type alias PathToElement =
    { path : List Spec.Element.Id.Id
    , parent : Spec.Element.Id.Id
    , target : Spec.Element.Id.Id
    }


toTargetSelection selection =
    case selection of
        SelectedRoot screenId ->
            TargetScreen screenId

        SelectedShallow screenId shallowId ->
            TargetElement { path = [], parent = screenId, target = shallowId.id }

        SelectedDeeper screenId { path, parent, target } ->
            TargetElement
                { path = screenId :: List.map .id path
                , parent = parent.id
                , target = target.id
                }


toPath sel =
    case toTargetSelection sel of
        TargetScreen id ->
            [ id ]

        TargetElement { path, parent, target } ->
            path ++ [ parent, target ]


getTargetId : SelectionItem -> Spec.Element.Id.Id
getTargetId rootSelection =
    case rootSelection of
        SelectedRoot screenId ->
            screenId

        SelectedShallow screenId selectedId ->
            selectedId.id

        SelectedDeeper screenId deeperSelection ->
            deeperSelection.target.id


fresh : Spec.Element.Id.Id -> SelectionItem
fresh rootId =
    SelectedRoot rootId


screenIndexToLabel : String -> Int -> String
screenIndexToLabel label index =
    case label of
        "" ->
            "Screen " ++ String.fromInt (index + 1)

        text ->
            text


{-| Turn the internal selection path into a well-typed structure

    import Spec.Element.Id

    id0 : Spec.Element.Id.Id
    id0 = (Spec.Element.Id.Id 0)

    id1 : Spec.Element.Id.Id
    id1 = (Spec.Element.Id.Id 1)

    id2 : Spec.Element.Id.Id
    id2 = (Spec.Element.Id.Id 2)

    id3 : Spec.Element.Id.Id
    id3 = (Spec.Element.Id.Id 3)

    id4 : Spec.Element.Id.Id
    id4 = (Spec.Element.Id.Id 4)

    addOne
        id1
        (SelectedRoot id0)
        --> SelectedShallow id0 {index = Nothing, id = id1}

    addOne
        id2
        (SelectedShallow id0 {index = Nothing, id = id1})
        --> (SelectedDeeper id0 {path = [], target = {id = id2, index = Nothing}, parent = {id = id1, index = Nothing}})

    addOne
        id3
        (SelectedDeeper id0 {path = [], target = {id = id2, index = Nothing}, parent = {id = id1, index = Nothing}})
        --> (SelectedDeeper id0 {path = [{id = id1, index = Nothing}], target = {id = id3, index = Nothing}, parent = {id = id2, index = Nothing}})

    addOne
        id4
        (SelectedDeeper id0 {path = [{id = id1, index = Nothing}], target = {id = id3, index = Nothing}, parent = {id = id2, index = Nothing}})
        --> (SelectedDeeper id0 {path = [{id = id1, index = Nothing}, {id = id2, index = Nothing}], target = {id = id4, index = Nothing}, parent = {id = id3, index = Nothing}})

-}
addOne : Spec.Element.Id.Id -> SelectionItem -> SelectionItem
addOne id selection =
    addOneWithIndex { id = id, index = Nothing } selection


addOneWithIndex : IdWithIndex -> SelectionItem -> SelectionItem
addOneWithIndex idWithIndex selection =
    case selection of
        SelectedRoot rootId ->
            SelectedShallow
                rootId
                idWithIndex

        SelectedShallow rootId shallowId ->
            SelectedDeeper rootId { parent = shallowId, target = idWithIndex, path = [] }

        SelectedDeeper root { path, parent, target } ->
            SelectedDeeper root { parent = target, target = idWithIndex, path = path ++ [ parent ] }


setIndex : Maybe Int -> SelectionItem -> SelectionItem
setIndex index selection =
    case selection of
        SelectedRoot rootId ->
            SelectedRoot rootId

        SelectedShallow rootId shallowId ->
            SelectedShallow rootId { shallowId | index = index }

        SelectedDeeper root ({ target } as deeperSelection) ->
            SelectedDeeper root { deeperSelection | target = { target | index = index } }


{-|

    import Spec.Element.Id

    id0 : Spec.Element.Id.Id
    id0 = (Spec.Element.Id.Id 0)

    id1 : Spec.Element.Id.Id
    id1 = (Spec.Element.Id.Id 1)

    id2 : Spec.Element.Id.Id
    id2 = (Spec.Element.Id.Id 2)

    id3 : Spec.Element.Id.Id
    id3 = (Spec.Element.Id.Id 3)

    id4 : Spec.Element.Id.Id
    id4 = (Spec.Element.Id.Id 4)

    removeOne
        (SelectedRoot id0)
        --> Nothing

    removeOne
        (SelectedShallow id0 {index = Nothing, id = id1})
        --> Just (SelectedRoot id0)

    removeOne
        (SelectedDeeper id0 {path = [{id = id1, index = Nothing}], target = {id = id3, index = Nothing}, parent = {id = id2, index = Nothing}})
        --> Just (SelectedDeeper id0 { parent = {id = id1, index = Nothing}, path = [], target = {id = id2, index = Nothing} })

    removeOne
        (SelectedDeeper id0 {path = [{id = id1, index = Nothing}, {id = id2, index = Nothing}], target = {id = id4, index = Nothing}, parent = {id = id3, index = Nothing}})
        --> Just (SelectedDeeper id0 {path = [{id = id1, index = Nothing}], target = {id = id3, index = Nothing}, parent = {id = id2, index = Nothing}})

-}
removeOne : SelectionItem -> Maybe SelectionItem
removeOne selection =
    case selection of
        SelectedRoot screenId ->
            Nothing

        SelectedShallow screenId _ ->
            Just (SelectedRoot screenId)

        SelectedDeeper root { parent, path, target } ->
            Just <|
                case List.reverse path of
                    [] ->
                        SelectedShallow root parent

                    grandParent :: ancestry ->
                        SelectedDeeper root { parent = grandParent, target = parent, path = List.reverse ancestry }



-- [generator-start]


type alias Selection =
    Maybe SelectionItem


type SelectionItem
    = SelectedRoot Spec.Element.Id.Id
    | SelectedShallow Spec.Element.Id.Id IdWithIndex
    | SelectedDeeper Spec.Element.Id.Id DeeperSelection


type alias IdWithIndex =
    { id : Spec.Element.Id.Id
    , index : Maybe Int
    }


type alias DeeperSelection =
    { path : List IdWithIndex
    , parent : IdWithIndex
    , target : IdWithIndex
    }



{- @@TODO
   imo SelectionItem could have a simpler datastructure:

   type SelectionItem
     { path : List Spec.Element.Id.Id
     , target : Spec.Element.Id.Id
     }

   With an empty path list serving as SelectedRoot.
   Maybe this design was chosen for other reason that I'm not aware of.

   -- STTR13
-}


{-| Which targets in the sidebar should show more detail
-}
type alias ExpansionSet =
    List Spec.Element.Id.Id



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeDeeperSelection =
    Decode.map3
        DeeperSelection
        (Decode.field "path" (Decode.list decodeIdWithIndex))
        (Decode.field "parent" decodeIdWithIndex)
        (Decode.field "target" decodeIdWithIndex)


decodeExpansionSet =
    Decode.list Spec.Element.Id.decodeId


decodeIdWithIndex =
    Decode.map2
        IdWithIndex
        (Decode.field "id" Spec.Element.Id.decodeId)
        (Decode.field "index" (Decode.maybe Decode.int))


decodeSelection =
    Decode.maybe decodeSelectionItem


decodeSelectionItem =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeSelectionItemHelp


decodeSelectionItemHelp constructor =
    case constructor of
        "SelectedRoot" ->
            Decode.map
                SelectedRoot
                (Decode.field "A1" Spec.Element.Id.decodeId)

        "SelectedShallow" ->
            Decode.map2
                SelectedShallow
                (Decode.field "A1" Spec.Element.Id.decodeId)
                (Decode.field "A2" decodeIdWithIndex)

        "SelectedDeeper" ->
            Decode.map2
                SelectedDeeper
                (Decode.field "A1" Spec.Element.Id.decodeId)
                (Decode.field "A2" decodeDeeperSelection)

        other ->
            Decode.fail <| "Unknown constructor for type SelectionItem: " ++ other


encodeDeeperSelection a =
    Encode.object
        [ ( "path", Encode.list encodeIdWithIndex a.path )
        , ( "parent", encodeIdWithIndex a.parent )
        , ( "target", encodeIdWithIndex a.target )
        ]


encodeExpansionSet a =
    Encode.list Spec.Element.Id.encodeId a


encodeIdWithIndex a =
    Encode.object
        [ ( "id", Spec.Element.Id.encodeId a.id )
        , ( "index", encodeMaybeInt a.index )
        ]


encodeMaybeInt a =
    case a of
        Just b ->
            Encode.int b

        Nothing ->
            Encode.null


encodeSelection a =
    case a of
        Just b ->
            encodeSelectionItem b

        Nothing ->
            Encode.null


encodeSelectionItem a =
    case a of
        SelectedRoot a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "SelectedRoot" )
                , ( "A1", Spec.Element.Id.encodeId a1 )
                ]

        SelectedShallow a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "SelectedShallow" )
                , ( "A1", Spec.Element.Id.encodeId a1 )
                , ( "A2", encodeIdWithIndex a2 )
                ]

        SelectedDeeper a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "SelectedDeeper" )
                , ( "A1", Spec.Element.Id.encodeId a1 )
                , ( "A2", encodeDeeperSelection a2 )
                ]



-- [generator-end]
