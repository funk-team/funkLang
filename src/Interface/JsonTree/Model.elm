module Interface.JsonTree.Model exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Set.Any as Set exposing (AnySet)


{-| The state of the JSON tree view. Note that this is just the runtime state needed to
implement things like expand/collapse--it is _not_ the tree data itself.

You should store the current state in your model.

-}
type State
    = State (Set String KeyPath)


{-| An update might need to traverse into a list and then the subselection of that list.
This specific QualifiedKeyPath helps with this.
It's a List of KeyPaths
-}
type alias KeyPathList =
    List KeyPath



-- TODO - remove?


type Selection
    = Value Node
    | List KeyPath


type alias Set key val =
    AnySet key val


{-| A node in the tree
-}
type alias Node =
    { value : TaggedValue
    , keyPath : KeyPath
    }


{-| A tagged value
-}
type TaggedValue
    = TString String
    | TFloat Float
    | TBool Bool
    | TList (List Node)
    | TDict (Dict String Node)
    | TNull



-- [decgen-start]


{-| The path to a piece of data in the tree.
-}
type alias KeyPath =
    List KeyPathComponent


type KeyPathComponent
    = ObjectAccessor String
    | IndexAccessor Int


encodeTree : TaggedValue -> Encode.Value
encodeTree val =
    case val of
        TString string ->
            Encode.string string

        TFloat float ->
            Encode.float float

        TBool bool ->
            Encode.bool bool

        TList list ->
            list
                |> List.map .value
                |> Encode.list encodeTree

        TDict dict ->
            dict
                |> Dict.toList
                |> List.map (Tuple.mapSecond (.value >> encodeTree))
                |> Encode.object

        TNull ->
            Encode.null


keyPathToString : KeyPath -> String
keyPathToString =
    List.map keyPathComponentToString
        >> String.join ""


keyPathComponentToString : KeyPathComponent -> String
keyPathComponentToString a =
    case a of
        ObjectAccessor o ->
            "." ++ o

        IndexAccessor i ->
            "[" ++ String.fromInt i ++ "]"



-- [decgen-generated-start] -- DO NOT MODIFY or remove this line


decodeKeyPath =
    Decode.list decodeKeyPathComponent


decodeKeyPathComponent =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeKeyPathComponentHelp


decodeKeyPathComponentHelp constructor =
    case constructor of
        "ObjectAccessor" ->
            Decode.map
                ObjectAccessor
                (Decode.field "A1" Decode.string)

        "IndexAccessor" ->
            Decode.map
                IndexAccessor
                (Decode.field "A1" Decode.int)

        other ->
            Decode.fail <| "Unknown constructor for type KeyPathComponent: " ++ other


encodeKeyPath a =
    Encode.list encodeKeyPathComponent a


encodeKeyPathComponent a =
    case a of
        ObjectAccessor a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "ObjectAccessor" )
                , ( "A1", Encode.string a1 )
                ]

        IndexAccessor a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "IndexAccessor" )
                , ( "A1", Encode.int a1 )
                ]



-- [decgen-end]
