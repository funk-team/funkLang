module Interface.Model exposing (..)

{-| The problem:

  - data sources are fragmented
  - we want to expose users to as little low-level JSON as possible

The observation:
UI, FFI, API all include 'interface'.
Maybe we can use this to generalize how components connect to each other and provide powerful tools to work with them.

The solution:
add a layer on top of JSON that 1. identifies relevant data 2. gives data meaning
Unsolved problems

  - reuse & chaining.

-}

import Dict.Any
import Interface.JsonTree.Model
import Interface.Selection
import Json.Decode as Decode
import Json.Encode as Encode


{-| When an element is bound to a list, the value of an element in the list is exposed to the children

this data structure describes a list

-}
type alias ListScopePointer =
    { name : String
    , interfaceId : InterfaceKey -- the interface from which to ge tthe list
    , pointerToListInInterface :
        PointerFromRoot

    -- where the list lives in the interface - this can theoretically also be a pointer in a list for n-dimensional lists. For now it's not
    , selectionForEachItemInListScope : Interface.Selection.Selection -- where the
    }


{-| When we iterate over a list, the thing that processes each list item has a scope - this scope holds the current item of the list
-- TODO: clean up
-}
type alias ScopeData =
    Dict.Any.AnyDict String ScopeId LocalScopeData


{-| The data visible to one instance of an element mapped over a list
-}
type alias LocalScopeData =
    { data : Decode.Value
    , selectionForEachItemInListScope : Interface.Selection.Selection
    }


type alias JsonValue =
    Decode.Value


encodeJsonValue =
    identity


decodeJsonValue =
    Decode.value


{-| The details shared between different interfaces
-}
type alias GenericInterface =
    { data : Maybe Decode.Value
    , id : InterfaceKey
    , outputSelection : Interface.Selection.Selection
    , name : String
    }



-- [generator-start]


type alias ScopeId =
    ( InterfaceKey, Interface.JsonTree.Model.KeyPath )


{-| If anything needs to refer to an interface it uses the interface pointer

It points to an interface and the data selected in that interface

-}
type alias InterfacePointer =
    { interfaceId : InterfaceKey
    , kind : Maybe InterfaceSelectionPointer
    }


{-| The different targetable interfaces as of now
-}
type InterfaceKey
    = ApiCallKey Int
    | TransformationKey Int


{-| Point to data exposed in an interface.

We can either point to data from the root of an interface or select from the scope of a list

-}
type
    InterfaceSelectionPointer
    -- PointerToJsonData
    = RelativeToInterfaceRoot PointerFromRoot
    | FromListScope PointerInScope


{-| Mark up where in the JSON I can find some data
-}
type alias PointerFromRoot =
    { rootPath : Interface.JsonTree.Model.KeyPath
    }


{-| Mark up inside which already selected list and where inside this selected list I can find specific data.
scopeId is used to identify the list.
subPath is used to find the piece of data inside each list item

given this data
{a : [{b: 1}, {b: 2}]}
and a parent selection of [ObjectAccessor 'a']

I can point to the values 1 and 2 using
{ scopeId : [ObjectAccessor 'a']
, subPath : [ObjectAccessor 'b']
}

THe scope path is really only supposed to be used as an identifier to retrieve data from an existing scope.

That means the renderer uses the root selection to populate a scope in case it is a list.

If [ObjectAccessor 'a'] on a given JSON results in a List the scope resolves to

    Dict [ ( [ ObjectAccessor 'a' ], JsonValue [ { b = 1 }, { b = 2 } ] ) ]

Now we can use the scopeId from the PointerInScope which is `[ObjectAccessor 'a']` to get the `JsonValue [{b: 1}, {b: 2}]`.

-}
type alias PointerInScope =
    -- PointerInScope
    { scopeId : ScopeId
    , subPath : Interface.JsonTree.Model.KeyPath
    }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeInterfaceKey =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeInterfaceKeyHelp


decodeInterfaceKeyHelp constructor =
    case constructor of
        "ApiCallKey" ->
            Decode.map
                ApiCallKey
                (Decode.field "A1" Decode.int)

        "TransformationKey" ->
            Decode.map
                TransformationKey
                (Decode.field "A1" Decode.int)

        other ->
            Decode.fail <| "Unknown constructor for type InterfaceKey: " ++ other


decodeInterfacePointer =
    Decode.map2
        InterfacePointer
        (Decode.field "interfaceId" decodeInterfaceKey)
        (Decode.field "kind" (Decode.maybe decodeInterfaceSelectionPointer))


decodeInterfaceSelectionPointer =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeInterfaceSelectionPointerHelp


decodeInterfaceSelectionPointerHelp constructor =
    case constructor of
        "RelativeToInterfaceRoot" ->
            Decode.map
                RelativeToInterfaceRoot
                (Decode.field "A1" decodePointerFromRoot)

        "FromListScope" ->
            Decode.map
                FromListScope
                (Decode.field "A1" decodePointerInScope)

        other ->
            Decode.fail <| "Unknown constructor for type InterfaceSelectionPointer: " ++ other


decodePointerFromRoot =
    Decode.map
        PointerFromRoot
        (Decode.field "rootPath" Interface.JsonTree.Model.decodeKeyPath)


decodePointerInScope =
    Decode.map2
        PointerInScope
        (Decode.field "scopeId" decodeScopeId)
        (Decode.field "subPath" Interface.JsonTree.Model.decodeKeyPath)


decodeScopeId =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" decodeInterfaceKey)
        (Decode.field "A2" Interface.JsonTree.Model.decodeKeyPath)


encodeInterfaceKey a =
    case a of
        ApiCallKey a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "ApiCallKey" )
                , ( "A1", Encode.int a1 )
                ]

        TransformationKey a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "TransformationKey" )
                , ( "A1", Encode.int a1 )
                ]


encodeInterfacePointer a =
    Encode.object
        [ ( "interfaceId", encodeInterfaceKey a.interfaceId )
        , ( "kind", encodeMaybeInterfaceSelectionPointer a.kind )
        ]


encodeInterfaceSelectionPointer a =
    case a of
        RelativeToInterfaceRoot a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "RelativeToInterfaceRoot" )
                , ( "A1", encodePointerFromRoot a1 )
                ]

        FromListScope a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "FromListScope" )
                , ( "A1", encodePointerInScope a1 )
                ]


encodeMaybeInterfaceSelectionPointer a =
    case a of
        Just b ->
            encodeInterfaceSelectionPointer b

        Nothing ->
            Encode.null


encodePointerFromRoot a =
    Encode.object
        [ ( "rootPath", Interface.JsonTree.Model.encodeKeyPath a.rootPath )
        ]


encodePointerInScope a =
    Encode.object
        [ ( "scopeId", encodeScopeId a.scopeId )
        , ( "subPath", Interface.JsonTree.Model.encodeKeyPath a.subPath )
        ]


encodeScopeId ( a1, a2 ) =
    Encode.object
        [ ( "A1", encodeInterfaceKey a1 )
        , ( "A2", Interface.JsonTree.Model.encodeKeyPath a2 )
        ]



-- [generator-end]
