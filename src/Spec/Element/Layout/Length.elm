module Spec.Element.Layout.Length exposing (..)

import Interface.Model
import Json.Decode as Decode
import Json.Encode as Encode


init : Float -> Length
init last =
    { current = UserInput (last |> round |> String.fromInt)
    , last = last
    }


initNullable : Float -> NullableLength
initNullable last =
    { current = UserInput (last |> round |> String.fromInt)
    , last = Just last
    }


null : NullableLength
null =
    { last = Nothing
    , current = UserInput ""
    }


toNullable : Length -> NullableLength
toNullable { last, current } =
    { last = Just last, current = current }


fromNullable : NullableLength -> Length
fromNullable { last, current } =
    { last = last |> Maybe.withDefault 100
    , current = current
    }



-- [generator-start]


{-| In case the input breaks we always want a fallback value
-}
type alias Length =
    { last : Float
    , current : LengthInput
    }


type alias NullableLength =
    { last : Maybe Float
    , current : LengthInput
    }


type LengthInput
    = UserInput String
    | Linked Interface.Model.InterfacePointer



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeLength =
    Decode.map2
        Length
        (Decode.field "last" Decode.float)
        (Decode.field "current" decodeLengthInput)


decodeLengthInput =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeLengthInputHelp


decodeLengthInputHelp constructor =
    case constructor of
        "UserInput" ->
            Decode.map
                UserInput
                (Decode.field "A1" Decode.string)

        "Linked" ->
            Decode.map
                Linked
                (Decode.field "A1" Interface.Model.decodeInterfacePointer)

        other ->
            Decode.fail <| "Unknown constructor for type LengthInput: " ++ other


decodeNullableLength =
    Decode.map2
        NullableLength
        (Decode.field "last" (Decode.maybe Decode.float))
        (Decode.field "current" decodeLengthInput)


encodeLength a =
    Encode.object
        [ ( "last", Encode.float a.last )
        , ( "current", encodeLengthInput a.current )
        ]


encodeLengthInput a =
    case a of
        UserInput a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "UserInput" )
                , ( "A1", Encode.string a1 )
                ]

        Linked a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Linked" )
                , ( "A1", Interface.Model.encodeInterfacePointer a1 )
                ]


encodeMaybeFloat a =
    case a of
        Just b ->
            Encode.float b

        Nothing ->
            Encode.null


encodeNullableLength a =
    Encode.object
        [ ( "last", encodeMaybeFloat a.last )
        , ( "current", encodeLengthInput a.current )
        ]



-- [generator-end]
