module IntDict.Typed exposing (..)

{-| Provide a dictionary with integer keys

Keys are typed using a phantom so that we do not confused keys and values of one dictionary with that of another

-}

import Dict
import IntDict
import Json.Decode as Decode
import Json.Encode as Encode


size (Typed dict) =
    Dict.size dict


isEmpty (Typed dict) =
    Dict.isEmpty dict


type Typed key val
    = Typed (IntDict.Dict val)


empty : Typed key val
empty =
    Typed Dict.empty


nextId : (Int -> key) -> Typed key val -> key
nextId wrap (Typed dict) =
    IntDict.nextId dict
        |> wrap


get : (key -> Int) -> key -> Typed key val -> Maybe val
get unwrap key (Typed dict) =
    Dict.get (unwrap key) dict


update : (key -> Int) -> key -> (Maybe v -> Maybe v) -> Typed key v -> Typed key v
update unwrap key fn (Typed dict) =
    Dict.update (unwrap key) fn dict
        |> Typed


map : (Int -> key) -> (key -> v -> v) -> Typed key v -> Typed key v
map wrap fn (Typed dict) =
    Dict.map (\key -> fn (wrap key)) dict
        |> Typed


remove : (key -> Int) -> key -> Typed key val -> Typed key val
remove unwrap key (Typed dict) =
    Dict.remove (unwrap key) dict
        |> Typed


insert : (key -> Int) -> key -> val -> Typed key val -> Typed key val
insert unwrap key val (Typed dict) =
    Dict.insert (unwrap key) val dict
        |> Typed


toList : (Int -> key) -> Typed key val -> List ( key, val )
toList wrap (Typed dict) =
    Dict.toList dict
        |> List.map (Tuple.mapFirst wrap)


decodeTyped : Decode.Decoder val -> Decode.Decoder (Typed tag val)
decodeTyped decodeVal =
    let
        decodeDictTuple =
            Decode.map2
                (\a1 a2 -> ( a1, a2 ))
                (Decode.field "A1" Decode.int)
                (Decode.field "A2" decodeVal)
    in
    Decode.map Dict.fromList (Decode.list decodeDictTuple)
        |> Decode.map Typed


encodeTyped : (val -> Encode.Value) -> Typed tag val -> Encode.Value
encodeTyped encodeVal (Typed a) =
    let
        encodeDictTuple ( a1, a2 ) =
            Encode.object
                [ ( "A1", Encode.int a1 )
                , ( "A2", encodeVal a2 )
                ]
    in
    Encode.list encodeDictTuple (Dict.toList a)
