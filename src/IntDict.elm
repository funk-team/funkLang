module IntDict exposing (Dict, decodeDict, empty, encodeDict, insertNew, isEmpty, makeKey, nextId, size)

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Random


insertNew =
    addNew


empty =
    Dict.empty


size =
    Dict.size


isEmpty =
    Dict.empty



-- [generator-start]


type alias Dict val =
    Dict.Dict Int val



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeDict decodeVal =
    let
        decodeDictTuple =
            Decode.map2
                (\a1 a2 -> ( a1, a2 ))
                (Decode.field "A1" Decode.int)
                (Decode.field "A2" decodeVal)
    in
    Decode.map Dict.fromList (Decode.list decodeDictTuple)


encodeDict encodeVal a =
    let
        encodeDictTuple ( a1, a2 ) =
            Encode.object
                [ ( "A1", Encode.int a1 )
                , ( "A2", encodeVal a2 )
                ]
    in
    Encode.list encodeDictTuple (Dict.toList a)



-- [generator-end]


addNew : a -> Dict.Dict Int a -> Dict.Dict Int a
addNew el dict =
    Dict.insert (nextId dict) el dict


nextId : Dict.Dict Int a -> Int
nextId =
    Dict.toList
        >> List.map Tuple.first
        >> List.maximum
        >> Maybe.withDefault -1
        >> (+) 1


makeKey =
    Random.step (Random.int 0 1000)
