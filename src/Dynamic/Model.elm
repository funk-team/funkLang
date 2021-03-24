module Dynamic.Model exposing (decode, encode, extractValues, init)

import Dict exposing (Dict)
import Dynamic.Data
import Dynamic.Data.Coders
import Dynamic.Data.Types
import Dynamic.Types exposing (Model)
import Json.Decode as D
import Json.Encode as E


extractValues : Model -> Dynamic.Data.Types.Instance
extractValues model =
    Dynamic.Data.Types.ComplexInstance
        (Dict.map (always .instance) model)


init : Model
init =
    Dict.empty


encodeField : Dynamic.Data.Types.NamedInstance -> E.Value
encodeField field =
    E.object
        [ ( "name", E.string field.name )
        , ( "value", Dynamic.Data.Coders.encodeInstance field.instance )
        ]


encode : Model -> E.Value
encode model =
    E.dict String.fromInt encodeField model


decode : D.Decoder Model
decode =
    D.dict decodeField
        |> D.map toIntDict


decodeField : D.Decoder Dynamic.Data.Types.NamedInstance
decodeField =
    D.map2 Dynamic.Data.Types.NamedInstance
        (D.field "name" D.string)
        (D.field "value" Dynamic.Data.Coders.decodeInstance)


toIntDict : Dict String a -> Dict Int a
toIntDict =
    Dict.toList
        >> List.map (Tuple.mapFirst String.toInt)
        >> List.filterMap
            (\( key, val ) ->
                case key of
                    Just num ->
                        Just ( num, val )

                    Nothing ->
                        Nothing
            )
        >> Dict.fromList
