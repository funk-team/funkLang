module Compass exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode



-- [decgen-start]


type Direction
    = North
    | NorthWest
    | West
    | SouthWest
    | South
    | SouthEast
    | East
    | NorthEast
    | Center



-- [decgen-generated-start] -- DO NOT MODIFY or remove this line


decodeDirection =
    let
        recover x =
            case x of
                "North" ->
                    Decode.succeed North

                "NorthWest" ->
                    Decode.succeed NorthWest

                "West" ->
                    Decode.succeed West

                "SouthWest" ->
                    Decode.succeed SouthWest

                "South" ->
                    Decode.succeed South

                "SouthEast" ->
                    Decode.succeed SouthEast

                "East" ->
                    Decode.succeed East

                "NorthEast" ->
                    Decode.succeed NorthEast

                "Center" ->
                    Decode.succeed Center

                other ->
                    Decode.fail <| "Unknown constructor for type Direction: " ++ other
    in
    Decode.string |> Decode.andThen recover


encodeDirection a =
    case a of
        North ->
            Encode.string "North"

        NorthWest ->
            Encode.string "NorthWest"

        West ->
            Encode.string "West"

        SouthWest ->
            Encode.string "SouthWest"

        South ->
            Encode.string "South"

        SouthEast ->
            Encode.string "SouthEast"

        East ->
            Encode.string "East"

        NorthEast ->
            Encode.string "NorthEast"

        Center ->
            Encode.string "Center"



-- [decgen-end]
