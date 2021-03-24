module FileSystem.Model exposing (..)

import Json.Decode as Decode


type alias Files =
    List ( String, Maybe Decode.Value )
