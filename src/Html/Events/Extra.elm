module Html.Events.Extra exposing (onColorSave)

import Color.Extra
import Html
import Html.Events
import Json.Decode



{- For the funk-pickr -}


onColorSave : Html.Attribute Color.Extra.ColorRgba
onColorSave =
    Html.Events.custom "change"
        (Json.Decode.field "detail" Color.Extra.decodeColorRgba255 |> Json.Decode.map (Color.Extra.fromColorRgba255 >> preventAll))


preventAll msg =
    { preventDefault = True, stopPropagation = True, message = msg }
