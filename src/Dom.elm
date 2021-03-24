module Dom exposing (..)

import BoundingClientRectangle
import Canvas.Events
import Json.Decode as Decode
import Spec.Element.Id


type alias Dom =
    { getElementById : Decode.Value }


getBoundingClientRect : Dom -> Spec.Element.Id.Id -> Maybe Canvas.Events.SceneRectangle
getBoundingClientRect { getElementById } id =
    let
        htmlId =
            Spec.Element.Id.toHtmlIdRaw id

        decoder : Decode.Decoder Canvas.Events.SceneRectangle
        decoder =
            Decode.at [ htmlId ] BoundingClientRectangle.decodeAsRectangle
                |> Decode.map Canvas.Events.SceneRectangle
    in
    Decode.decodeValue decoder getElementById
        |> Result.toMaybe
