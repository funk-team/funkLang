module BoundingClientRectangle exposing (decodeAsRectangle)

{-| Modelling the bounding client rect
-}

import Json.Decode as Decode
import Rectangle


type alias BoundingClientRectangle =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


decodeAsRectangle =
    Decode.map toRectangle decodeBoundingClientRectangle


{-| Decode the bounding client rect from a DOM event

> Requires monkey-patching a 'boundingClientRect' getter on the HTMLElement prototype

-}
decodeBoundingClientRectangle : Decode.Decoder BoundingClientRectangle
decodeBoundingClientRectangle =
    Decode.field "boundingClientRect"
        (Decode.map4 BoundingClientRectangle
            (Decode.field "x" Decode.float)
            (Decode.field "y" Decode.float)
            (Decode.field "width" Decode.float)
            (Decode.field "height" Decode.float)
        )


toRectangle : BoundingClientRectangle -> Rectangle.Rectangle
toRectangle { x, y, width, height } =
    Rectangle.fromPoints
        { x1 = x
        , y1 = y
        , x2 = x + width
        , y2 = y + height
        }
