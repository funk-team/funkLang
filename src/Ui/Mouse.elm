module Ui.Mouse exposing (..)

import Element
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Svg
import Svg.Events as SvgEvents


type alias MouseInfo =
    { x : Int
    , y : Int
    , mousePressed : Bool
    }


positionAttrs :
    { onMouseDown : Maybe (MouseInfo -> msg)
    , onMouseUp : Maybe msg
    , onClick : Maybe (MouseInfo -> msg)
    , onMouseMove : Maybe (MouseInfo -> msg)
    }
    -> List (Element.Attribute msg)
positionAttrs { onMouseDown, onMouseUp, onClick, onMouseMove } =
    let
        onMouseDownSvg : (MouseInfo -> msg) -> Svg.Attribute msg
        onMouseDownSvg msgCreator =
            SvgEvents.on "mousedown" (Decode.map msgCreator decodeMouseInfo)

        onMouseMoveSvg : (MouseInfo -> msg) -> Svg.Attribute msg
        onMouseMoveSvg msgCreator =
            SvgEvents.on "mousemove" (Decode.map msgCreator decodeMouseInfo)

        onClickSvg : (MouseInfo -> msg) -> Svg.Attribute msg
        onClickSvg msgCreator =
            SvgEvents.on "click" (Decode.map msgCreator decodeMouseInfo)
    in
    [ onMouseDown |> Maybe.map onMouseDownSvg
    , onMouseUp |> Maybe.map SvgEvents.onMouseUp
    , onClick |> Maybe.map onClickSvg
    , onMouseMove |> Maybe.map onMouseMoveSvg
    ]
        |> List.filterMap (\a -> a)
        |> List.map Element.htmlAttribute


pointCursor =
    Html.Attributes.style "cursor" "pointer"
        |> Element.htmlAttribute


grabbingCursor =
    Html.Attributes.style "cursor" "grab"
        |> Element.htmlAttribute


addElementCursor =
    Html.Attributes.style "cursor" "copy"
        |> Element.htmlAttribute


decodeMouseInfo : Decoder MouseInfo
decodeMouseInfo =
    Decode.map3 MouseInfo
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)
        (Decode.field "buttons" Decode.int |> Decode.map ((/=) 0))
