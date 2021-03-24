module Spec.Element exposing (..)

{-| Model and transform the elements that live on a canvas, including their layout constraints
-}

import Canvas.Events
import Rectangle
import Spec.Element.Id
import Spec.Element.Layout
import Spec.Element.Layout.Length
import Spec.Element.Model exposing (..)
import Viewport


type alias RectWithId =
    { id : Spec.Element.Id.Id
    , rect : Rectangle.Rectangle
    }


mapShared : (Shared -> Shared) -> Element a -> Element a
mapShared fn el =
    { el | shared = fn el.shared }


getChildren : Element a -> List EitherElement
getChildren element =
    case element.shared.children of
        AbsoluteChildren abs ->
            List.map
                wrapAbsolute
                abs

        FlowChildren inFlow ->
            List.map
                wrapFlow
                inFlow


setChildren : Children -> Element a -> Element a
setChildren children element =
    mapShared
        (\shared -> { shared | children = children })
        element


isResponsified : Element a -> Bool
isResponsified element =
    case element.shared.children of
        AbsoluteChildren abs ->
            False

        FlowChildren inFlow ->
            True


hasChildren : Element a -> Bool
hasChildren element =
    case element.shared.children of
        AbsoluteChildren [] ->
            False

        FlowChildren [] ->
            False

        AbsoluteChildren _ ->
            True

        FlowChildren _ ->
            True


createAbsoluteElementDimensions : Canvas.Events.ElementRectangle -> AbsoluteElementDimensions
createAbsoluteElementDimensions rect =
    let
        (Canvas.Events.ElementRectangle rect_) =
            rect
    in
    { width = Spec.Element.Layout.Length.init <| Rectangle.width rect_
    , height = Spec.Element.Layout.Length.init <| Rectangle.height rect_
    , x = Spec.Element.Layout.Length.init <| Rectangle.x1 rect_
    , y = Spec.Element.Layout.Length.init <| Rectangle.y1 rect_
    }


rectangleToScreenSize : Rectangle.Rectangle -> ScreenSize
rectangleToScreenSize rect =
    case
        rect
            |> Viewport.correspondingPreset
            |> Maybe.map Viewport.presetToDevice
    of
        Nothing ->
            Custom (Canvas.Events.AbsoluteRectangle rect)

        Just device ->
            Preset device { x = Rectangle.x1 rect, y = Rectangle.y1 rect }


screenSizeToRectangle : ScreenSize -> Rectangle.Rectangle
screenSizeToRectangle outerGeometry =
    let
        presetToRect device { x, y } =
            { x1 = x
            , y1 = y
            , x2 = x + toFloat device.width
            , y2 = y + toFloat device.height
            }
                |> Rectangle.Rectangle
    in
    case outerGeometry of
        Preset device offset ->
            presetToRect device offset

        Custom (Canvas.Events.AbsoluteRectangle rect) ->
            rect


wrapAbsolute : Element AbsoluteElementDimensions -> EitherElement
wrapAbsolute { shared, outerGeometry } =
    { shared = shared
    , outerGeometry = AbsoluteElementGeometry outerGeometry
    }


wrapFlow : Element { size : Spec.Element.Layout.Size, alignment : Spec.Element.Layout.Alignment } -> EitherElement
wrapFlow { shared, outerGeometry } =
    { shared = shared, outerGeometry = FlowElementGeometry outerGeometry }


{-| @@TODO: make consistent: This one is spelled out to make it more comprehensive
-}
wrapScreen : Element ScreenSize -> EitherElement
wrapScreen { shared, outerGeometry } =
    { shared = shared, outerGeometry = ScreenGeometry outerGeometry }


getLabel : Element a -> Maybe String
getLabel { shared } =
    case String.trim shared.label of
        "" ->
            Nothing

        something ->
            Just something


getLabelOrId : Element a -> String
getLabelOrId { shared } =
    case shared.label of
        "" ->
            Spec.Element.Id.toString shared.id

        _ ->
            shared.label
