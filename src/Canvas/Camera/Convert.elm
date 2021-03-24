module Canvas.Camera.Convert exposing (..)

{-| Complexities are

  - boundingClientRectangle as provided by the browser platform is relative to the user's current viewport.
  - we need conversions between different contexts: Relative to viewport, parent element or origin of canvas.

-}

import Canvas.Camera.Model
import Canvas.Events exposing (..)
import Rectangle


{-| Transform points from different contexts
-}
elementPointFromAbsolutePoint (AbsoluteRectangle r) (AbsolutePoint p2) =
    p2
        |> Rectangle.movePoint { x = Rectangle.x1 r, y = Rectangle.y1 r }
        |> ElementPoint


elementRectangleFromElementPoints (ElementPoint p1) (ElementPoint p2) =
    Rectangle.fromMovement p1 p2
        |> ElementRectangle


sceneRectangleFromScenePoints (ScenePoint p1) (ScenePoint p2) =
    Rectangle.fromMovement p1 p2
        |> SceneRectangle


{-| Convert a point on the scene to an absolute point
CORRECT / integrated with debugger
-}
scenePointToAbsolute :
    Canvas.Camera.Model.Model
    -> Rectangle.Point
    -> Rectangle.Point
scenePointToAbsolute camera { x, y } =
    { x = (x - camera.x) / camera.zoom
    , y = (y - camera.y) / camera.zoom
    }


{-| Make a point in a screen rectangle a point relative to the scene
CORRECT! @@TODO: set up fixtures
-}
scenePointFromElementPoint :
    Canvas.Camera.Model.Model
    -> ElementPointInContext
    -> ScenePoint
scenePointFromElementPoint camera pointInContext =
    let
        (SceneRectangle rect) =
            Tuple.second pointInContext.context

        (ElementPoint { x, y }) =
            pointInContext.point
    in
    ScenePoint
        { x = x * camera.zoom + Rectangle.x1 rect
        , y = y * camera.zoom + Rectangle.y1 rect
        }


{-| Wow, this is cumbersome and I'm running out of function names...
-}
absolutePointFromElementPoint : Canvas.Camera.Model.Model -> ElementPointInContext -> AbsolutePoint
absolutePointFromElementPoint camera pointInContext =
    scenePointFromElementPoint camera pointInContext
        |> absolutePointFromScenePoint camera


{-| Wow, this is cumbersome and I'm running out of function names...
-}
absolutePointFromElementPointSimple : Canvas.Camera.Model.Model -> ( Canvas.Events.ElementPoint, Canvas.Events.SceneRectangle ) -> AbsolutePoint
absolutePointFromElementPointSimple camera ( point, context ) =
    let
        (SceneRectangle rect) =
            context

        (ElementPoint { x, y }) =
            point

        scenePoint =
            ScenePoint
                { x = x * camera.zoom + Rectangle.x1 rect
                , y = y * camera.zoom + Rectangle.y1 rect
                }
    in
    scenePoint
        |> absolutePointFromScenePoint camera


{-| Make a point in a screen rectangle a point relative to the scene
CORRECT! @@TODO: set up fixtures
-}
elementPointFromScenePoint :
    Canvas.Camera.Model.Model
    -> SceneRectangle
    -> ScenePoint
    -> ElementPoint
elementPointFromScenePoint camera context scenePoint =
    let
        (SceneRectangle rect) =
            context

        (ScenePoint { x, y }) =
            scenePoint
    in
    ElementPoint
        { x = (x - Rectangle.x1 rect) / camera.zoom
        , y = (y - Rectangle.y1 rect) / camera.zoom
        }


scenePointFromAbsolutePoint :
    Canvas.Camera.Model.Model
    -> AbsolutePoint
    -> ScenePoint
scenePointFromAbsolutePoint camera (AbsolutePoint { x, y }) =
    { x = (x - camera.x) / camera.zoom
    , y = (y - camera.y) / camera.zoom
    }
        |> ScenePoint


absolutePointFromScenePoint :
    Canvas.Camera.Model.Model
    -> ScenePoint
    -> AbsolutePoint
absolutePointFromScenePoint camera (ScenePoint s) =
    scenePointToAbsolute camera s
        |> AbsolutePoint


absoluteRectangleFromScenePoints camera (ScenePoint p1) (ScenePoint p2) =
    let
        p1_ =
            scenePointToAbsolute
                camera
                p1

        p2_ =
            scenePointToAbsolute
                camera
                p2
    in
    Rectangle.fromMovement p1_ p2_


elementRectangleFromAbsoluteRectangle : AbsoluteRectangle -> AbsoluteRectangle -> ElementRectangle
elementRectangleFromAbsoluteRectangle (AbsoluteRectangle parentRect) (AbsoluteRectangle childRect) =
    childRect
        |> Rectangle.moveBy -(Rectangle.x1 parentRect) -(Rectangle.y1 parentRect)
        |> ElementRectangle


absoluteRectangleFromElementRectangle : AbsoluteRectangle -> ElementRectangle -> AbsoluteRectangle
absoluteRectangleFromElementRectangle (AbsoluteRectangle parentRect) (ElementRectangle childRect) =
    childRect
        |> Rectangle.moveBy (Rectangle.x1 parentRect) (Rectangle.y1 parentRect)
        |> AbsoluteRectangle


absolutePointFromElementPointAndParentRectangle : AbsoluteRectangle -> ElementPoint -> AbsolutePoint
absolutePointFromElementPointAndParentRectangle (AbsoluteRectangle parentRect) (ElementPoint point) =
    point
        |> Rectangle.movePoint { x = Rectangle.x1 parentRect, y = Rectangle.y1 parentRect }
        |> AbsolutePoint


elementRectangleFromSceneRectangle :
    Canvas.Camera.Model.Model
    -> AbsoluteRectangle
    -> SceneRectangle
    -> ElementRectangle
elementRectangleFromSceneRectangle camera parentDimension =
    absoluteRectangleFromSceneRectangle camera
        >> elementRectangleFromAbsoluteRectangle parentDimension


{-| Treat parent rectangle as zero
-}
normalizeParentRectangle : AbsoluteRectangle -> ElementRectangle
normalizeParentRectangle (AbsoluteRectangle rect) =
    Rectangle.moveToOrigin rect
        |> ElementRectangle


absoluteRectangleFromSceneRectangle :
    Canvas.Camera.Model.Model
    -> SceneRectangle
    -> AbsoluteRectangle
absoluteRectangleFromSceneRectangle camera (SceneRectangle r) =
    { x1 = (Rectangle.x1 r - camera.x) / camera.zoom
    , y1 = (Rectangle.y1 r - camera.y) / camera.zoom
    , x2 = (Rectangle.x2 r - camera.x) / camera.zoom
    , y2 = (Rectangle.y2 r - camera.y) / camera.zoom
    }
        |> Rectangle.fromPoints
        |> AbsoluteRectangle


sceneRectangleFromAbsoluteRectangle :
    Canvas.Camera.Model.Model
    -> AbsoluteRectangle
    -> SceneRectangle
sceneRectangleFromAbsoluteRectangle camera (AbsoluteRectangle r) =
    { x1 = Rectangle.x1 r * camera.zoom + camera.x
    , y1 = Rectangle.y1 r * camera.zoom + camera.y
    , x2 = Rectangle.x2 r * camera.zoom + camera.x
    , y2 = Rectangle.y2 r * camera.zoom + camera.y
    }
        |> Rectangle.fromPoints
        |> SceneRectangle


elementToAbsolutePoint :
    Canvas.Camera.Model.Model
    -> ElementPointInContext
    -> AbsolutePoint
elementToAbsolutePoint camera pointInContext =
    let
        (ElementPoint point) =
            pointInContext.point

        (AbsoluteRectangle rect) =
            absoluteRectangleFromSceneRectangle
                camera
                (Tuple.second pointInContext.context)
    in
    AbsolutePoint
        { x = Rectangle.x1 rect + point.x
        , y = Rectangle.y1 rect + point.y
        }


absoluteRectangleFromElementPoints :
    Canvas.Camera.Model.Model
    -> ElementPointInContext
    -> ElementPointInContext
    -> AbsoluteRectangle
absoluteRectangleFromElementPoints camera p1 p2 =
    let
        (AbsolutePoint p1_) =
            elementToAbsolutePoint
                camera
                p1

        (AbsolutePoint p2_) =
            elementToAbsolutePoint
                camera
                p2
    in
    Rectangle.fromMovement p1_ p2_
        |> AbsoluteRectangle
