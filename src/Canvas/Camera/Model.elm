module Canvas.Camera.Model exposing (..)

{-| Basic model that captures panning and zooming on a scene
-}

import Canvas.Events



{-
   + - - > x
   |
   |
   v y

   The camera x and y form the vector going from the top left corner of the
   screen to the origin of the canvas. No zoom is ever applied to this vector.
   The zoom is applied to all the elements inside the canvas.
-}


type alias Model =
    { zoom : Float
    , x : Float
    , y : Float
    , gesture : Maybe GestureState
    }


type alias GestureData =
    { scale : Float
    }


type alias GestureState =
    { scale : Float
    , position : Canvas.Events.ScenePoint
    }


applyGesture : Model -> Model
applyGesture camera =
    case camera.gesture of
        Just { scale, position } ->
            let
                zoomAfter =
                    scale * camera.zoom
            in
            adjustFocusPoint
                { camera | gesture = Nothing }
                position
                zoomAfter

        Nothing ->
            camera


{-|

    import Canvas.Events

    adjustFocusPoint { gesture = Nothing, x = -164, y = 390, zoom = 1.5 } (Canvas.Events.ScenePoint { x = 459, y = 512 }) 1.6
    --> { gesture = Nothing, x = -205.53333333333333, y = 381.8666666666667, zoom = 1.6 }

    adjustFocusPoint { gesture = Nothing, x = -164, y = 390, zoom = 1.5 } (Canvas.Events.ScenePoint { x = 459, y = 512 }) 0.9
    --> { gesture = Nothing, x = 85.20000000000003, y = 438.8, zoom = 0.9 }

-}
adjustFocusPoint : Model -> Canvas.Events.ScenePoint -> Float -> Model
adjustFocusPoint camera position zoomAfter =
    let
        (Canvas.Events.ScenePoint mouseOffset) =
            position

        transformationBefore =
            { x = camera.x
            , y = camera.y
            }

        zoomFactor =
            zoomAfter / camera.zoom

        -- when the user zooms we want to zoom towards the where the mouse is currently located
        -- so we need to recalculate the offset of the camera.
        { x, y } =
            computeCodeAfter transformationBefore mouseOffset zoomFactor

        computeCodeAfter vb m z =
            let
                va =
                    { x = z * vb.x - m.x * (z - 1)
                    , y = z * vb.y - m.y * (z - 1)
                    }
            in
            va
    in
    { camera
        | zoom = zoomAfter
        , x = x
        , y = y
    }
