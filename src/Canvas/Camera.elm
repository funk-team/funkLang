module Canvas.Camera exposing (..)

{-| Panning and zooming
Complexity:
The user wants to zoom from / to the location of the mouse

    + - - > x
    |
    |
    v y

    For more info about the logic happening here please have a look at GH-104:
    https://github.com/funk-team/funk/issues/104

-}

import BoundingClientRectangle
import Canvas.Camera.Model
import Canvas.Events
import Element
import Html
import Html.Events
import Json.Decode as Decode
import Keyboard
import Rectangle



---- GET MOUSE EVENTS ----


type alias MouseEvent =
    { position : Canvas.Events.ScenePoint
    , screenSize : Canvas.Events.SceneRectangle
    , detail : GestureOrWheel
    }


type GestureOrWheel
    = GestureEnd
    | Gesture Canvas.Camera.Model.GestureData
    | Wheel WheelData


type alias WheelData =
    { ctrlKey : Bool
    , deltaX : Float
    , deltaY : Float
    }


events : (MouseEvent -> msg) -> List (Element.Attribute msg)
events msg =
    [ onWheel msg
    , onGestureChange msg
    , onGestureEnd msg
    ]
        |> List.map Element.htmlAttribute


onWheel : (MouseEvent -> msg) -> Html.Attribute msg
onWheel msg =
    Html.Events.preventDefaultOn
        "wheel"
        (decodeOutsideSidebar (decodeMouseEvent (Decode.map Wheel decodeWheelData))
            |> Decode.map (\ev -> ( msg ev, True ))
        )


onGestureChange : (MouseEvent -> msg) -> Html.Attribute msg
onGestureChange msg =
    Html.Events.preventDefaultOn
        "gesturechange"
        (decodeOutsideSidebar (decodeMouseEvent (Decode.map Gesture decodeGestureData))
            |> Decode.map (\ev -> ( msg ev, True ))
        )


onGestureEnd : (MouseEvent -> msg) -> Html.Attribute msg
onGestureEnd msg =
    Html.Events.preventDefaultOn
        "gestureend"
        (decodeOutsideSidebar (decodeMouseEvent (Decode.succeed GestureEnd))
            |> Decode.map (\ev -> ( msg ev, True ))
        )


decodeOutsideSidebar decoder =
    Decode.at [ "target", "funk_isInSidebar" ] Decode.bool
        |> Decode.andThen
            (\isInside ->
                case isInside of
                    True ->
                        Decode.fail ""

                    False ->
                        decoder
            )


decodeMouseEvent subDecoder =
    Decode.map3
        MouseEvent
        (Decode.oneOf
            [ Canvas.Events.decodeScenePoint

            -- safari has no x/y points on gesture events
            , Decode.map Canvas.Events.ScenePoint Rectangle.readClientPoint
            ]
        )
        (Decode.field "target" BoundingClientRectangle.decodeAsRectangle |> Decode.map Canvas.Events.SceneRectangle)
        subDecoder


decodeWheelData =
    Decode.map3 WheelData
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "deltaX" Decode.float)
        (Decode.field "deltaY" Decode.float)


decodeGestureData =
    Decode.map Canvas.Camera.Model.GestureData
        (Decode.field "scale" Decode.float)



---- ZOOM AND SCROLL LOGIC ----


zoomLowerBoundary =
    0.01


zoomHigherBoundary =
    5


clampZoom =
    clamp zoomLowerBoundary zoomHigherBoundary


updateCamera : List Keyboard.Key -> MouseEvent -> Canvas.Camera.Model.Model -> Canvas.Camera.Model.Model
updateCamera keys { detail, position, screenSize } camera =
    case detail of
        GestureEnd ->
            Canvas.Camera.Model.applyGesture camera

        Gesture { scale } ->
            { camera | gesture = Just { scale = scale, position = position } }

        Wheel { ctrlKey, deltaX, deltaY } ->
            -- ctrlKey is pressed when user uses gesture zoom on OSX
            case ctrlKey of
                True ->
                    let
                        -- if the user uses a mouse rather than a trackpad, fewer events will fire but the delat is higher
                        -- this can lead to huge jumps which are confusing.
                        -- so we reduce the maximum size of the jumps.
                        cappedScrollStepSize =
                            clamp -10 10 deltaY

                        zoomBefore =
                            camera.zoom

                        zoomAfter =
                            clampZoom (zoomBefore - (cappedScrollStepSize * 0.01))
                    in
                    Canvas.Camera.Model.adjustFocusPoint camera position zoomAfter

                False ->
                    if List.member Keyboard.Shift keys then
                        { camera
                            | x = clampCamera <| camera.x - deltaY
                        }

                    else
                        { camera
                            | x = clampCamera <| camera.x - deltaX
                            , y = clampCamera <| camera.y - deltaY
                        }



---- APPLYING TRANSFORMATIONS TO THE CANVAS ----


canvasCodes camera =
    let
        { zoom, x, y } =
            Canvas.Camera.Model.applyGesture camera
    in
    [ Element.moveDown y
    , Element.moveRight x
    , Element.scale zoom
    ]



---- UTILS ---


defaultCamera : Canvas.Camera.Model.Model
defaultCamera =
    { zoom = 1, x = 0, y = 0, gesture = Nothing }


clampCamera =
    identity
