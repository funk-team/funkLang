module Canvas.Camera.Move exposing (..)

{-| This allows the user to focus the camera on specific elements
-}

import Browser.Dom
import Canvas.Camera
import Canvas.Camera.Model
import Canvas.Msg
import Rectangle
import Spec.Element
import Spec.Element.Model
import Task
import Ui.Style


type alias ViewportSize =
    { width : Int, height : Int }


englobingCameraCmd : List Spec.Element.Model.Screen -> Cmd Canvas.Msg.Msg
englobingCameraCmd listScreens =
    let
        viewportToEnglobingCameraMsg { viewport } =
            let
                cameraSize =
                    { width = round viewport.width
                    , height = round viewport.height
                    }
            in
            englobingCamera listScreens cameraSize
                |> Canvas.Msg.UpdateCamera
    in
    Task.perform
        viewportToEnglobingCameraMsg
        Browser.Dom.getViewport


englobingCamera :
    List Spec.Element.Model.Screen
    -> ViewportSize
    -> Canvas.Camera.Model.Model
englobingCamera listScreens cameraSize =
    let
        maybeBoundingBox =
            listScreens
                |> List.map (.outerGeometry >> Spec.Element.screenSizeToRectangle)
                |> Rectangle.boundingBox
    in
    case maybeBoundingBox of
        Nothing ->
            Canvas.Camera.defaultCamera

        Just boundingBox ->
            focusCamera boundingBox cameraSize


{-| Focus camera on a zone of interest

the viewport is essentially the size of the browser window

based on the size of the zone of interest we calculate a zoom factor and the depending offset of the camera

-}
focusCamera :
    Rectangle.Rectangle
    -> ViewportSize
    -> Canvas.Camera.Model.Model
focusCamera zoneOfInterest viewportSize_ =
    let
        xPadding =
            Ui.Style.sidebarWidth + 10

        yPadding =
            Ui.Style.headerHeight + 10

        -- viewportSize minus UI overlays (padding)
        viewportSize =
            { viewportSize_
                | width = viewportSize_.width - (xPadding * 2)
                , height = viewportSize_.height - (yPadding * 2)
            }

        zoomX =
            toFloat viewportSize.width / Rectangle.width zoneOfInterest

        zoomY =
            toFloat viewportSize.height / Rectangle.height zoneOfInterest

        zoom =
            clamp Canvas.Camera.zoomLowerBoundary 1 (min zoomX zoomY)

        -- offset for position of zone of interest
        zoneOfInterestInTopLeftCameraCorner =
            { x = -zoom * Rectangle.x1 zoneOfInterest
            , y = -zoom * Rectangle.y1 zoneOfInterest
            }

        -- offset for size of zone of interest
        deltaZoneOfInterestViewportSize =
            { x = toFloat viewportSize.width - (zoom * Rectangle.width zoneOfInterest)
            , y = toFloat viewportSize.height - (zoom * Rectangle.height zoneOfInterest)
            }
    in
    { x = zoneOfInterestInTopLeftCameraCorner.x + deltaZoneOfInterestViewportSize.x / 2 + xPadding
    , y = zoneOfInterestInTopLeftCameraCorner.y + deltaZoneOfInterestViewportSize.y / 2 + yPadding
    , zoom = zoom
    , gesture = Nothing
    }
