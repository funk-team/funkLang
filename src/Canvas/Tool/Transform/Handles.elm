module Canvas.Tool.Transform.Handles exposing (..)

import Canvas.Camera.Model
import Compass
import Element
import Element.Border
import Element.Events.Extra
import Html.Attributes
import Json.Decode as Decode
import Ui.Help
import Ui.Style


outline : a -> Element.Attribute msg
outline camera =
    Ui.Style.class "outline"


lightOutline : Canvas.Camera.Model.Model -> Element.Attribute msg
lightOutline camera =
    Ui.Style.class "outline-light"


handles :
    Canvas.Camera.Model.Model
    -> Bool
    -> (Compass.Direction -> Decode.Decoder msg)
    -> List (Element.Attribute msg)
handles camera shouldPassThrough msgDecoder =
    let
        adjustedOutline =
            outline camera

        dots : List (Element.Attribute msg)
        dots =
            [ Compass.North
            , Compass.NorthWest
            , Compass.West
            , Compass.SouthWest
            , Compass.South
            , Compass.SouthEast
            , Compass.East
            , Compass.NorthEast
            , Compass.Center
            ]
                |> List.map (handle camera shouldPassThrough msgDecoder)

        rendered =
            adjustedOutline :: dots
    in
    rendered


{-| Render a handle for different compass directions
-}
handle :
    Canvas.Camera.Model.Model
    -> Bool
    -> (Compass.Direction -> Decode.Decoder msg)
    -> Compass.Direction
    -> Element.Attribute msg
handle camera shouldPassThrough msgDecoder handleId =
    let
        diameter =
            10

        cur direction =
            Element.htmlAttribute
                (Html.Attributes.style "cursor" (direction ++ "-resize"))

        eventHandler =
            Element.Events.Extra.onMouseDown (msgDecoder handleId)

        curCenter =
            Element.htmlAttribute
                (Html.Attributes.style "cursor" "move")

        ( cursor, ( x, y ), ( offsetX, offsetY ) ) =
            case handleId of
                Compass.North ->
                    ( cur "n", ( Element.centerX, Element.alignTop ), ( Element.moveLeft 0, Element.moveUp <| diameter / 1.5 - (1 / camera.zoom) ) )

                Compass.NorthWest ->
                    ( cur "nw", ( Element.alignLeft, Element.alignTop ), ( Element.moveLeft <| diameter / 1.5 - (1 / camera.zoom), Element.moveUp <| diameter / 1.5 - (1 / camera.zoom) ) )

                Compass.West ->
                    ( cur "w", ( Element.alignLeft, Element.centerY ), ( Element.moveLeft <| diameter / 1.5 - (1 / camera.zoom), Element.moveDown 0 ) )

                Compass.SouthWest ->
                    ( cur "sw", ( Element.alignLeft, Element.alignBottom ), ( Element.moveLeft <| diameter / 1.5 - (1 / camera.zoom), Element.moveDown <| diameter / 1.5 - (1 / camera.zoom) ) )

                Compass.South ->
                    ( cur "s", ( Element.centerX, Element.alignBottom ), ( Element.moveLeft 0, Element.moveDown <| diameter / 1.5 - (1 / camera.zoom) ) )

                Compass.SouthEast ->
                    ( cur "se", ( Element.alignRight, Element.alignBottom ), ( Element.moveRight <| diameter / 1.5 - (1 / camera.zoom), Element.moveDown <| diameter / 1.5 - (1 / camera.zoom) ) )

                Compass.East ->
                    ( cur "e", ( Element.alignRight, Element.centerY ), ( Element.moveRight <| diameter / 1.5 - (1 / camera.zoom), Element.moveDown 0 ) )

                Compass.NorthEast ->
                    ( cur "ne", ( Element.alignRight, Element.alignTop ), ( Element.moveRight <| diameter / 1.5 - (1 / camera.zoom), Element.moveUp <| diameter / 1.5 - (1 / camera.zoom) ) )

                Compass.Center ->
                    ( curCenter, ( Element.centerX, Element.centerY ), ( Element.moveRight 0, Element.moveUp 0 ) )

        scaleFactor =
            Element.scale (1 / camera.zoom)

        attribs =
            [ Element.width (Element.px diameter)
            , Element.height (Element.px diameter)
            , Ui.Style.style "background-color" "var(--highlight-solid)"
            , Element.Border.rounded (diameter // 2)
            , cursor

            -- handle position
            , x
            , y
            , offsetX
            , offsetY
            , scaleFactor

            -- events
            , eventHandler
            ]
                ++ (if shouldPassThrough then
                        [ Ui.Help.noPointerEvents ]

                    else
                        [ Ui.Help.allPointerEvents ]
                   )
    in
    Element.el
        attribs
        Element.none
        |> Element.inFront
