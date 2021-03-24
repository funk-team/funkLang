module Canvas.Tool.Debug exposing
    ( Msg
    , State(..)
    , augmentCanvas
    , augmentElement
    , augmentScene
    , decodeState
    , encodeState
    )

import Canvas.Camera.Convert
import Canvas.Camera.Model
import Canvas.Events
import Element
import Element.Background
import Element.Border
import Json.Decode as Decode
import Json.Encode as Encode
import Rectangle
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Ui.Help
import Ui.Style


augmentElement { camera, element } state =
    [ elementEvents
    ]
        ++ viewOnElement element camera state


renderCrossHair : Rectangle.Point -> Element.Attribute msg
renderCrossHair { x, y } =
    let
        elementCrosshair =
            [ Element.height (Element.px 10)
            , Element.width (Element.px 10)
            , Element.Background.color Ui.Style.highlightColorSolid
            , Element.moveDown (y - 5)
            , Element.moveRight (x - 5)

            -- , Element.inFront <| Element.el [ Ui.Help.noPointerEvents ] <| Element.text domId
            , Ui.Help.noPointerEvents
            ]

        crosshair =
            Element.none
                |> Element.el elementCrosshair
                |> Element.inFront
    in
    crosshair


viewOnElement :
    Spec.Element.Model.Element size
    -> Canvas.Camera.Model.Model
    -> State
    -> List (Element.Attribute msg)
viewOnElement element camera s =
    case s of
        HoveringElement domId pointInContext ->
            let
                { context, point } =
                    pointInContext

                elementId =
                    Spec.Element.Id.toHtmlIdRaw element.shared.id

                isThisElement =
                    domId == elementId

                (Canvas.Events.ElementPoint elPointThroughScene) =
                    pointInContext
                        |> Canvas.Camera.Convert.scenePointFromElementPoint camera
                        |> Canvas.Camera.Convert.elementPointFromScenePoint camera (Tuple.second context)

                (Canvas.Events.ElementPoint elementPoint) =
                    point
            in
            if isThisElement then
                [ renderCrossHair elementPoint, renderCrossHair elPointThroughScene ]

            else
                []

        _ ->
            []


elementEvents =
    Decode.map2 HoveringElement
        readTargetId
        Canvas.Events.readPointInContext
        |> Canvas.Events.onMouseMove


readTargetId : Decode.Decoder String
readTargetId =
    Decode.at
        [ "currentTarget", "id" ]
        Decode.string


sceneEvents =
    Canvas.Events.decodeScenePoint
        |> Decode.map HoveringScene
        |> Canvas.Events.onMouseMove


viewOnScene : Canvas.Camera.Model.Model -> State -> List (Element.Attribute msg)
viewOnScene camera state =
    case state of
        -- this one requires no projection
        HoveringScene point ->
            let
                (Canvas.Events.ScenePoint { x, y }) =
                    point
            in
            Element.el
                [ Element.Background.color Ui.Style.importantHighlightColorSolid
                , Element.moveRight (x - 5)
                , Element.moveDown (y - 5)
                , Element.width (Element.px 10)
                , Element.height (Element.px 10)
                , Element.Border.rounded 5
                , Ui.Help.noPointerEvents
                ]
                Element.none
                |> Element.inFront
                |> List.singleton

        HoveringElement domId pointInContext ->
            let
                (Canvas.Events.SceneRectangle rect) =
                    Tuple.second pointInContext.context

                (Canvas.Events.ScenePoint point) =
                    Canvas.Camera.Convert.scenePointFromElementPoint camera pointInContext

                outlineOfElement =
                    Element.el
                        [ Ui.Help.noPointerEvents
                        , Element.Border.width 1
                        , Element.Border.color Ui.Style.importantHighlightColorSolid
                        , Element.height (Element.px (Rectangle.height rect |> round))
                        , Element.width (Element.px (Rectangle.width rect |> round))
                        , Element.moveRight (Rectangle.x1 rect)
                        , Element.moveDown (Rectangle.y1 rect)
                        ]
                        Element.none

                crosshair =
                    [ -- horizontal
                      Element.el
                        [ Element.height (Element.px 2)
                        , Element.width (Element.px 20)
                        , Element.Background.color Ui.Style.importantHighlightColorSolid
                        , Element.moveDown (point.y - 1)
                        , Element.moveRight (point.x - 10)
                        , Ui.Help.noPointerEvents
                        ]
                        Element.none

                    -- vertical
                    , Element.el
                        [ Element.width (Element.px 2)
                        , Element.height (Element.px 20)
                        , Element.Background.color Ui.Style.importantHighlightColorSolid
                        , Element.moveDown (point.y - 10)
                        , Element.moveRight (point.x - 1)
                        , Ui.Help.noPointerEvents
                        ]
                        Element.none
                    ]
            in
            outlineOfElement
                :: crosshair
                |> List.map Element.inFront

        _ ->
            Element.none
                |> Element.inFront
                |> List.singleton


augmentCanvas =
    viewOnCanvas


viewOnCanvas : Canvas.Camera.Model.Model -> State -> List (Element.Attribute msg)
viewOnCanvas camera state =
    case state of
        -- this one requires no projection
        HoveringElement domId pointInContext ->
            let
                (Canvas.Events.AbsoluteRectangle rect) =
                    Tuple.second pointInContext.context
                        |> Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera

                outlineOfElement =
                    Element.el
                        [ Ui.Help.noPointerEvents
                        , Element.Border.width 2
                        , Element.Border.color Ui.Style.highlightColorSolid
                        , Element.height (Element.px (Rectangle.height rect |> round))
                        , Element.width (Element.px (Rectangle.width rect |> round))
                        , Element.moveRight (Rectangle.x1 rect)
                        , Element.moveDown (Rectangle.y1 rect)
                        ]
                        Element.none
            in
            [ outlineOfElement ]
                |> List.map Element.inFront

        HoveringScene scenePoint ->
            let
                (Canvas.Events.AbsolutePoint absPoint) =
                    Canvas.Camera.Convert.absolutePointFromScenePoint camera scenePoint
            in
            renderCrossHair absPoint
                |> List.singleton

        NotHovering ->
            []


augmentScene camera state =
    sceneEvents :: viewOnScene camera state


type alias Msg =
    State



-- [generator-start]


type State
    = NotHovering
    | HoveringElement String Canvas.Events.ElementPointInContext
    | HoveringScene Canvas.Events.ScenePoint



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeState =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeStateHelp


decodeStateHelp constructor =
    case constructor of
        "NotHovering" ->
            Decode.succeed NotHovering

        "HoveringElement" ->
            Decode.map2
                HoveringElement
                (Decode.field "A1" Decode.string)
                (Decode.field "A2" Canvas.Events.decodeElementPointInContext)

        "HoveringScene" ->
            Decode.map
                HoveringScene
                (Decode.field "A1" Canvas.Events.decodeScenePoint)

        other ->
            Decode.fail <| "Unknown constructor for type State: " ++ other


encodeState a =
    case a of
        NotHovering ->
            Encode.object
                [ ( "Constructor", Encode.string "NotHovering" )
                ]

        HoveringElement a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "HoveringElement" )
                , ( "A1", Encode.string a1 )
                , ( "A2", Canvas.Events.encodeElementPointInContext a2 )
                ]

        HoveringScene a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "HoveringScene" )
                , ( "A1", Canvas.Events.encodeScenePoint a1 )
                ]



-- [generator-end]
