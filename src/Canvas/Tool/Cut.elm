module Canvas.Tool.Cut exposing (..)

import BoundingClientRectangle
import Canvas.Camera
import Canvas.Camera.Model
import Canvas.Events
import Canvas.Guides
import Canvas.Selection
import Canvas.Tool.AugmentationParams
import Element
import Element.Background
import Element.Border
import Element.Events.Extra
import Element.Font
import Html.Attributes
import Html.Events
import Interface.Model
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Rectangle
import Spec
import Spec.Element
import Spec.Element.Id
import Spec.Element.Layout
import Spec.Element.Layout.Length
import Spec.Element.Layout.Padding
import Spec.Element.Model
import Spec.Model
import Spec.Mutation
import Ui.Help
import Ui.Style


augmentScene =
    [ Element.Events.Extra.onMouseMove (Decode.succeed HoverScene)
    ]


renderCutPreview : Spec.Model.WithSpec userModel -> Interface.Model.ScopeData -> Canvas.Camera.Model.Model -> HoverData -> List (Element.Attribute msg)
renderCutPreview userModel scope camera eventData =
    let
        (Canvas.Events.ElementPoint offsetPoint) =
            eventData.offset
    in
    List.map Element.inFront <|
        case determineDirection eventData.dimensions offsetPoint of
            AlongXAxis ->
                let
                    ( aboveLine, belowLine ) =
                        determineSplit
                            (Rectangle.height eventData.dimensions)
                            offsetPoint.y

                    labelBelowLine =
                        Element.el
                            [ Element.moveDown (offsetPoint.y + (26 / camera.zoom))
                            , Element.moveRight 10
                            , Element.Font.color Ui.Style.highlightColorSolid
                            , Ui.Help.noPointerEvents
                            , Element.scale (1 / camera.zoom)
                            , Ui.Style.style "transform-origin" "bottom left"
                            ]
                            (behaviorToLabel userModel scope belowLine)

                    labelAboveLine =
                        Element.el
                            [ Element.moveDown (offsetPoint.y - (40 / camera.zoom))
                            , Element.moveRight 10
                            , Element.Font.color Ui.Style.highlightColorSolid
                            , Ui.Help.noPointerEvents
                            , Element.scale (1 / camera.zoom)
                            , Ui.Style.style "transform-origin" "top left"
                            ]
                            (behaviorToLabel userModel scope aboveLine)
                in
                [ Element.el
                    ([ Element.Background.color Ui.Style.importantHighlightColorSolid
                     , Element.Font.color Ui.Style.importantHighlightColorSolid
                     ]
                        ++ Canvas.Guides.horizontalLineWithinElement camera offsetPoint.y
                        ++ cutLineEndPoint camera AlongXAxis
                    )
                    Element.none
                , Element.el
                    ([ Ui.Help.noPointerEvents
                     , Element.htmlAttribute (Html.Attributes.style "background-position" ("0 calc(" ++ String.fromFloat offsetPoint.y ++ "px)"))
                     , Element.htmlAttribute (Html.Attributes.style "background-repeat" "none")
                     , Ui.Style.highlightColorGradientVertical
                     , Element.inFront <|
                        labelBelowLine
                     , Element.inFront <|
                        labelAboveLine
                     ]
                        ++ Canvas.Guides.verticalLineWithinElement camera offsetPoint.x
                    )
                    Element.none
                ]

            AlongYAxis ->
                let
                    ( leftOfLine, rightOfLine ) =
                        determineSplit (Rectangle.width eventData.dimensions) offsetPoint.x
                in
                [ Element.el
                    ([ Element.Background.color Ui.Style.importantHighlightColorSolid
                     , Element.Font.color Ui.Style.importantHighlightColorSolid
                     ]
                        ++ Canvas.Guides.verticalLineWithinElement camera offsetPoint.x
                        ++ cutLineEndPoint camera AlongYAxis
                    )
                    Element.none
                , Element.el
                    ([ Ui.Help.noPointerEvents
                     , Element.htmlAttribute (Html.Attributes.style "background-position" (String.fromFloat offsetPoint.x ++ "px 0"))
                     , Element.htmlAttribute (Html.Attributes.style "background-repeat" "none")
                     , Ui.Style.highlightColorGradientHorizontal
                     , Element.onRight <|
                        Element.el
                            [ Element.moveRight (offsetPoint.x + 30 - Rectangle.width eventData.dimensions)
                            , Element.moveDown 10
                            , Element.Font.alignLeft
                            , Element.Font.color Ui.Style.highlightColorSolid
                            , Ui.Help.noPointerEvents
                            , Element.scale (1 / camera.zoom)
                            , Ui.Style.style "transform-origin" "bottom left"
                            ]
                            (behaviorToLabel userModel scope rightOfLine)
                     , Element.onLeft <|
                        Element.el
                            [ Element.moveRight (offsetPoint.x - 35)
                            , Element.moveDown 10
                            , Element.Font.color Ui.Style.highlightColorSolid
                            , Ui.Help.noPointerEvents
                            , Element.scale (1 / camera.zoom)
                            , Ui.Style.style "transform-origin" "bottom right"
                            ]
                            (behaviorToLabel userModel scope leftOfLine)
                     ]
                        ++ Canvas.Guides.horizontalLineWithinElement camera offsetPoint.y
                    )
                    Element.none
                ]


behaviorToLabel :
    Spec.Model.WithSpec userModel
    -> Interface.Model.ScopeData
    -> Spec.Element.Layout.Behavior
    -> Element.Element msg
behaviorToLabel userModel scopeData behavior =
    case behavior of
        Spec.Element.Layout.Fill ->
            Element.text "fill"

        Spec.Element.Layout.Shrink ->
            Element.text "shrink"

        Spec.Element.Layout.Static len ->
            Element.text (String.fromFloat (Spec.resolve userModel scopeData len) ++ "px")


cutLineEndPoint : Canvas.Camera.Model.Model -> Direction -> List (Element.Attribute msg)
cutLineEndPoint camera horzOrVert =
    let
        diameter =
            12 / camera.zoom

        endStyles =
            [ Element.width (Element.px (diameter |> round))
            , Element.height (Element.px (diameter |> round))
            , Element.Background.color Ui.Style.importantHighlightColorSolid
            , Element.centerX
            , Element.Border.rounded 3
            ]
    in
    case horzOrVert of
        AlongXAxis ->
            [ Element.onLeft (Element.el (endStyles ++ [ Element.moveUp (diameter / 2) ]) Element.none)
            , Element.onRight (Element.el (endStyles ++ [ Element.moveUp (diameter / 2) ]) Element.none)
            ]

        AlongYAxis ->
            [ Element.above (Element.el endStyles Element.none)
            , Element.below (Element.el endStyles Element.none)
            ]


{-| Add events and HUD elements
-}
augment :
    Spec.Model.WithSpec userModel
    -> Canvas.Tool.AugmentationParams.AugmentationParams
    -> State
    -> List (Element.Attribute Msg)
augment userModel augmentationParams state =
    let
        guides =
            case state of
                NotHovering ->
                    []

                Hovering data ->
                    if augmentationParams.element.shared.id == data.target.id then
                        renderCutPreview userModel augmentationParams.scope augmentationParams.camera data

                    else
                        []

        onMouseMove : Decode.Decoder msg -> Element.Attribute msg
        onMouseMove =
            Decode.map (\data -> ( data, True ))
                >> Html.Events.stopPropagationOn "mousemove"
                >> Element.htmlAttribute

        trackClickOnElement =
            Element.Events.Extra.onClickNoBubble Click

        trackMovingOnElement =
            onMouseMove
                (decodeHoverDataFromEvent augmentationParams)
                |> Element.mapAttribute Hover

        events =
            [ trackMovingOnElement
            , trackClickOnElement
            , Element.htmlAttribute (Html.Attributes.class "tracked")
            ]
    in
    events ++ guides


projectRectangleFromEvent : Canvas.Camera.Model.Model -> Rectangle.Rectangle -> Rectangle.Rectangle
projectRectangleFromEvent camera (Rectangle.Rectangle { x1, x2, y1, y2 }) =
    { x1 = (x1 + camera.x) / camera.zoom
    , y1 = (y1 + camera.y) / camera.zoom
    , x2 = (x2 + camera.x) / camera.zoom
    , y2 = (y2 + camera.y) / camera.zoom
    }
        |> Rectangle.Rectangle


decodeHoverDataFromEvent :
    Canvas.Tool.AugmentationParams.AugmentationParams
    -> Decode.Decoder HoverData
decodeHoverDataFromEvent { camera, element, selectionItem } =
    Decode.map3
        (HoverData element.shared)
        (Decode.succeed selectionItem)
        (Decode.field "target" BoundingClientRectangle.decodeAsRectangle
            |> Decode.map (projectRectangleFromEvent camera)
        )
        Canvas.Events.decodeOffsetPoint


{-| Take the data captured in an event and return what the new children before and
after the new cut line will look like / behave like.
-}
makeLayoutFromEventData :
    HoverData
    -> { firstSize : Spec.Element.Layout.Size, secondSize : Spec.Element.Layout.Size }
makeLayoutFromEventData eventData =
    let
        ( direction, before, after ) =
            determineDirectionAndSplit eventData

        minMax =
            Spec.Element.Layout.MinMax
                Spec.Element.Layout.Length.null
                Spec.Element.Layout.Length.null
    in
    case direction of
        AlongXAxis ->
            let
                firstSize : Spec.Element.Layout.Size
                firstSize =
                    { width = { behavior = Spec.Element.Layout.Fill, minMax = minMax }
                    , height = { behavior = before, minMax = minMax }
                    }

                secondSize : Spec.Element.Layout.Size
                secondSize =
                    { height = { behavior = after, minMax = minMax }
                    , width = { behavior = Spec.Element.Layout.Fill, minMax = minMax }
                    }
            in
            { firstSize = firstSize
            , secondSize = secondSize
            }

        AlongYAxis ->
            { firstSize =
                { height = { behavior = Spec.Element.Layout.Fill, minMax = minMax }
                , width = { behavior = before, minMax = minMax }
                }
            , secondSize =
                { height = { behavior = Spec.Element.Layout.Fill, minMax = minMax }, width = { behavior = after, minMax = minMax } }
            }


update : Random.Seed -> Msg -> State -> ( State, Maybe Spec.Mutation.Mutation, Random.Seed )
update seed msg s =
    case msg of
        HoverScene ->
            ( NotHovering, Nothing, seed )

        Hover data ->
            ( Hovering data, Nothing, seed )

        Click ->
            case s of
                Hovering eventData ->
                    let
                        shared : Spec.Element.Model.Shared
                        shared =
                            eventData.target

                        ( direction, before, after ) =
                            determineDirectionAndSplit eventData

                        ( firstId, firstNewSeed ) =
                            Spec.Element.Id.random seed

                        ( secondId, secondNewSeed ) =
                            Spec.Element.Id.random firstNewSeed

                        { firstSize, secondSize } =
                            makeLayoutFromEventData eventData

                        firstChild =
                            makeChild firstId firstSize

                        secondChild =
                            makeChild secondId secondSize

                        newShared : Spec.Element.Model.Shared
                        newShared =
                            { shared
                                | flow =
                                    case direction of
                                        AlongXAxis ->
                                            Spec.Element.Layout.Column

                                        AlongYAxis ->
                                            Spec.Element.Layout.Row
                                , children =
                                    Spec.Element.Model.FlowChildren
                                        [ firstChild
                                        , secondChild
                                        ]
                            }

                        mutation : Spec.Mutation.Mutation
                        mutation =
                            newShared
                                |> Spec.Mutation.UpdateShared eventData.selectionItem
                    in
                    ( NotHovering, Just mutation, secondNewSeed )

                NotHovering ->
                    ( NotHovering, Nothing, seed )


makeChild : Spec.Element.Id.Id -> Spec.Element.Layout.Size -> Spec.Element.Model.FlowElement
makeChild id size =
    let
        shared : Spec.Element.Model.Shared
        shared =
            { children = Spec.Element.Model.AbsoluteChildren []
            , id = id
            , flow = Spec.Element.Layout.Column
            , padding = Spec.Element.Layout.Padding.defaultPadding
            , spacing = Nothing
            , label = ""
            , kind = Spec.Element.Model.Box
            }

        newSibling : Spec.Element.Model.FlowElement
        newSibling =
            { outerGeometry = { size = size, alignment = { x = Nothing, y = Nothing } }
            , shared = shared
            }
    in
    newSibling


type Direction
    = AlongXAxis
    | AlongYAxis


determineDirectionAndSplit : HoverData -> ( Direction, Spec.Element.Layout.Behavior, Spec.Element.Layout.Behavior )
determineDirectionAndSplit data =
    let
        (Canvas.Events.ElementPoint offsetPoint) =
            data.offset

        direction =
            determineDirection data.dimensions offsetPoint

        ( before, after ) =
            case direction of
                AlongXAxis ->
                    determineSplit (Rectangle.height data.dimensions) offsetPoint.y

                AlongYAxis ->
                    determineSplit (Rectangle.width data.dimensions) offsetPoint.x
    in
    ( direction, before, after )



-------x-------


determineSplit : Float -> Float -> ( Spec.Element.Layout.Behavior, Spec.Element.Layout.Behavior )
determineSplit fullLength offset =
    let
        roughlyEqualInSize =
            offset * 1.85 < fullLength && offset * 2.15 > fullLength

        firstHalfIsBigger =
            offset * 2 > fullLength
    in
    if roughlyEqualInSize then
        ( Spec.Element.Layout.Fill, Spec.Element.Layout.Fill )

    else if firstHalfIsBigger then
        ( Spec.Element.Layout.Fill, Spec.Element.Layout.Static <| Spec.Element.Layout.Length.init (fullLength - offset) )

    else
        ( Spec.Element.Layout.Static <| Spec.Element.Layout.Length.init offset, Spec.Element.Layout.Fill )


{-| Which direction should the cut go? AlongYAxis or AlongXAxis through the element?
<
@@TODO: think about idea: Fuzz unit tests and then toggle results for visual testing
Think monte carlo sampling visualization of results

    import Rectangle

    determineDirection
        (Rectangle.fromPoints {x1 = 0, x2 = 100, y1 = 100, y2 = 200})
        ({x = 20, y = 10})
        --> AlongYAxis

    determineDirection
        (Rectangle.fromPoints {x1 = 200, x2 = 500, y1 = 300, y2 = 900})
        ({x = 140, y = 200})
        --> AlongYAxis

-}
determineDirection : Rectangle.Rectangle -> Rectangle.Point -> Direction
determineDirection dimensions offset =
    let
        -- between 0-1 relative to simplify calculations
        offsetX =
            offset.x
                / Rectangle.width dimensions

        offsetY =
            offset.y
                / Rectangle.height dimensions

        leftHalfClassification =
            if
                (offsetY < offsetX)
                    || (offsetY > 1 - offsetX)
            then
                AlongYAxis

            else
                AlongXAxis

        -- it is below the line going from bottom left to top right
        rightHalfClassfication =
            if
                (offsetY > offsetX)
                    || (offsetY < 1 - offsetX)
            then
                AlongYAxis

            else
                AlongXAxis

        -- it is below the line going from bottom left to top right
    in
    if offsetX < 0.5 then
        leftHalfClassification

    else
        rightHalfClassfication


type Msg
    = Hover HoverData
    | HoverScene
    | Click



-- [generator-start]


type State
    = Hovering HoverData
    | NotHovering


type alias HoverData =
    { target : Spec.Element.Model.Shared
    , selectionItem : Canvas.Selection.SelectionItem
    , dimensions : Rectangle.Rectangle
    , offset : Canvas.Events.ElementPoint
    }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeHoverData =
    Decode.map4
        HoverData
        (Decode.field "target" Spec.Element.Model.decodeShared)
        (Decode.field "selectionItem" Canvas.Selection.decodeSelectionItem)
        (Decode.field "dimensions" Rectangle.decodeRectangle)
        (Decode.field "offset" Canvas.Events.decodeElementPoint)


decodeState =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeStateHelp


decodeStateHelp constructor =
    case constructor of
        "Hovering" ->
            Decode.map
                Hovering
                (Decode.field "A1" decodeHoverData)

        "NotHovering" ->
            Decode.succeed NotHovering

        other ->
            Decode.fail <| "Unknown constructor for type State: " ++ other


encodeHoverData a =
    Encode.object
        [ ( "target", Spec.Element.Model.encodeShared a.target )
        , ( "selectionItem", Canvas.Selection.encodeSelectionItem a.selectionItem )
        , ( "dimensions", Rectangle.encodeRectangle a.dimensions )
        , ( "offset", Canvas.Events.encodeElementPoint a.offset )
        ]


encodeState a =
    case a of
        Hovering a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Hovering" )
                , ( "A1", encodeHoverData a1 )
                ]

        NotHovering ->
            Encode.object
                [ ( "Constructor", Encode.string "NotHovering" )
                ]



-- [generator-end]
