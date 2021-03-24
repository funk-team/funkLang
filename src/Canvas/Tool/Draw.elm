module Canvas.Tool.Draw exposing (..)

{-| Smart drawing facilities for funk

@@TODOS:

  - fix guides on scene not correct when camera is offset
  - rounding on labels

There are several scenarios possible when the user draws rectangles alone:

  - drawing on scene
  - drawing on an element
  - drawing around other elements, regardless of where
  - drawing across different elements, that means the start and end point are in different contexts

The complexities are

  - the rectangle should have a minimum size so that simple clicks are not drawing tiny elements (noise)
  - when the user draws across elements, we want to detect the correct parent (which would be the smaller element, as in more specific)
  - when the user draws around elements we want to detect the contained children and put them in the right place
  - the drawn element should only snap to the resulting parent and the resulting siblings, not the contained children

The result contains

  - the resulting shared properties of the parent
      - new children + absolute children list
  - the new element, as the appropriate rectangle, just to render
  - snaps to render

The data processing goes through the following steps
The Event Data 1 AND 2 will be processed in the following ways


# Input

@docs eventsOnElement, eventsOnScene, normalize
@docs Capture events.


# Normalized

@docs EventOnScene, EventOnElement, normalizeEventOnElement, normalizeEventOnScene
@docs Provide every relevant data point as absolute or element rectangle, no more piping stuff through several conversion functions in a row


# Deambiguate

@docs Normalized, Deambiguate
@docs aggreate two possible source contexts. For example: scene with element or element1 with elemen2


# Validate

@docs Combined, validate
@docs make sure the rectangle is large enough


# Snap

@docs Valid, snap
@docs make sure the rectangle is large enough


# Output

@docs Snapped, viewRect, viewSceneRect, viewElementRect
@docs including snaps

  - Preview OR Mutation
      - side effect

-}

import Canvas.Camera.Convert
import Canvas.Camera.Model
import Canvas.Events
import Canvas.Guides
import Canvas.Selection
import Canvas.Tool.AugmentationParams
import Canvas.Tool.Draw.Model exposing (..)
import Canvas.Tool.Draw.Msg exposing (..)
import Element
import Element.Background
import Element.Border
import Element.Font
import Json.Decode as Decode
import List.Extra
import Random
import Rectangle
import Renderer.InteractivityAttribs
import Renderer.StyleCompositor
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Spec.Mutation
import Ui.Help
import Ui.Style
import Viewport



---- DATA STRUCTURE ----


type DrawResult
    = DrawingTooSmall
    | NotDrawingAtThisMoment
    | DrawingOnSceneAccepted ScreenDrawResultData
    | DrawingOnElementAccepted ElementDrawResultData


{-| All the information about snapping, containments etc. as determined by the smart algorithm
The id and geometry are parameterizable so that we can use it for both itemsOnCanvas on elements and the scene
-}
type alias DrawResultData selectionItem geometry =
    -- can be applied as result and used to find / augment the right element
    { containments : Containments

    -- the element to draw during the operation
    , resultingRect : geometry

    -- optionally a selection item for mutations and parents to render stuff in.
    , selectionItem : selectionItem

    -- the snaps as found by the magnetic snapping algorithm
    , snapAlignments : List Canvas.Guides.Alignment

    -- the rectangles that were candidates for creating guides
    , snapSources : Spec.Element.Id.Dict Rectangle.Rectangle
    }


type alias ScreenDrawResultData =
    DrawResultData () Canvas.Events.AbsoluteRectangle


{-| The return value for each tool
-}
type alias Return state =
    { state : state
    , mutation : Maybe Spec.Mutation.Mutation
    , seed : Random.Seed
    , selection : Maybe Canvas.Selection.SelectionItem
    }



-- [generator-start]
---- INTERESTING CODE ----


rectangle_size_threshold =
    20


{-| Prevent noisy rectangles from being drawn.
For example, a user might just click the canvas.
In this case, a rectangle of size 0|0 is described through the mouse movement.
We assume that the user does not want to draw these close-to-invisible rectangles.
To prevent these very small rectangles from being create, we measure the size and check if it is beyond a specific threshold.
-}
validateSize : Canvas.Camera.Model.Model -> DrawResult -> DrawResult
validateSize camera r =
    let
        isVisible rect =
            let
                adjustedWidth =
                    Rectangle.width rect / camera.zoom

                adjustedHeight =
                    Rectangle.height rect / camera.zoom
            in
            adjustedHeight * adjustedWidth > rectangle_size_threshold
    in
    case r of
        DrawingOnSceneAccepted data ->
            let
                (Canvas.Events.AbsoluteRectangle resultingRect) =
                    data.resultingRect
            in
            if isVisible resultingRect then
                r

            else
                DrawingTooSmall

        DrawingOnElementAccepted data ->
            let
                (Canvas.Events.ElementRectangle resultingRect) =
                    data.snappedRect
            in
            if isVisible resultingRect then
                r

            else
                DrawingTooSmall

        DrawingTooSmall ->
            r

        NotDrawingAtThisMoment ->
            r


{-| Try to make sense of the mouse movements the user has made so far
-}
getDrawResult : Canvas.Camera.Model.Model -> DrawState -> DrawResult
getDrawResult camera state =
    validateSize camera <|
        case state of
            NotDrawing ->
                NotDrawingAtThisMoment

            Drawing from to ->
                case ( from, to ) of
                    -- when we stay on the scene
                    ( OnSceneEventData from_, OnSceneEventData to_ ) ->
                        getResultForOnSceneOnly camera from_ to_

                    ( OnElementEventData from_, OnElementEventData to_ ) ->
                        getResultForOnElement camera from_ to_

                    -- when we draw between scene and rectangle
                    ( OnSceneEventData from_, OnElementEventData to_ ) ->
                        getResultForBetweenElementAndScene camera from_ to_

                    ( OnElementEventData from_, OnSceneEventData to_ ) ->
                        getResultForBetweenElementAndScene camera to_ from_


{-| Describe which elements have been enclosed within a rectangle the user has drawn
-}
type alias ContainmentResult =
    { outside : List Spec.Element.Id.Id
    , contained : Containments
    }


{-| Detect whether elements are inside another element and calculate the diff
-}
detectContainment :
    List ( Spec.Element.Id.Id, Rectangle.Rectangle )
    -> Rectangle.Rectangle
    -> ContainmentResult
detectContainment siblings siblingThatPossiblyEnclosesOtherSiblings =
    let
        outside =
            siblings
                |> List.filterMap
                    (\( id, thisSiblingDimension ) ->
                        case Rectangle.contains siblingThatPossiblyEnclosesOtherSiblings thisSiblingDimension of
                            True ->
                                Nothing

                            False ->
                                Just id
                    )

        contained =
            siblings
                |> List.filterMap
                    (\( id, dimension ) ->
                        case Rectangle.contains siblingThatPossiblyEnclosesOtherSiblings dimension of
                            False ->
                                Nothing

                            True ->
                                Just
                                    ( id
                                    , Canvas.Camera.Convert.elementRectangleFromAbsoluteRectangle (Canvas.Events.AbsoluteRectangle siblingThatPossiblyEnclosesOtherSiblings) (Canvas.Events.AbsoluteRectangle dimension)
                                    )
                    )
    in
    { outside = outside
    , contained = contained
    }


{-| What happens when the user draws on the background of our canvas ?
-}
getResultForOnSceneOnly : Canvas.Camera.Model.Model -> OnSceneData -> OnSceneData -> DrawResult
getResultForOnSceneOnly camera from to =
    let
        rect =
            Canvas.Camera.Convert.sceneRectangleFromScenePoints from.point to.point

        { outside, contained } =
            detectContainment absSiblings resultRect

        absRect =
            Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle
                camera
                rect

        absSiblings =
            from.siblingsDimensions
                |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera))
                |> List.map (Tuple.mapSecond (\(Canvas.Events.AbsoluteRectangle r_) -> r_))

        snapSources : Spec.Element.Id.Dict Rectangle.Rectangle
        snapSources =
            absSiblings
                |> Spec.Element.Id.dictFromList

        ( Canvas.Events.AbsoluteRectangle resultRect, alignments ) =
            Canvas.Guides.snap
                camera
                snapSources
                absRect
    in
    { resultingRect =
        Canvas.Events.AbsoluteRectangle resultRect
    , containments = contained
    , snapSources = snapSources
    , snapAlignments = alignments
    , selectionItem = ()
    }
        |> DrawingOnSceneAccepted


{-| What happens when the user draws from the Scene and the rectangle overlaps another element
-}
getResultForBetweenElementAndScene : Canvas.Camera.Model.Model -> OnSceneData -> OnElementData -> DrawResult
getResultForBetweenElementAndScene camera sceneData elementData =
    let
        elRect : Canvas.Events.AbsoluteRectangle
        elRect =
            elementData.point.context
                |> Tuple.second
                |> Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera

        elPointFromScenePoint : Canvas.Events.ElementPoint
        elPointFromScenePoint =
            sceneData.point
                |> Canvas.Camera.Convert.elementPointFromScenePoint camera (Tuple.second elementData.point.context)
    in
    getResultForOnElementHelp camera elementData elPointFromScenePoint


{-| When the rectangle the user draws starts and ends on the same or two different rectangles
-}
getResultForOnElement :
    Canvas.Camera.Model.Model
    -> OnElementData
    -> OnElementData
    -> DrawResult
getResultForOnElement camera from to =
    let
        startedAndEndedOnSameElement =
            (Tuple.second from.sourceElement).shared.id
                == (Tuple.second to.sourceElement).shared.id

        ( parentData, pointRelativeToParent ) =
            if startedAndEndedOnSameElement then
                ( from, to.point.point )

            else
                resolveParentAmbiguities camera from to
    in
    getResultForOnElementHelp
        camera
        parentData
        pointRelativeToParent


{-| Process disambiguated element data
-}
getResultForOnElementHelp :
    Canvas.Camera.Model.Model
    -> OnElementData
    -> Canvas.Events.ElementPoint
    -> DrawResult
getResultForOnElementHelp camera parentData toPoint =
    let
        parentAbs =
            parentData.point.context
                |> Tuple.second
                |> Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera

        relSiblings =
            parentData.siblingsDimensions
                |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera))
                |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.elementRectangleFromAbsoluteRectangle parentAbs))
                |> List.map (Tuple.mapSecond (\(Canvas.Events.ElementRectangle r_) -> r_))

        ( parentId, parentRect ) =
            parentData.point.context
                |> Tuple.mapSecond (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera)

        snapSources : Spec.Element.Id.Dict Rectangle.Rectangle
        snapSources =
            let
                (Canvas.Events.AbsoluteRectangle absParentRect) =
                    parentRect

                relParentRect =
                    Rectangle.moveToOrigin absParentRect

                parentRectWithId =
                    ( parentId, relParentRect )
            in
            parentRectWithId
                :: relSiblings
                |> Spec.Element.Id.dictFromList

        ( Canvas.Events.AbsoluteRectangle snappedRect, alignments ) =
            Canvas.Guides.snap
                camera
                snapSources
                (Canvas.Events.AbsoluteRectangle drawnRect)

        (Canvas.Events.ElementRectangle drawnRect) =
            Canvas.Camera.Convert.elementRectangleFromElementPoints
                parentData.point.point
                toPoint

        { contained, outside } =
            detectContainment
                relSiblings
                snappedRect

        flowToAbsolute : Spec.Element.Model.FlowElement -> Maybe Spec.Element.Model.AbsoluteElement
        flowToAbsolute el =
            relSiblings
                |> List.Extra.find (\( id, _ ) -> id == el.shared.id)
                |> Maybe.map
                    (\( _, relativeDimensions ) ->
                        { shared = el.shared
                        , outerGeometry =
                            Canvas.Events.ElementRectangle relativeDimensions
                                |> Spec.Element.createAbsoluteElementDimensions
                        }
                    )

        -- get the siblings' absolute position
        absoluteSiblingsBeforeAddition =
            case (Tuple.second parentData.sourceElement).shared.children of
                Spec.Element.Model.AbsoluteChildren children ->
                    children

                Spec.Element.Model.FlowChildren children ->
                    children
                        |> List.filterMap flowToAbsolute

        ( containedSiblings, notContainedSiblings ) =
            Spec.Mutation.processContainments contained absoluteSiblingsBeforeAddition
    in
    { snappedRect = Canvas.Events.ElementRectangle snappedRect
    , notContainedSiblings = notContainedSiblings
    , containedSiblings = containedSiblings

    -- @TODO - add containments
    , snapInfo =
        { sources =
            snapSources
        , alignments =
            alignments
        }
    , selectionItem = Tuple.first parentData.sourceElement
    }
        |> DrawingOnElementAccepted


{-| Find the parent based on some metrics if it's not clear and return the elementPoint of the non-root element relative to the determined root.
For now, we just always use the 'from' value
-}
resolveParentAmbiguities :
    Canvas.Camera.Model.Model
    -> OnElementData
    -> OnElementData
    -> ( OnElementData, Canvas.Events.ElementPoint )
resolveParentAmbiguities camera from to =
    let
        fromIsLarger =
            True

        -- do later
        fromSceneRect : Canvas.Events.SceneRectangle
        fromSceneRect =
            Tuple.second from.point.context

        toPointRelativeToFromRectangle : Canvas.Events.ElementPoint
        toPointRelativeToFromRectangle =
            to.point
                |> Canvas.Camera.Convert.scenePointFromElementPoint camera
                |> Canvas.Camera.Convert.elementPointFromScenePoint camera fromSceneRect
    in
    ( from, toPointRelativeToFromRectangle )


{-| Event handlers and drawing preview for new screen
-}
augmentScene : Canvas.Camera.Model.Model -> State -> List (Element.Attribute Msg)
augmentScene camera state =
    let
        onRelease_ =
            Decode.succeed Release
                |> Canvas.Events.onRelease
    in
    case state.mode of
        TextInput ->
            [ onRelease_ ]

        -- can't draw inputs on the canvas
        _ ->
            let
                onPress_ =
                    Decode.map2
                        (\siblingsDimensions point -> ScenePress { point = point, siblingsDimensions = siblingsDimensions })
                        Canvas.Events.readCanvasElements
                        Canvas.Events.readScenePoint
                        |> Canvas.Events.onPress

                -- @@TODO - propagate children
                onPressMove_ =
                    Decode.map2
                        (\siblingsDimensions point -> SceneMove { point = point, siblingsDimensions = siblingsDimensions })
                        Canvas.Events.readSiblingsInTarget
                        Canvas.Events.readScenePoint
                        |> Canvas.Events.onPressMove

                sceneAugmentations =
                    [ onPress_
                    , onPressMove_
                    , onRelease_
                    ]
            in
            sceneAugmentations


augmentCanvas : Canvas.Camera.Model.Model -> State -> List (Element.Attribute Msg)
augmentCanvas camera state =
    case getDrawResult camera state.drawState of
        DrawingOnSceneAccepted data ->
            previewCanvasRectangle camera data

        _ ->
            []


{-| Display guides etc. if the user is currently drawing in this element
-}
augmentElement :
    Canvas.Tool.AugmentationParams.AugmentationParams
    -> State
    -> List (Element.Attribute Msg)
augmentElement augmentationParams state =
    let
        preview =
            case getDrawResult augmentationParams.camera state.drawState of
                DrawingOnElementAccepted res ->
                    let
                        isOnThisElement =
                            (augmentationParams.selectionItem |> Canvas.Selection.getTargetId)
                                |> (==) (Canvas.Selection.getTargetId res.selectionItem)
                    in
                    if isOnThisElement then
                        previewElementRectangle augmentationParams.camera res

                    else
                        []

                _ ->
                    []

        onPress =
            Decode.map2
                (\siblingsDimensions elementPointInContext ->
                    { sourceElement = ( augmentationParams.selectionItem, augmentationParams.element )
                    , siblingsDimensions = siblingsDimensions
                    , point = elementPointInContext
                    }
                        |> ElementPress
                )
                Canvas.Events.readAllElements
                Canvas.Events.readPointInContext
                |> Canvas.Events.onPress

        onPressMove =
            Decode.map2
                (\siblingsDimensions elementPointInContext ->
                    { sourceElement = ( augmentationParams.selectionItem, augmentationParams.element )
                    , siblingsDimensions = siblingsDimensions
                    , point = elementPointInContext
                    }
                        |> ElementMove
                )
                Canvas.Events.readAllElements
                Canvas.Events.readPointInContext
                |> Canvas.Events.onPressMove

        onRelease =
            Decode.succeed Release
                |> Canvas.Events.onRelease
    in
    preview
        ++ [ onPress
           , onPressMove
           , onRelease
           ]


{-| Show HUD with size and width of the newly drawn rectangle
-}
measurements : Canvas.Camera.Model.Model -> Rectangle.Rectangle -> List (Element.Attribute msg)
measurements camera rect =
    let
        strokeWidth =
            Element.px (1 / camera.zoom |> round)

        background =
            Element.Background.color Ui.Style.highlightColorSolid

        yAxisLHS =
            Element.el
                [ Element.width strokeWidth
                , Element.height Element.fill
                , background
                , Ui.Help.noPointerEvents
                , Element.moveRight (Rectangle.width rect)
                ]
                Element.none

        yAxisRHS =
            Element.el
                [ Element.width strokeWidth
                , Element.height Element.fill
                , background
                , Ui.Help.noPointerEvents
                ]
                Element.none

        xAxisTop =
            Element.el
                [ Element.height strokeWidth
                , Element.width Element.fill
                , background
                , Ui.Help.noPointerEvents
                ]
                Element.none

        xAxisBottom =
            Element.el
                [ Element.height strokeWidth
                , Element.width Element.fill
                , background
                , Element.moveDown (Rectangle.height rect)
                , Element.below
                    (Element.el
                        ([ Element.centerX
                         , Element.moveDown (10 / camera.zoom)
                         , Ui.Help.noPointerEvents
                         , Element.scale (1 / camera.zoom)
                         , Element.Font.color Ui.Style.white
                         , Element.Font.size 16
                         , Element.padding 5
                         , Element.Border.rounded 3
                         , Element.Background.color Ui.Style.highlightColorSolid
                         ]
                            ++ Ui.Help.initFontStyles
                        )
                        (Element.text
                            (String.fromInt (Rectangle.width rect |> round)
                                ++ " x "
                                ++ String.fromInt (Rectangle.height rect |> round)
                            )
                        )
                    )
                , Ui.Help.noPointerEvents
                ]
                Element.none
    in
    [ xAxisTop
    , yAxisRHS
    , yAxisLHS
    , xAxisBottom
    ]
        |> List.map Element.inFront


borderStyles : Float -> List (Element.Attribute msg)
borderStyles zoom =
    [ Element.Border.color Ui.Style.highlightColor
    , Element.Border.dashed
    , Element.Border.width (1 / zoom |> round)
    , Ui.Help.noPointerEvents
    ]


drawingGuidesY : Canvas.Camera.Model.Model -> Rectangle.Rectangle -> List (Element.Attribute msg)
drawingGuidesY camera rect =
    let
        strokeWidth =
            Element.px (1 / camera.zoom |> round)

        yAxis =
            Element.el
                ([ Element.width strokeWidth
                 , Element.height (Element.px 2000)
                 , Element.moveRight (Rectangle.width rect / 2)
                 ]
                    ++ borderStyles camera.zoom
                )
                Element.none
    in
    [ yAxis ]
        |> List.map Element.above


drawingGuidesX : Canvas.Camera.Model.Model -> Rectangle.Rectangle -> List (Element.Attribute msg)
drawingGuidesX camera rect =
    let
        strokeWidth =
            Element.px (1 / camera.zoom |> round)

        xAxis =
            Element.el
                ([ Element.height strokeWidth
                 , Element.width (Element.px 2000)
                 , Element.moveDown (Rectangle.height rect / 2)
                 ]
                    ++ borderStyles camera.zoom
                )
                Element.none
    in
    [ xAxis ]
        |> List.map Element.onLeft


{-| Preview a rectangle drawn on an element
-}
previewElementRectangle : Canvas.Camera.Model.Model -> ElementDrawResultData -> List (Element.Attribute Msg)
previewElementRectangle camera res =
    let
        styles =
            Renderer.StyleCompositor.render Renderer.InteractivityAttribs.schematicStyle

        (Canvas.Events.ElementRectangle elementRect) =
            res.snappedRect

        drawnRectangleDimensions =
            Rectangle.render elementRect

        guides =
            Canvas.Guides.viewReasons camera res.snapInfo.sources res.snapInfo.alignments
                |> List.map Element.inFront

        attribs =
            Ui.Help.noPointerEvents
                :: drawnRectangleDimensions
                ++ styles
                ++ drawingGuidesX camera elementRect
                ++ drawingGuidesY camera elementRect
                ++ measurements camera elementRect

        el =
            Element.el
                attribs
                Element.none
                |> Element.inFront
    in
    el :: guides


previewCanvasRectangle :
    Canvas.Camera.Model.Model
    -> ScreenDrawResultData
    -> List (Element.Attribute Msg)
previewCanvasRectangle camera drawResult =
    let
        styles =
            Renderer.StyleCompositor.render Renderer.InteractivityAttribs.schematicStyle

        (Canvas.Events.AbsoluteRectangle absRect) =
            drawResult.resultingRect

        siblingsDimensions =
            Rectangle.render absRect

        guides =
            Canvas.Guides.viewReasons camera drawResult.snapSources drawResult.snapAlignments

        attribs =
            Ui.Help.noPointerEvents
                :: siblingsDimensions
                ++ styles
                ++ drawingGuidesX camera absRect
                ++ drawingGuidesY camera absRect
                ++ measurements camera absRect
                ++ visualizePresets absRect camera.zoom

        el =
            Element.el
                attribs
                Element.none
    in
    el
        :: guides
        |> List.map Element.inFront


{-| Preview screen size presets
-}
visualizePresets rect zoom =
    let
        -- smallest distances first
        distanceDeviceOrderedList : List ( Float, Viewport.Device )
        distanceDeviceOrderedList =
            Viewport.presets
                |> List.map
                    (\preset ->
                        let
                            device =
                                Viewport.presetToDevice preset

                            distance =
                                evaluateDistance rect device
                        in
                        ( distance, device )
                    )
                |> List.sortBy Tuple.first

        isolateStickyDevice :
            List ( Float, Viewport.Device )
            -> ( Maybe ( Float, Viewport.Device ), List ( Float, Viewport.Device ) )
        isolateStickyDevice list =
            case list of
                head :: queue ->
                    case passesThreshold head of
                        Just stickyDevice ->
                            ( Just head
                            , queue
                            )

                        Nothing ->
                            ( Nothing
                            , list
                            )

                [] ->
                    ( Nothing, [] )

        ( maybeStickyDevice, otherDevices ) =
            distanceDeviceOrderedList |> isolateStickyDevice

        pushMaybeIntoList maybe list =
            case maybe of
                Just a ->
                    a :: list

                Nothing ->
                    list
    in
    pushMaybeIntoList
        (Maybe.map (visualizePreset True zoom) maybeStickyDevice)
        (List.map (visualizePreset False zoom) otherDevices)


visualizePreset : Bool -> Float -> ( Float, Viewport.Device ) -> Element.Attribute msg
visualizePreset isSticky zoom ( distance, device ) =
    let
        counterZoom v =
            v / zoom |> round

        nameAttr =
            Element.el
                [ Element.Font.color color ]
                (Element.text device.name)
                |> belowRight

        belowRight elem =
            Element.el
                [ Element.width (Element.px device.width)
                , Ui.Help.noPointerEvents
                ]
                (Element.el
                    [ Element.alignRight
                    , Element.Font.size (counterZoom 16)
                    , Element.paddingEach { edges | top = counterZoom 3 }
                    ]
                    elem
                )
                |> Element.below

        borderWidth =
            if isSticky then
                3

            else
                1

        borderWidthStr =
            (borderWidth / zoom |> round)
                |> String.fromInt

        color =
            Element.rgba 1 0 0 alpha

        colorStr =
            "rgba(255, 0, 0, " ++ String.fromFloat alpha ++ ")"

        {- to refine the behaviour you can play around with m and p

           m will make the preset aprear and desapear faster when bigger and
           p will move the threshold before which the preset is visible.
           p = 0 would mean all the presets are visible and p = 1 means none are
        -}
        ( m, p ) =
            ( 30, 0.3 )

        alpha =
            sigmoid (m * ((1 / distance) - p))

        sigmoid x =
            1 / (1 + (e ^ -x))
    in
    Element.el
        [ Element.width (Element.px device.width)
        , Element.height (Element.px device.height)
        , Ui.Style.style "outline"
            (colorStr ++ " solid " ++ borderWidthStr ++ "px")
        , Ui.Help.noPointerEvents
        , nameAttr
        ]
        Element.none
        |> Element.inFront


{-| Based on data returnde by the smart algorithm we build a mutation etc.
-}
makeReturnForScreen :
    Random.Seed
    -> ScreenDrawResultData
    -> Return DrawState
makeReturnForScreen seed { containments, resultingRect } =
    let
        ( id, newSeed ) =
            Spec.Element.Id.random seed

        mutation =
            let
                (Canvas.Events.AbsoluteRectangle rect) =
                    resultingRect
            in
            Spec.Mutation.AddElementToScene
                { containments = containments
                , id = id
                , resultingRect = screenSize rect
                }
                |> Just

        --@@TODO need to pass down the screenSize through the mutation Msg
        screenSize rect =
            case
                closestDevice rect
                    |> Maybe.andThen passesThreshold
            of
                Nothing ->
                    Spec.Element.Model.Custom (Canvas.Events.AbsoluteRectangle rect)

                Just device ->
                    let
                        offset =
                            { x = Rectangle.x1 rect
                            , y = Rectangle.y1 rect
                            }
                    in
                    Spec.Element.Model.Preset device offset
    in
    { state = NotDrawing
    , mutation = mutation
    , seed = newSeed
    , selection = Just <| Canvas.Selection.SelectedRoot id
    }


passesThreshold : ( Float, Viewport.Device ) -> Maybe Viewport.Device
passesThreshold ( distance, device ) =
    let
        {- to refine the behaviour you can play around with the threshold

           The largest the threshold is the further away the screen will stick to the preset
        -}
        threshold =
            2
    in
    if distance < threshold then
        Just device

    else
        Nothing


closestDevice : Rectangle.Rectangle -> Maybe ( Float, Viewport.Device )
closestDevice rect =
    Viewport.presets
        |> List.map
            (\preset ->
                let
                    device =
                        Viewport.presetToDevice preset
                in
                ( evaluateDistance rect device
                , device
                )
            )
        |> List.Extra.minimumBy Tuple.first


evaluateDistance : Rectangle.Rectangle -> Viewport.Device -> Float
evaluateDistance draw device =
    let
        distance =
            sumOfSquares / scale

        -- we want the distance to increase exponentialy as the delta grows
        sumOfSquares =
            sqrt deltaWidth + sqrt deltaHeight

        -- the bigger the drawn element the biggest the authorised delta
        scale =
            logBase e longestDrawnSide

        ---- helpers ----
        drawWidth =
            Rectangle.width draw

        drawHeight =
            Rectangle.height draw

        deviceWidth =
            toFloat device.width

        deviceHeight =
            toFloat device.height

        deltaWidth =
            abs (drawWidth - deviceWidth)

        deltaHeight =
            abs (drawHeight - deviceHeight)

        longestDrawnSide =
            max drawWidth drawHeight
    in
    distance


{-| Based on data returnde by the smart algorithm we build a mutation etc.
-}
makeReturnForOnElement :
    Random.Seed
    -> Mode
    -> ElementDrawResultData
    -> Return DrawState
makeReturnForOnElement seed mode drawResultData =
    let
        ( id, newSeed ) =
            Spec.Element.Id.random seed

        mutation : Maybe Spec.Mutation.Mutation
        mutation =
            Spec.Mutation.CompleteDraw { id = id, mode = mode, drawResultData = drawResultData }
                |> Just
    in
    { state = NotDrawing
    , mutation = mutation
    , seed = newSeed
    , selection = Just <| Canvas.Selection.addOne id drawResultData.selectionItem
    }


wrapReturn : State -> Return DrawState -> Return State
wrapReturn toolState { state, mutation, seed, selection } =
    { state = { toolState | drawState = state }
    , mutation = mutation
    , seed = seed
    , selection = selection
    }


update :
    Random.Seed
    -> Canvas.Camera.Model.Model
    -> Msg
    -> State
    -> Return State
update seed camera msg state =
    let
        s =
            state.drawState
    in
    wrapReturn state <|
        case msg of
            -- initialize drawing process with either
            ElementPress rawOnElementData ->
                { state = Drawing (OnElementEventData rawOnElementData) (OnElementEventData rawOnElementData)
                , mutation = Nothing
                , seed = seed
                , selection = Nothing
                }

            ScenePress scenePoint ->
                { state = Drawing (OnSceneEventData scenePoint) (OnSceneEventData scenePoint)
                , mutation = Nothing
                , seed = seed
                , selection = Nothing
                }

            -- move on either
            SceneMove to ->
                case s of
                    NotDrawing ->
                        { state = NotDrawing
                        , mutation = Nothing
                        , seed = seed
                        , selection = Nothing
                        }

                    Drawing from _ ->
                        { state = Drawing from (OnSceneEventData to)
                        , mutation = Nothing
                        , seed = seed
                        , selection = Nothing
                        }

            -- move on either
            ElementMove to ->
                case s of
                    NotDrawing ->
                        { state = NotDrawing
                        , mutation = Nothing
                        , seed = seed
                        , selection = Nothing
                        }

                    Drawing from _ ->
                        { state = Drawing from (OnElementEventData to)
                        , mutation = Nothing
                        , seed = seed
                        , selection = Nothing
                        }

            Release ->
                case getDrawResult camera s of
                    DrawingOnSceneAccepted data ->
                        makeReturnForScreen seed data

                    DrawingOnElementAccepted data ->
                        makeReturnForOnElement seed state.mode data

                    DrawingTooSmall ->
                        { state = NotDrawing
                        , mutation = Nothing
                        , seed = seed
                        , selection = Nothing
                        }

                    NotDrawingAtThisMoment ->
                        { state = NotDrawing
                        , mutation = Nothing
                        , seed = seed
                        , selection = Nothing
                        }


edges =
    { top = 0, right = 0, left = 0, bottom = 0 }
