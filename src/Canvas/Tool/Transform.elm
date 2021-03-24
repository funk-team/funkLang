module Canvas.Tool.Transform exposing (..)

{-| Complexities of highlighting and transforming elements

  - screens vs regular elements
  - snapping
  - ensuring geometry behaves sane

-}

import BoundingClientRectangle
import Canvas.Camera
import Canvas.Camera.Convert
import Canvas.Camera.Model
import Canvas.Events
import Canvas.Guides
import Canvas.Selection
import Canvas.Tool.AugmentationParams
import Canvas.Tool.Transform.Handles
import Canvas.Tool.Transform.Model
import Canvas.Tool.Transform.Msg
import Compass
import Element
import Element.Events.Extra
import Interface.Data
import Interface.Model
import Interface.Scope
import Json.Decode as Decode
import Keyboard
import List.Extra
import Model.Model
import Rectangle
import Renderer.Help
import Spec
import Spec.Element
import Spec.Element.Id
import Spec.Element.Layout
import Spec.Element.Layout.Length
import Spec.Element.Model
import Spec.Mutation
import Ui.Help
import Ui.Style


{-| Attach events, handles and pointer-events declarations to elements
-}
augmentEitherElement :
    Model.Model.UserModel
    -> Maybe Spec.Element.Model.EitherElement
    -> Canvas.Tool.AugmentationParams.AugmentationParams
    -> Canvas.Tool.Transform.Model.State
    ->
        { other : List (Element.Attribute Canvas.Tool.Transform.Msg.Msg)
        , layout : List (Element.Attribute Canvas.Tool.Transform.Msg.Msg)
        , element : Spec.Element.Model.EitherElement
        }
augmentEitherElement userModel parent_ augmentationParams state =
    let
        augmentElement_ =
            augmentElement
                userModel
                augmentationParams
                state

        noOp =
            { layout = []
            , other = [ Element.inFront (Element.text "ERROR: This should have a parent element in its context") ]
            , element = augmentationParams.element
            }
    in
    case ( parent_, augmentationParams.element.outerGeometry ) of
        ( _, Spec.Element.Model.ScreenGeometry geo ) ->
            augmentScreen
                userModel
                state
                augmentationParams
                { shared = augmentationParams.element.shared, outerGeometry = geo }
                |> (\{ layout, other, element } ->
                        { layout = layout
                        , other = other
                        , element = element
                        }
                   )

        ( Just parent, Spec.Element.Model.AbsoluteElementGeometry geo ) ->
            augmentElement_
                parent
                ({ shared = augmentationParams.element.shared, outerGeometry = geo }
                    |> Spec.Element.wrapAbsolute
                )

        ( Just parent, Spec.Element.Model.FlowElementGeometry geo ) ->
            augmentElement_
                parent
                ({ shared = augmentationParams.element.shared, outerGeometry = geo }
                    |> Spec.Element.wrapFlow
                )

        ( Nothing, _ ) ->
            noOp


type Relation
    = Sibling
    | Parent
    | Selected
    | Child
    | NoRelation


isDragTarget : Canvas.Tool.Transform.Model.State -> Canvas.Tool.AugmentationParams.AugmentationParams -> Bool
isDragTarget state augmentationParams =
    case state of
        Canvas.Tool.Transform.Model.TransformingScreen { handleLocation } (Canvas.Tool.Transform.Model.ToElement { selectionItem }) ->
            selectionItem
                == augmentationParams.selectionItem
                && (case handleLocation of
                        Canvas.Tool.Transform.Model.SomewhereOnElement _ ->
                            True

                        Canvas.Tool.Transform.Model.CompassDirection Compass.Center ->
                            True

                        _ ->
                            False
                   )

        Canvas.Tool.Transform.Model.TransformingElement { handleLocation } (Canvas.Tool.Transform.Model.ToElement { selectionItem }) ->
            selectionItem
                == augmentationParams.selectionItem
                && (case handleLocation of
                        Canvas.Tool.Transform.Model.SomewhereOnElement _ ->
                            True

                        Canvas.Tool.Transform.Model.CompassDirection Compass.Center ->
                            True

                        _ ->
                            False
                   )

        _ ->
            False


highlightDragTarget state augmentationParams =
    case isDragTarget state augmentationParams of
        True ->
            let
                outlineSize =
                    2
                        / augmentationParams.camera.zoom
                        |> String.fromFloat
            in
            [ Ui.Style.style "outline" (outlineSize ++ "px dashed var(--highlight-solid)") ]

        False ->
            []


{-| Attach events, handles and pointer-events declarations to elements

Element is there twice in the parameters because it was wrapped before.

This function affects

  - parent element: prevents collaps and changes highlight color when children are flow elemnt
  - sibling elements: highlights when in same flow
  - selected element: show handles

-}
augmentElement :
    Model.Model.UserModel
    -> Canvas.Tool.AugmentationParams.AugmentationParams
    -> Canvas.Tool.Transform.Model.State
    -> Spec.Element.Model.EitherElement
    -> Spec.Element.Model.EitherElement
    -> { other : List (Element.Attribute Canvas.Tool.Transform.Msg.Msg), layout : List (Element.Attribute Canvas.Tool.Transform.Msg.Msg), element : Spec.Element.Model.EitherElement }
augmentElement userModel augmentationParams state parent element =
    let
        -- children need to be augmentd if the child of an element happens to be transformed
        -- if so
        -- [ ] all the children need to be converted into absolute children and their respective positions need to be applied
        -- [ ] the parent's dimensions need to be set to absolute
        { camera } =
            augmentationParams

        isSelected : Bool
        isSelected =
            augmentationParams.selection == Just augmentationParams.selectionItem

        -- is sibling of selected if the parent of this item is the same as the one in the selection
        isSiblingOfSelected : Bool
        isSiblingOfSelected =
            Maybe.andThen Canvas.Selection.removeOne augmentationParams.selection
                == Canvas.Selection.removeOne augmentationParams.selectionItem

        -- the current element is the parent of the selected element if we remove one element from the selection and get to this element
        isParentOfSelected : Bool
        isParentOfSelected =
            Maybe.andThen
                Canvas.Selection.removeOne
                augmentationParams.selection
                == (augmentationParams.selectionItem |> Just)

        -- when something is being transformed, any element can be dragged into
        isTransformingSomething =
            state /= Canvas.Tool.Transform.Model.NotTransforming

        -- it's a child if the currently selected id is within the this element's parent path
        isChild =
            case ( augmentationParams.selection, Canvas.Selection.removeOne augmentationParams.selectionItem ) of
                ( Just selection, Just parentSelection ) ->
                    List.member (Canvas.Selection.getTargetId selection) (Canvas.Selection.toPath parentSelection)

                _ ->
                    False

        relation : Relation
        relation =
            if isSelected then
                Selected

            else if isSiblingOfSelected then
                Sibling

            else if isParentOfSelected then
                Parent

            else if isChild then
                Child

            else
                NoRelation

        bakeData :
            ( List ( Spec.Element.Id.Id, Canvas.Events.SceneRectangle ), HandleAndBoundingBox )
            -> Canvas.Tool.Transform.Model.MouseDownDataOnElement
        bakeData ( siblingsDimensions, handleInfo ) =
            { handleLocation = handleInfo.handle
            , originalElementGeometry = handleInfo.elementBoundingBox -- actually this element
            , parentGeometry = handleInfo.parentBoundingBox
            , element = element
            , selectionItem = augmentationParams.selectionItem
            , siblingsDimensions = siblingsDimensions
            , parentElement = parent
            }

        decodeMouseDown :
            Decode.Decoder Canvas.Tool.Transform.Model.MouseDownLocation
            -> Decode.Decoder ( List ( Spec.Element.Id.Id, Canvas.Events.SceneRectangle ), HandleAndBoundingBox )
        decodeMouseDown startLocationDecoder =
            Decode.map2 Tuple.pair
                Canvas.Events.readAllElements
                (decodeElementBoundingBoxForHandle startLocationDecoder)

        handles : Bool -> List (Element.Attribute Canvas.Tool.Transform.Msg.Msg)
        handles passThrough =
            let
                compassHandles =
                    (\handle -> Decode.succeed (Canvas.Tool.Transform.Model.CompassDirection handle) |> decodeMouseDown)
                        |> Canvas.Tool.Transform.Handles.handles augmentationParams.camera passThrough

                decodePosition =
                    Canvas.Events.readAbsolutePoint
                        |> Decode.map (Canvas.Camera.Convert.absolutePointFromElementPointSimple camera)
                        |> Decode.map Canvas.Tool.Transform.Model.SomewhereOnElement
            in
            fullHandle (decodeMouseDown decodePosition) passThrough
                :: compassHandles
                |> List.map (Element.mapAttribute (bakeData >> Canvas.Tool.Transform.Msg.MouseDownOnElement))

        id : Spec.Element.Id.Id
        id =
            element.shared.id

        {-
           give different types of children different highlight colors
        -}
        childrenColorCue =
            case ( relation == Parent, element.shared.children ) of
                ( True, Spec.Element.Model.FlowChildren _ ) ->
                    [ Ui.Style.setHighlightToSecondary ]

                _ ->
                    []

        -- highlight siblings if one of them is selected and they are in the same flow
        siblingHighlight =
            case ( relation == Sibling, element.outerGeometry ) of
                ( True, Spec.Element.Model.FlowElementGeometry _ ) ->
                    [ Canvas.Tool.Transform.Handles.lightOutline augmentationParams.camera ]

                _ ->
                    []

        canHaveChildren =
            case element.shared.kind of
                Spec.Element.Model.TextInput _ ->
                    False

                _ ->
                    case Renderer.Help.getContent id userModel augmentationParams.scope |> Maybe.map .value of
                        Just (Interface.Data.Media { kind }) ->
                            case kind of
                                Interface.Data.Image ->
                                    True

                                _ ->
                                    False

                        Nothing ->
                            True

                        -- other content types
                        _ ->
                            False

        {- If the element is selected, render handles
           If its a sibling render the outline
           If we are transforming, do not catch any events on that element
        -}
        maybeEvents =
            case ( relation, isTransformingSomething ) of
                -- when no action is going on, allow selecting
                ( _, False ) ->
                    [ Canvas.Events.onPress
                        (Decode.succeed (Canvas.Tool.Transform.Msg.Select augmentationParams.selectionItem))
                    ]

                -- the current element can not receive events
                ( Child, True ) ->
                    [ Ui.Help.noPointerEvents ]

                -- nor the children
                ( Selected, True ) ->
                    [ Ui.Help.noPointerEvents ]

                -- everything else is accepted as target
                ( _, True ) ->
                    -- if it does not contain any content
                    if canHaveChildren then
                        [ onPressMove (readElementTarget camera augmentationParams.selectionItem element) ]

                    else
                        [ Ui.Help.noPointerEvents ]

        maybeHighlights =
            siblingHighlight
                ++ childrenColorCue

        maybeHandles =
            if relation == Selected then
                handles isTransformingSomething

            else
                []

        -- the transform tool needs to modify both the transformed element as well as the parent
        -- because the parent geometry needs to go to "absolute" to prevent collapsing
        childTransformationData =
            case state of
                Canvas.Tool.Transform.Model.TransformingElement mouseDownData point ->
                    Just ( mouseDownData, point )

                _ ->
                    Nothing

        return =
            { other = maybeHandles ++ maybeEvents ++ maybeHighlights ++ highlightDragTarget state augmentationParams
            , layout = []
            , element = element
            }
    in
    case childTransformationData of
        Nothing ->
            return

        Just ( mouseDownData, point ) ->
            let
                parentAbs =
                    mouseDownData.parentGeometry
                        |> Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera

                relSiblings =
                    mouseDownData.siblingsDimensions
                        |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera))
                        |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.elementRectangleFromAbsoluteRectangle parentAbs))
                        |> List.map (Tuple.mapSecond (\(Canvas.Events.ElementRectangle r_) -> r_))

                -- convert a flow element to an absolute element useing the values read from the dom
                flowToAbsolute : Spec.Element.Model.EitherElement -> Maybe Spec.Element.Model.AbsoluteElement
                flowToAbsolute el_ =
                    relSiblings
                        |> List.Extra.find (\( siblingId, _ ) -> siblingId == el_.shared.id)
                        |> Maybe.map
                            (\( _, relativeDimensions ) ->
                                { shared = el_.shared
                                , outerGeometry =
                                    Canvas.Events.ElementRectangle relativeDimensions
                                        |> Spec.Element.createAbsoluteElementDimensions
                                }
                            )
            in
            case relation of
                Parent ->
                    let
                        siblings =
                            Spec.Element.getChildren mouseDownData.parentElement
                                |> List.filterMap flowToAbsolute
                    in
                    { return
                        | element =
                            Spec.Element.setChildren
                                (Spec.Element.Model.AbsoluteChildren siblings)
                                element
                                |> preventCollapse userModel augmentationParams.scope parentAbs
                    }

                Selected ->
                    let
                        transformPoint =
                            normalizeTargetPoint camera point
                    in
                    case flowToAbsolute mouseDownData.element of
                        Nothing ->
                            return

                        Just transformTarget ->
                            let
                                snapSources : Spec.Element.Id.Dict Rectangle.Rectangle
                                snapSources =
                                    relSiblings
                                        -- we do not want the currently transformed element
                                        |> List.filter (\( siblingId, _ ) -> not <| siblingId == mouseDownData.element.shared.id)
                                        |> Spec.Element.Id.dictFromList

                                adjustedSnapSources =
                                    snapSources
                                        |> Spec.Element.Id.dictToList
                                        |> List.map (\( id_, snapSourceRectangle ) -> ( id_, Rectangle.move ( -(Rectangle.x1 rawSnappedRect), -(Rectangle.y1 rawSnappedRect) ) snapSourceRectangle ))
                                        |> Spec.Element.Id.dictFromList

                                guides =
                                    Canvas.Guides.viewReasons camera adjustedSnapSources alignments
                                        |> List.map Element.inFront

                                (Canvas.Events.AbsoluteRectangle rawSnappedRect) =
                                    snappedRect

                                ( snappedRect, alignments ) =
                                    Canvas.Guides.snap
                                        camera
                                        snapSources
                                        (Canvas.Events.AbsoluteRectangle transformedRect)

                                layoutAugmentations =
                                    [ Element.width
                                        (Element.px <| round (Rectangle.width rawSnappedRect))
                                    , Element.height
                                        (Element.px <| round (Rectangle.height rawSnappedRect))
                                    , Element.moveRight
                                        (Rectangle.x1 rawSnappedRect)
                                    , Element.moveDown
                                        (Rectangle.y1 rawSnappedRect)
                                    ]

                                context =
                                    Interface.Scope.populateForElement augmentationParams.selectionItem userModel

                                transformTargetSize =
                                    transformTarget.outerGeometry
                                        |> Spec.getAbsoluteElementRectangle userModel context
                                        |> Canvas.Camera.Convert.absoluteRectangleFromElementRectangle parentAbs

                                (Canvas.Events.ElementRectangle transformedRect) =
                                    transformRect
                                        { startLocation = mouseDownData.handleLocation
                                        , currentLocation = transformPoint
                                        , rect = transformTargetSize
                                        , constrainToOneAxis = shouldConstrainToAxis augmentationParams.pressedKeys
                                        }
                                        |> Canvas.Camera.Convert.elementRectangleFromAbsoluteRectangle parentAbs

                                selectedElementEnhancements =
                                    { return
                                        | layout = layoutAugmentations
                                        , other = return.other ++ guides
                                    }
                            in
                            selectedElementEnhancements

                _ ->
                    return


normalizeTargetPoint camera point =
    case point of
        Canvas.Tool.Transform.Model.ToScene point_ ->
            Canvas.Camera.Convert.absolutePointFromScenePoint
                camera
                point_

        Canvas.Tool.Transform.Model.ToElement data ->
            Canvas.Camera.Convert.absolutePointFromElementPointAndParentRectangle
                data.targetAbs
                data.point


readElementTarget :
    Canvas.Camera.Model.Model
    -> Canvas.Selection.SelectionItem
    -> Spec.Element.Model.EitherElement
    -> Decode.Decoder Canvas.Tool.Transform.Msg.Msg
readElementTarget camera selectionItem element =
    let
        help ( elementPoint, sceneRectangle ) children =
            let
                relChildren =
                    children
                        |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera))
                        |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.elementRectangleFromAbsoluteRectangle targetAbs))

                targetAbs =
                    Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle
                        camera
                        sceneRectangle
            in
            Canvas.Tool.Transform.Model.ElementTarget
                selectionItem
                element
                elementPoint
                targetAbs
                relChildren
                |> Canvas.Tool.Transform.Model.ToElement
                |> Canvas.Tool.Transform.Msg.MouseDrag
    in
    Decode.map2 help
        Canvas.Events.readAbsolutePoint
        Canvas.Events.readSiblingsInTarget



-- when a responsified element has height: shrink and the children suddely are placed
-- absolotely, it will collaps to zero


preventCollapse userModel scope parentAbs parentElement =
    { parentElement
        | outerGeometry =
            case parentElement.outerGeometry of
                Spec.Element.Model.FlowElementGeometry sizeAndAlignment ->
                    -- if it's fill, leave it fill
                    { sizeAndAlignment
                        | size = fixSize userModel scope parentAbs sizeAndAlignment.size
                    }
                        |> Spec.Element.Model.FlowElementGeometry

                _ ->
                    parentElement.outerGeometry
    }


{-| This function ensurse that a rectangles min / max values do not contradict each other
-}
fixSize :
    Model.Model.UserModel
    -> Interface.Model.ScopeData
    -> Canvas.Events.AbsoluteRectangle
    -> Spec.Element.Layout.Size
    -> Spec.Element.Layout.Size
fixSize userModel scope (Canvas.Events.AbsoluteRectangle renderedSize) { width, height } =
    let
        help : (Rectangle.Rectangle -> Float) -> Spec.Element.Layout.Length -> Spec.Element.Layout.Length
        help rectToLength dynamicLength =
            case dynamicLength.behavior of
                Spec.Element.Layout.Static _ ->
                    dynamicLength

                Spec.Element.Layout.Fill ->
                    dynamicLength

                Spec.Element.Layout.Shrink ->
                    let
                        minMax =
                            let
                                b =
                                    dynamicLength.minMax

                                min =
                                    case Spec.resolveNullable userModel scope dynamicLength.minMax.min of
                                        Just minLength ->
                                            if minLength > rectToLength renderedSize then
                                                Spec.Element.Layout.Length.initNullable minLength

                                            else
                                                Spec.Element.Layout.Length.initNullable (rectToLength renderedSize)

                                        Nothing ->
                                            Spec.Element.Layout.Length.initNullable (rectToLength renderedSize)
                            in
                            { b | min = min }
                    in
                    { dynamicLength | minMax = minMax }
    in
    { width = help Rectangle.width width
    , height = help Rectangle.height height
    }



-- this one allows the user to grab the entire element


fullHandle decoder passThrough =
    Element.el
        [ if passThrough then
            Ui.Help.noPointerEvents

          else
            Ui.Help.allPointerEvents
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.Events.Extra.onMouseDown decoder
        ]
        Element.none
        |> Element.behindContent


{-| Attach events, handles and pointer-events declarations to elements
-}
augmentScreen :
    Model.Model.UserModel
    -> Canvas.Tool.Transform.Model.State
    -> Canvas.Tool.AugmentationParams.AugmentationParams
    -> Spec.Element.Model.Screen
    -> { other : List (Element.Attribute Canvas.Tool.Transform.Msg.Msg), layout : List (Element.Attribute Canvas.Tool.Transform.Msg.Msg), element : Spec.Element.Model.EitherElement }
augmentScreen userModel state ({ camera, selection, selectionItem } as augmentationParams) element =
    let
        relation =
            if isParentOfSelected then
                Parent

            else if isSelected then
                Selected

            else
                NoRelation

        isParentOfSelected : Bool
        isParentOfSelected =
            Canvas.Selection.isSelectedElementId
                (Maybe.andThen Canvas.Selection.removeOne selection)
                element.shared.id

        {-
           give different types of children different highlight colors
        -}
        maybeHighlights =
            case ( relation, element.shared.children ) of
                ( Parent, Spec.Element.Model.FlowChildren _ ) ->
                    [ Ui.Style.setHighlightToSecondary ]

                _ ->
                    []

        bakeData :
            ( List ( Spec.Element.Id.Id, Canvas.Events.SceneRectangle )
            , { handle : Canvas.Tool.Transform.Model.MouseDownLocation
              , elementBoundingBox : Canvas.Events.SceneRectangle
              }
            )
            -> Canvas.Tool.Transform.Model.MouseDownDataOnScreen
        bakeData ( siblingsDimensions, { elementBoundingBox, handle } ) =
            { handleLocation = handle
            , originalElementGeometry = elementBoundingBox
            , element = element
            , selectionItem = selectionItem
            , siblingsDimensions = siblingsDimensions
            }

        decodeMouseDown :
            Decode.Decoder Canvas.Tool.Transform.Model.MouseDownLocation
            ->
                Decode.Decoder
                    ( List ( Spec.Element.Id.Id, Canvas.Events.SceneRectangle )
                    , { handle : Canvas.Tool.Transform.Model.MouseDownLocation
                      , elementBoundingBox : Canvas.Events.SceneRectangle
                      }
                    )
        decodeMouseDown startLocationDecoder =
            Decode.map2 Tuple.pair
                Canvas.Events.readAllElements
                (decodeElementBoundingBoxForHandleWithoutParent startLocationDecoder)

        handles : Bool -> List (Element.Attribute Canvas.Tool.Transform.Msg.Msg)
        handles passThrough =
            let
                compassHandles =
                    (\handle -> Decode.succeed (Canvas.Tool.Transform.Model.CompassDirection handle) |> decodeMouseDown)
                        |> Canvas.Tool.Transform.Handles.handles camera passThrough

                decodePosition =
                    Canvas.Events.readAbsolutePoint
                        |> Decode.map (Canvas.Camera.Convert.absolutePointFromElementPointSimple camera)
                        |> Decode.map Canvas.Tool.Transform.Model.SomewhereOnElement
            in
            fullHandle (decodeMouseDown decodePosition) passThrough
                :: compassHandles
                |> List.map (Element.mapAttribute (bakeData >> Canvas.Tool.Transform.Msg.MouseDownOnScreen))

        isSelected : Bool
        isSelected =
            selection == Just selectionItem

        -- when something is being transformed, any element can be dragged into
        isTransformingSomething =
            state /= Canvas.Tool.Transform.Model.NotTransforming

        maybeHandles =
            if relation == Selected then
                handles isTransformingSomething

            else
                []

        return =
            { other = maybeHandles ++ maybeEvents ++ maybeHighlights ++ highlightDragTarget state augmentationParams
            , layout = []
            , element = Spec.Element.wrapScreen element
            }

        maybeEvents =
            case ( relation, isTransformingSomething ) of
                -- when no action is going on, allow selecting
                ( _, False ) ->
                    [ Canvas.Events.onPress
                        (Decode.succeed (Canvas.Tool.Transform.Msg.Select augmentationParams.selectionItem))
                    ]

                -- the current element can not receive events
                ( Selected, True ) ->
                    [ Ui.Help.noPointerEvents ]

                -- everything else is accepted as target
                ( _, True ) ->
                    [ onPressMove (readElementTarget camera augmentationParams.selectionItem (Spec.Element.wrapScreen element)) ]
    in
    case state of
        Canvas.Tool.Transform.Model.TransformingElement mouseDownData point ->
            let
                relSiblings =
                    mouseDownData.siblingsDimensions
                        |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera))
                        |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.elementRectangleFromAbsoluteRectangle parentAbs))
                        |> List.map (Tuple.mapSecond (\(Canvas.Events.ElementRectangle r_) -> r_))

                parentAbs =
                    mouseDownData.parentGeometry
                        |> Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera

                siblings =
                    Spec.Element.getChildren mouseDownData.parentElement
                        |> List.filterMap flowToAbsolute

                flowToAbsolute : Spec.Element.Model.EitherElement -> Maybe Spec.Element.Model.AbsoluteElement
                flowToAbsolute el_ =
                    relSiblings
                        |> List.Extra.find (\( siblingId, _ ) -> siblingId == el_.shared.id)
                        |> Maybe.map
                            (\( _, relativeDimensions ) ->
                                { shared = el_.shared
                                , outerGeometry =
                                    Canvas.Events.ElementRectangle relativeDimensions
                                        |> Spec.Element.createAbsoluteElementDimensions
                                }
                            )
            in
            if isParentOfSelected then
                { return
                    | element =
                        Spec.Element.setChildren
                            (Spec.Element.Model.AbsoluteChildren siblings)
                            (Spec.Element.wrapScreen element)
                            |> preventCollapse userModel augmentationParams.scope parentAbs
                }

            else
                return

        Canvas.Tool.Transform.Model.TransformingScreen mouseDownData point ->
            if isSelected then
                let
                    transformTarget =
                        mouseDownData.originalElementGeometry
                            |> Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera

                    transformPoint =
                        normalizeTargetPoint camera point

                    transformedRect =
                        transformRect
                            { startLocation = mouseDownData.handleLocation
                            , currentLocation = transformPoint
                            , rect = transformTarget
                            , constrainToOneAxis = shouldConstrainToAxis augmentationParams.pressedKeys
                            }

                    absSiblings =
                        mouseDownData.siblingsDimensions
                            |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera))
                            |> List.map (Tuple.mapSecond (\(Canvas.Events.AbsoluteRectangle r_) -> r_))

                    snapSources : Spec.Element.Id.Dict Rectangle.Rectangle
                    snapSources =
                        absSiblings
                            |> List.filter (\( id_, _ ) -> not <| id_ == mouseDownData.element.shared.id)
                            |> Spec.Element.Id.dictFromList

                    ( snappedRect, alignments ) =
                        Canvas.Guides.snap
                            camera
                            snapSources
                            transformedRect

                    (Canvas.Events.AbsoluteRectangle rawSnappedRect) =
                        snappedRect

                    layout =
                        [ Element.width
                            (Element.px <| round (Rectangle.width rawSnappedRect))
                        , Element.height
                            (Element.px <| round (Rectangle.height rawSnappedRect))
                        , Element.moveRight
                            (Rectangle.x1 rawSnappedRect)
                        , Element.moveDown
                            (Rectangle.y1 rawSnappedRect)
                        ]
                in
                { return
                    | layout = layout
                }

            else
                return

        _ ->
            return


transformRect :
    { startLocation : Canvas.Tool.Transform.Model.MouseDownLocation
    , currentLocation : Canvas.Events.AbsolutePoint
    , rect : Canvas.Events.AbsoluteRectangle
    , constrainToOneAxis : Bool
    }
    -> Canvas.Events.AbsoluteRectangle
transformRect { startLocation, currentLocation, rect, constrainToOneAxis } =
    let
        (Canvas.Events.AbsolutePoint currentLocation_) =
            currentLocation

        constrain offset =
            case constrainToOneAxis of
                False ->
                    offset

                True ->
                    { x =
                        if abs offset.x > abs offset.y then
                            offset.x

                        else
                            0
                    , y =
                        if abs offset.y > abs offset.x then
                            offset.y

                        else
                            0
                    }

        (Canvas.Events.AbsoluteRectangle rect_) =
            rect
    in
    Canvas.Events.AbsoluteRectangle <|
        case startLocation of
            Canvas.Tool.Transform.Model.CompassDirection Compass.Center ->
                let
                    offset =
                        Rectangle.getOffset
                            (Rectangle.center rect_)
                            currentLocation_
                            |> constrain
                in
                Rectangle.moveBy
                    offset.x
                    offset.y
                    rect_

            -- don't constrain when scaling
            Canvas.Tool.Transform.Model.CompassDirection direction ->
                Rectangle.transformWithPoint
                    direction
                    currentLocation_
                    rect_

            Canvas.Tool.Transform.Model.SomewhereOnElement (Canvas.Events.AbsolutePoint mouseDownPoint) ->
                let
                    offset =
                        Rectangle.getOffset
                            mouseDownPoint
                            currentLocation_
                            |> constrain
                in
                Rectangle.moveBy
                    offset.x
                    offset.y
                    rect_


type alias HandleAndBoundingBox =
    { handle : Canvas.Tool.Transform.Model.MouseDownLocation
    , elementBoundingBox : Canvas.Events.SceneRectangle
    , parentBoundingBox : Canvas.Events.SceneRectangle
    }


decodeElementBoundingBoxForHandle :
    Decode.Decoder Canvas.Tool.Transform.Model.MouseDownLocation
    -> Decode.Decoder HandleAndBoundingBox
decodeElementBoundingBoxForHandle mouseDownLocationDecoder =
    Decode.map3
        (\mouseDownLocation elementBoundingBox parentBoundingBox ->
            { handle = mouseDownLocation
            , elementBoundingBox = elementBoundingBox
            , parentBoundingBox = parentBoundingBox
            }
        )
        mouseDownLocationDecoder
        (Decode.at [ "target", "parentElement" ] BoundingClientRectangle.decodeAsRectangle
            |> Decode.map Canvas.Events.SceneRectangle
        )
        (Decode.at [ "target", "funk_closestParent" ] BoundingClientRectangle.decodeAsRectangle
            |> Decode.map Canvas.Events.SceneRectangle
        )


decodeElementBoundingBoxForHandleWithoutParent :
    Decode.Decoder Canvas.Tool.Transform.Model.MouseDownLocation
    -> Decode.Decoder { handle : Canvas.Tool.Transform.Model.MouseDownLocation, elementBoundingBox : Canvas.Events.SceneRectangle }
decodeElementBoundingBoxForHandleWithoutParent mouseDownDecoder =
    Decode.map2
        (\mouseDown elementBoundingBox -> { handle = mouseDown, elementBoundingBox = elementBoundingBox })
        mouseDownDecoder
        (Decode.at [ "target", "parentElement" ] BoundingClientRectangle.decodeAsRectangle
            |> Decode.map Canvas.Events.SceneRectangle
        )


getCorner : Compass.Direction -> Rectangle.Rectangle -> Rectangle.Point
getCorner location rect =
    case location of
        Compass.Center ->
            Rectangle.center rect

        Compass.North ->
            { y = Rectangle.y1 rect, x = Rectangle.centerX rect }

        Compass.South ->
            { y = Rectangle.y2 rect, x = Rectangle.centerX rect }

        Compass.West ->
            { y = Rectangle.centerY rect, x = Rectangle.x1 rect }

        Compass.East ->
            { y = Rectangle.centerY rect, x = Rectangle.x2 rect }

        Compass.NorthWest ->
            { y = Rectangle.y1 rect, x = Rectangle.x1 rect }

        Compass.SouthEast ->
            { y = Rectangle.y2 rect, x = Rectangle.x2 rect }

        Compass.NorthEast ->
            { y = Rectangle.y1 rect, x = Rectangle.x2 rect }

        Compass.SouthWest ->
            { y = Rectangle.y2 rect, x = Rectangle.x1 rect }


augmentScene : Canvas.Camera.Model.Model -> Canvas.Tool.Transform.Model.State -> List (Element.Attribute Canvas.Tool.Transform.Msg.Msg)
augmentScene camera state =
    case state of
        Canvas.Tool.Transform.Model.NotTransforming ->
            [ Canvas.Events.onPress (Decode.succeed Canvas.Tool.Transform.Msg.SceneClicked)
            ]

        _ ->
            [ onPressMove Canvas.Events.readScenePoint |> Element.mapAttribute (Canvas.Tool.Transform.Model.ToScene >> Canvas.Tool.Transform.Msg.MouseDrag)
            , Element.Events.Extra.onMouseUp (Decode.succeed Canvas.Tool.Transform.Msg.MouseUp)
            ]


onPressMove dec =
    let
        onlyLeftMouseButtonPress =
            Decode.field "buttons" Decode.int
                |> Decode.andThen
                    (\buttons ->
                        case buttons of
                            1 ->
                                dec

                            _ ->
                                Decode.fail "I only track mouse downs"
                    )
                |> Canvas.Events.ignoreFromOtherTargets
    in
    Element.Events.Extra.onMouseMove onlyLeftMouseButtonPress


update :
    List Keyboard.Key
    -> Model.Model.UserModel
    -> Canvas.Camera.Model.Model
    -> Canvas.Selection.Selection
    -> Canvas.Tool.Transform.Msg.Msg
    -> Canvas.Tool.Transform.Model.State
    -> ( Canvas.Tool.Transform.Model.State, Maybe Spec.Mutation.Mutation, Canvas.Selection.Selection )
update pressedKeys userModel camera selection msg state =
    case msg of
        Canvas.Tool.Transform.Msg.SceneClicked ->
            -- deselect
            ( state, Nothing, Nothing )

        Canvas.Tool.Transform.Msg.Select newSelection ->
            ( state, Nothing, Just newSelection )

        Canvas.Tool.Transform.Msg.MouseDownOnElement mouseDownData ->
            ( Canvas.Tool.Transform.Model.HandleGrabbedOnElement mouseDownData, Nothing, selection )

        Canvas.Tool.Transform.Msg.MouseDownOnScreen mouseDownData ->
            ( Canvas.Tool.Transform.Model.HandleGrabbedOnScreen mouseDownData, Nothing, selection )

        Canvas.Tool.Transform.Msg.MouseDrag point ->
            case state of
                Canvas.Tool.Transform.Model.HandleGrabbedOnElement mouseDownData ->
                    ( Canvas.Tool.Transform.Model.TransformingElement mouseDownData point, Nothing, selection )

                Canvas.Tool.Transform.Model.HandleGrabbedOnScreen mouseDownData ->
                    ( Canvas.Tool.Transform.Model.TransformingScreen mouseDownData point, Nothing, selection )

                Canvas.Tool.Transform.Model.TransformingElement mouseDownData _ ->
                    ( Canvas.Tool.Transform.Model.TransformingElement mouseDownData point, Nothing, selection )

                Canvas.Tool.Transform.Model.TransformingScreen mouseDownData _ ->
                    ( Canvas.Tool.Transform.Model.TransformingScreen mouseDownData point, Nothing, selection )

                _ ->
                    ( state, Nothing, selection )

        Canvas.Tool.Transform.Msg.MouseUp ->
            let
                ( mutation, newSelection ) =
                    case state of
                        Canvas.Tool.Transform.Model.TransformingScreen mouseDownData point ->
                            makeScreenMutation pressedKeys userModel point camera mouseDownData

                        Canvas.Tool.Transform.Model.TransformingElement mouseDownData point ->
                            makeElementMutation pressedKeys userModel point camera mouseDownData

                        _ ->
                            ( Nothing, selection )
            in
            ( Canvas.Tool.Transform.Model.NotTransforming, mutation, newSelection )



{-
   when transforming an element we must also convert its sibligns to absolute elements,
   otherwise things would jump around because the flow is chaning.
-}


makeElementMutation :
    List Keyboard.Key
    -> Model.Model.UserModel
    -> Canvas.Tool.Transform.Model.TransformTarget
    -> Canvas.Camera.Model.Model
    -> Canvas.Tool.Transform.Model.MouseDownDataOnElement
    -> ( Maybe Spec.Mutation.Mutation, Canvas.Selection.Selection )
makeElementMutation pressedKeys userModel point camera mouseDownData =
    let
        el =
            mouseDownData.element

        parentAbs =
            mouseDownData.parentGeometry
                |> Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera

        relSiblings =
            mouseDownData.siblingsDimensions
                |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera))
                |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.elementRectangleFromAbsoluteRectangle parentAbs))
                |> List.map (Tuple.mapSecond (\(Canvas.Events.ElementRectangle r_) -> r_))

        absSiblings =
            mouseDownData.siblingsDimensions
                |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera))
                |> List.map (Tuple.mapSecond (\(Canvas.Events.AbsoluteRectangle r_) -> r_))

        snapSources : Spec.Element.Id.Dict Rectangle.Rectangle
        snapSources =
            absSiblings
                -- we do not want the currently transformed element
                |> List.filter (\( id_, _ ) -> not <| id_ == mouseDownData.element.shared.id)
                |> Spec.Element.Id.dictFromList

        flowToAbsolute : Spec.Element.Model.EitherElement -> Maybe Spec.Element.Model.AbsoluteElement
        flowToAbsolute el_ =
            relSiblings
                |> List.Extra.find (\( siblingId, _ ) -> siblingId == el_.shared.id)
                |> Maybe.map
                    (\( _, relativeDimensions ) ->
                        { shared = el_.shared
                        , outerGeometry =
                            Canvas.Events.ElementRectangle relativeDimensions
                                |> Spec.Element.createAbsoluteElementDimensions
                        }
                    )

        unchangedSelection =
            Just mouseDownData.selectionItem

        noOp =
            ( Nothing, unchangedSelection )

        transformPoint =
            normalizeTargetPoint
                camera
                point
    in
    case flowToAbsolute mouseDownData.element of
        Nothing ->
            noOp

        Just transformTarget ->
            let
                context =
                    Interface.Scope.populateForElement mouseDownData.selectionItem userModel

                transformTargetSize =
                    transformTarget.outerGeometry
                        |> Spec.getAbsoluteElementRectangle userModel context
                        |> Canvas.Camera.Convert.absoluteRectangleFromElementRectangle parentAbs

                transformedRect =
                    transformRect
                        { startLocation =
                            mouseDownData.handleLocation
                        , currentLocation =
                            transformPoint
                        , rect =
                            transformTargetSize
                        , constrainToOneAxis =
                            shouldConstrainToAxis pressedKeys
                        }

                ( snappedRect, alignments ) =
                    Canvas.Guides.snap camera snapSources transformedRect

                resultRect =
                    snappedRect
                        |> Canvas.Camera.Convert.elementRectangleFromAbsoluteRectangle parentAbs

                transformedEl : Spec.Element.Model.AbsoluteElement
                transformedEl =
                    { shared = el.shared
                    , outerGeometry =
                        resultRect
                            |> Spec.Element.createAbsoluteElementDimensions
                    }

                parent =
                    mouseDownData.parentElement

                parentShared =
                    parent.shared

                sourceSelectedId =
                    Canvas.Selection.getTargetId mouseDownData.selectionItem
            in
            case Canvas.Selection.removeOne mouseDownData.selectionItem of
                Nothing ->
                    ( Nothing, Just mouseDownData.selectionItem )

                Just originalParentSelection ->
                    let
                        scope =
                            Interface.Scope.populateForElement mouseDownData.selectionItem userModel

                        siblingsAfterEmigration =
                            Spec.Element.getChildren mouseDownData.parentElement
                                |> List.filterMap flowToAbsolute
                                -- we do not want the currently transformed element in the siblings when we drag out of the element
                                |> List.filter (\{ shared } -> not <| shared.id == mouseDownData.element.shared.id)

                        ( mutation, selection ) =
                            case point of
                                Canvas.Tool.Transform.Model.ToScene _ ->
                                    let
                                        newSelectionItem =
                                            Canvas.Selection.SelectedRoot sourceSelectedId

                                        newScreen : Spec.Element.Model.Screen
                                        newScreen =
                                            { shared = transformedEl.shared
                                            , outerGeometry =
                                                Spec.Element.Model.Custom
                                                    (Canvas.Camera.Convert.absoluteRectangleFromElementRectangle parentAbs resultRect)
                                            }
                                    in
                                    ( Spec.Mutation.BatchMutation
                                        [ makeOriginalElementMutation siblingsAfterEmigration
                                        , Spec.Mutation.MoveElementToScene newScreen
                                        ]
                                    , Just newSelectionItem
                                    )

                                Canvas.Tool.Transform.Model.ToElement targetData ->
                                    if targetData.selectionItem == originalParentSelection then
                                        ( Spec.Element.getChildren mouseDownData.parentElement
                                            |> List.filterMap flowToAbsolute
                                            |> List.Extra.setIf (\{ shared } -> shared.id == mouseDownData.element.shared.id) transformedEl
                                            |> makeOriginalElementMutation
                                        , unchangedSelection
                                        )

                                    else
                                        let
                                            flowToAbsolute_ : Spec.Element.Model.EitherElement -> Maybe Spec.Element.Model.AbsoluteElement
                                            flowToAbsolute_ el_ =
                                                targetData.relChildren
                                                    |> List.Extra.find (\( siblingId, _ ) -> siblingId == el_.shared.id)
                                                    |> Maybe.map
                                                        (\( _, relativeDimensions_ ) ->
                                                            { shared = el_.shared
                                                            , outerGeometry =
                                                                relativeDimensions_
                                                                    |> Spec.Element.createAbsoluteElementDimensions
                                                            }
                                                        )

                                            targetChildren =
                                                Spec.Element.getChildren targetData.element
                                                    |> List.filterMap flowToAbsolute_

                                            newParentMutation =
                                                Spec.Mutation.CompleteTransform
                                                    targetData.selectionItem
                                                    resultingNewParent

                                            targetScope =
                                                Interface.Scope.populateForElement
                                                    targetData.selectionItem
                                                    userModel

                                            transformedElInNewContext =
                                                { transformedEl
                                                    | outerGeometry =
                                                        resultRect
                                                            |> Canvas.Camera.Convert.absoluteRectangleFromElementRectangle parentAbs
                                                            |> Canvas.Camera.Convert.elementRectangleFromAbsoluteRectangle targetData.targetAbs
                                                            |> Spec.Element.createAbsoluteElementDimensions
                                                }

                                            resultingNewParent =
                                                targetData.element
                                                    |> Spec.Element.setChildren
                                                        (Spec.Element.Model.AbsoluteChildren (targetChildren ++ [ transformedElInNewContext ]))
                                                    |> preventCollapse userModel targetScope targetData.targetAbs

                                            newSelection =
                                                targetData.selectionItem
                                                    |> Canvas.Selection.addOne sourceSelectedId

                                            -- when we reparent the child in the same hierarchy
                                            -- because the children of the parent still include the element even after removing it in the other mutation
                                            -- we need to manually remove it
                                            mutations =
                                                [ makeOriginalElementMutation siblingsAfterEmigration
                                                , newParentMutation
                                                , Spec.Mutation.Delete mouseDownData.selectionItem
                                                ]
                                                    |> Spec.Mutation.BatchMutation
                                        in
                                        ( mutations, Just newSelection )

                        makeOriginalElementMutation children =
                            Spec.Mutation.CompleteTransform
                                originalParentSelection
                                (resultingOriginalParent children)

                        resultingOriginalParent children =
                            parent
                                |> Spec.Element.setChildren
                                    (Spec.Element.Model.AbsoluteChildren children)
                                |> preventCollapse userModel scope parentAbs
                    in
                    ( Just mutation, selection )


shouldConstrainToAxis : List Keyboard.Key -> Bool
shouldConstrainToAxis =
    List.member Keyboard.Shift


makeScreenMutation :
    List Keyboard.Key
    -> Model.Model.UserModel
    -> Canvas.Tool.Transform.Model.TransformTarget
    -> Canvas.Camera.Model.Model
    -> Canvas.Tool.Transform.Model.MouseDownDataOnScreen
    -> ( Maybe Spec.Mutation.Mutation, Canvas.Selection.Selection )
makeScreenMutation pressedKeys userModel point camera mouseDownData =
    let
        el =
            mouseDownData.element

        transformTarget =
            mouseDownData.originalElementGeometry
                |> Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera

        transformPoint =
            normalizeTargetPoint camera point

        transformedRect =
            transformRect
                { startLocation = mouseDownData.handleLocation
                , currentLocation = transformPoint
                , rect = transformTarget
                , constrainToOneAxis =
                    shouldConstrainToAxis
                        pressedKeys
                }

        absSiblings =
            mouseDownData.siblingsDimensions
                |> List.map (Tuple.mapSecond (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera))
                |> List.map (Tuple.mapSecond (\(Canvas.Events.AbsoluteRectangle r_) -> r_))

        snapSources : Spec.Element.Id.Dict Rectangle.Rectangle
        snapSources =
            absSiblings
                -- snap to anything but the screen itself
                |> List.filter (\( id, _ ) -> not <| id == el.shared.id)
                |> Spec.Element.Id.dictFromList

        ( resultRect, alignments ) =
            transformRect
                { startLocation = mouseDownData.handleLocation
                , currentLocation = transformPoint
                , rect = transformTarget
                , constrainToOneAxis = shouldConstrainToAxis pressedKeys
                }
                |> Canvas.Guides.snap camera snapSources

        (Canvas.Events.AbsoluteRectangle rawResultRect) =
            resultRect

        -- if the user uses the center handle, he only wants to move and not change the preset
        outerGeometry =
            case ( el.outerGeometry, mouseDownData.handleLocation ) of
                ( Spec.Element.Model.Preset device _, Canvas.Tool.Transform.Model.CompassDirection Compass.Center ) ->
                    Spec.Element.Model.Preset
                        device
                        { x = Rectangle.x1 rawResultRect
                        , y = Rectangle.y1 rawResultRect
                        }

                ( Spec.Element.Model.Preset device _, Canvas.Tool.Transform.Model.SomewhereOnElement _ ) ->
                    Spec.Element.Model.Preset
                        device
                        { x = Rectangle.x1 rawResultRect
                        , y = Rectangle.y1 rawResultRect
                        }

                _ ->
                    Spec.Element.Model.Custom resultRect

        result : Spec.Element.Model.Screen
        result =
            { el
                | outerGeometry = outerGeometry
            }

        sourceSelectedId =
            Canvas.Selection.getTargetId mouseDownData.selectionItem

        unchangedSelection =
            Just mouseDownData.selectionItem
    in
    case point of
        Canvas.Tool.Transform.Model.ToScene _ ->
            ( result
                |> Spec.Mutation.UpdateScreen mouseDownData.selectionItem
                |> Just
            , unchangedSelection
            )

        Canvas.Tool.Transform.Model.ToElement targetData ->
            let
                absoluteOuterGeometry : Spec.Element.Model.AbsoluteElementDimensions
                absoluteOuterGeometry =
                    resultRect
                        |> Canvas.Camera.Convert.elementRectangleFromAbsoluteRectangle targetData.targetAbs
                        |> Spec.Element.createAbsoluteElementDimensions

                transformedElInNewContext =
                    { shared = el.shared
                    , outerGeometry = absoluteOuterGeometry
                    }

                flowToAbsolute_ : Spec.Element.Model.EitherElement -> Maybe Spec.Element.Model.AbsoluteElement
                flowToAbsolute_ el_ =
                    targetData.relChildren
                        |> List.Extra.find (\( siblingId, _ ) -> siblingId == el_.shared.id)
                        |> Maybe.map
                            (\( _, relativeDimensions_ ) ->
                                { shared = el_.shared
                                , outerGeometry =
                                    relativeDimensions_
                                        |> Spec.Element.createAbsoluteElementDimensions
                                }
                            )

                targetChildren =
                    Spec.Element.getChildren targetData.element
                        |> List.filterMap flowToAbsolute_

                newParentMutation =
                    Spec.Mutation.CompleteTransform
                        targetData.selectionItem
                        resultingNewParent

                targetScope =
                    Interface.Scope.populateForElement
                        targetData.selectionItem
                        userModel

                resultingNewParent =
                    targetData.element
                        |> Spec.Element.setChildren
                            (Spec.Element.Model.AbsoluteChildren (targetChildren ++ [ transformedElInNewContext ]))
                        |> preventCollapse userModel targetScope targetData.targetAbs

                newSelection =
                    targetData.selectionItem
                        |> Canvas.Selection.addOne sourceSelectedId

                mutations =
                    [ Spec.Mutation.CompleteTransform targetData.selectionItem resultingNewParent
                    , Spec.Mutation.Delete mouseDownData.selectionItem
                    ]
                        |> Spec.Mutation.BatchMutation
            in
            ( Just mutations, Just newSelection )
