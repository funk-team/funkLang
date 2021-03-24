module Canvas.Events exposing (..)

{-| Tracking events on the infinite canvas two major complexities.


# Performance

@docs ignoreFromOtherTargets


# Geometry Projection

  - performance for mouse event listeners

This module is a visual debugger for camera projections
At the time of this writing we have the problem that point projections on the draw tool are incorrect
when you are zoomed out and transforming the camera.

The strategy to resolve this is to

1.  wrap the points from events into types that describe their source context
2.  wrap the points for rendering into types that describe the target context
3.  visually render the projected points and then describe projections from source to target
4.  these typed projections will then be spot-checked with unit tests.

The Debug tool can be used to visually debug the projections.

Conversions can be found in Camera.Convert.

-}

import BoundingClientRectangle
import Element
import Element.Events.Extra
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Rectangle
import Spec.Element.Id



-- [generator-start]
-- different types of rectangles


{-| Relative to 0|0 of the current _viewport_, that is the browser window, unaffected by camera state
-}
type SceneRectangle
    = SceneRectangle Rectangle.Rectangle


{-| relative to {x = 0, y = 0} on the coordinate system
-}
type AbsoluteRectangle
    = AbsoluteRectangle Rectangle.Rectangle


{-| relative to {x = 0, y = 0} of an element, can be built from offsetPoints
-}
type ElementRectangle
    = ElementRectangle Rectangle.Rectangle



-- Different types of points


{-| Relative to an element, can be read from the DOM using offsetX and offsetY
-}
type ElementPoint
    = ElementPoint Rectangle.Point -- offset point as read from the dom


{-| relative to {x = 0, y = 0} on the coordinate system
-}
type AbsolutePoint
    = AbsolutePoint Rectangle.Point -- relative to {x = 0, y = 0} on the coordinate system


{-| Relative to 0|0 of the current _viewport_, that is the browser window, unaffected by camera state
-}
type ScenePoint
    = ScenePoint Rectangle.Point -- relative to the plane behind all other geometry which captures any event -- example: drawing a new shape.


type alias ElementPointInContext =
    { context : ( Spec.Element.Id.Id, SceneRectangle )
    , point : ElementPoint
    }


{-| As events bubble through the dom, they will call event listeners that are not actually the target.
Imagine I have a rectangle (<div>) on the artboard and move with the mouse over the rectangle.
The listeners I attached to mousemove for the artboard AND the rectangle will be called.
This currently creates race conditions (in the debug tool) and flickering (in the drawing tool).

We could call event.stopPropagation() but this has several disadvantages:

  - has performance impacts: <https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md>
  - because it forces synchronous Model (and DOM) updates: <https://package.elm-lang.org/packages/elm/virtual-dom/latest/VirtualDom#Handler>
  - did not work reliably (see state of debug tool in commit c2477a728c62003331cc88d02719b17d3c55ba17 )

Solution: Compare the funk element IDs and fail if the target is not the source.
This allows us to disable event handlers on elements that we do not want to listen to.
However it turns out that I used a deprecated value and it meant sth else than I thought.

-}
ignoreFromOtherTargets : Decode.Decoder a -> Decode.Decoder a
ignoreFromOtherTargets decodeCorrectTarget =
    Decode.map2 Tuple.pair
        -- in case of content, the user might click the content element rather than the wrapping elm-ui element which is why we go the tree back up one step.
        (Decode.oneOf [ Decode.at [ "target", "id" ] decodeNonEmpty, Decode.at [ "target", "parentElement", "id" ] decodeNonEmpty ])
        readIdOfCurrentTarget
        |> Decode.andThen
            (\( idOfClickedElementOrParent, idOfElementWithListener ) ->
                if idOfClickedElementOrParent == idOfElementWithListener then
                    decodeCorrectTarget

                else
                    Decode.fail "target and source are not the same element"
            )


decodeNonEmpty =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "" ->
                        Decode.fail "string is empty"

                    _ ->
                        Decode.succeed str
            )


readIdOfCurrentTarget =
    Decode.at [ "currentTarget", "id" ] Decode.string



-- READERS - not to be confused with decoders. Readers read stuff from the DOM and are NOT suitable for serialization/deserialization


readAbsolutePoint : Decode.Decoder ( ElementPoint, SceneRectangle )
readAbsolutePoint =
    Decode.map2
        Tuple.pair
        decodeOffsetPoint
        (Decode.field "target" readSceneRectangle)


readPointInContext =
    Decode.map2 ElementPointInContext
        (Decode.map2 Tuple.pair
            (readIdOfCurrentTarget
                |> Decode.andThen
                    (\str ->
                        case Spec.Element.Id.parseFromHtmlId str of
                            Just id ->
                                Decode.succeed id

                            Nothing ->
                                Decode.fail "could not parse id"
                    )
            )
            (Decode.field "target" readSceneRectangle)
        )
        decodeOffsetPoint


readScenePoint =
    Decode.map ScenePoint Rectangle.decodePoint


readSceneRectangle =
    BoundingClientRectangle.decodeAsRectangle
        |> Decode.map SceneRectangle


readIdAndSceneRectangle =
    Decode.map2 Tuple.pair
        (Decode.field "id" Decode.string |> Decode.andThen decodeIdHelp)
        readSceneRectangle


decodeIdHelp htmlId =
    case Spec.Element.Id.parseFromHtmlId htmlId of
        Just id ->
            Decode.succeed id

        Nothing ->
            Decode.fail "Could not parse funk element id"


{-| Ignore elements that are not funk elements, e.g. elements inserted by elm-ui
-}
readCanvasElements : Decode.Decoder (List ( Spec.Element.Id.Id, SceneRectangle ))
readCanvasElements =
    Decode.at
        [ "target", "funkCanvasElements" ]
        (resilientListDecoder readIdAndSceneRectangle)


{-| Ignore elements that are not funk elements, e.g. elements inserted by elm-ui
-}
readAllElements : Decode.Decoder (List ( Spec.Element.Id.Id, SceneRectangle ))
readAllElements =
    Decode.at
        [ "target", "allFunkElements" ]
        (resilientListDecoder readIdAndSceneRectangle)


{-| Ignore elements that are not funk elements, e.g. elements inserted by elm-ui
-}
readSiblingsInTarget : Decode.Decoder (List ( Spec.Element.Id.Id, SceneRectangle ))
readSiblingsInTarget =
    Decode.field
        "target"
        readSiblings


{-| Ignore elements that are not funk elements, e.g. elements inserted by elm-ui
-}
readSiblings : Decode.Decoder (List ( Spec.Element.Id.Id, SceneRectangle ))
readSiblings =
    Decode.field
        "childrenAsArray"
        (resilientListDecoder readIdAndSceneRectangle)


resilientListDecoder : Decode.Decoder a -> Decode.Decoder (List a)
resilientListDecoder dec =
    let
        dropBad =
            List.map
                (Decode.decodeValue dec)
                >> List.filterMap
                    (\el ->
                        case el of
                            Ok e ->
                                Just e

                            Err err ->
                                Nothing
                    )
    in
    Decode.list Decode.value
        |> Decode.map dropBad


decodeOffsetPoint : Decode.Decoder ElementPoint
decodeOffsetPoint =
    Decode.map2 Rectangle.Point
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)
        |> Decode.map ElementPoint



-- SMART EVENT HANDLERS
-- They ignore stupid events!


{-| Capture mouse moves but only if the element is the actual target
-}
onMouseMove : Decode.Decoder msg -> Element.Attribute msg
onMouseMove decoder =
    Html.Events.on "mousemove" (ignoreFromOtherTargets decoder)
        |> Element.htmlAttribute


onPress decoder =
    Html.Events.on "mousedown" (ignoreFromOtherTargets decoder)
        |> Element.htmlAttribute


onPressMove dec =
    let
        onlyLeftMouseButtonPress =
            Decode.field "buttons" Decode.int
                |> Decode.andThen
                    (\buttons ->
                        case buttons of
                            1 ->
                                ignoreFromOtherTargets dec

                            _ ->
                                Decode.fail "I only track mouse downs"
                    )
    in
    Element.Events.Extra.onMouseMove onlyLeftMouseButtonPress


onRelease dec =
    Element.Events.Extra.onMouseUp (ignoreFromOtherTargets dec)



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeAbsolutePoint =
    Decode.map AbsolutePoint Rectangle.decodePoint


decodeAbsoluteRectangle =
    Decode.map AbsoluteRectangle Rectangle.decodeRectangle


decodeElementPoint =
    Decode.map ElementPoint Rectangle.decodePoint


decodeElementPointInContext =
    Decode.map2
        ElementPointInContext
        (Decode.field "context" decodeTuple_Spec_Element_Model_Id_Id_SceneRectangle_)
        (Decode.field "point" decodeElementPoint)


decodeElementRectangle =
    Decode.map ElementRectangle Rectangle.decodeRectangle


decodeScenePoint =
    Decode.map ScenePoint Rectangle.decodePoint


decodeSceneRectangle =
    Decode.map SceneRectangle Rectangle.decodeRectangle


decodeTuple_Spec_Element_Model_Id_Id_SceneRectangle_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Spec.Element.Id.decodeId)
        (Decode.field "A2" decodeSceneRectangle)


encodeAbsolutePoint (AbsolutePoint a1) =
    Rectangle.encodePoint a1


encodeAbsoluteRectangle (AbsoluteRectangle a1) =
    Rectangle.encodeRectangle a1


encodeElementPoint (ElementPoint a1) =
    Rectangle.encodePoint a1


encodeElementPointInContext a =
    Encode.object
        [ ( "context", encodeTuple_Spec_Element_Model_Id_Id_SceneRectangle_ a.context )
        , ( "point", encodeElementPoint a.point )
        ]


encodeElementRectangle (ElementRectangle a1) =
    Rectangle.encodeRectangle a1


encodeScenePoint (ScenePoint a1) =
    Rectangle.encodePoint a1


encodeSceneRectangle (SceneRectangle a1) =
    Rectangle.encodeRectangle a1


encodeTuple_Spec_Element_Model_Id_Id_SceneRectangle_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", Spec.Element.Id.encodeId a1 )
        , ( "A2", encodeSceneRectangle a2 )
        ]



-- [generator-end]
