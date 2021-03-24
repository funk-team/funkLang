module Canvas.Tool.Responsify.Utils exposing
    ( allSimilarWith
    , bottomCheck
    , centerCheckX
    , centerCheckY
    , comparison
    , correctPadding
    , fillHeight
    , fillWidth
    , fillWidthFlipped
    , interleave
    , leftCheck
    , orderAbsoluteElementsByID
    , rightCheck
    , rowHeight
    , shrinkHeight
    , shrinkWidth
    , sideBySideSort
    , testAllAdjacent
    , toFlowChild
    , topCheck
    )

import Canvas.Tool.Responsify.Types exposing (Drawn, PredictedOuterGeometry)
import List.Extra
import Maybe.Extra exposing (values)
import Rectangle exposing (Rectangle)
import Spec.Element.Id as I exposing (Id)
import Spec.Element.Layout as Layout
import Spec.Element.Layout.Length as Length
import Spec.Element.Layout.Padding exposing (Padding, Padding_(..))
import Spec.Element.Model exposing (AbsoluteElement, FlowElement)
import Spec.Element.Style.Edges exposing (EdgeDimensions)



-- Alignment tolerance as a fraction of the size of the parent
-- element.


tolerance : Float
tolerance =
    0.1



-- Alignment comparison.


comparison : (Rectangle -> Float) -> Drawn -> Float
comparison f data =
    let
        base =
            tolerance * f data.parent
    in
    -- if base < 10 then
    --     10
    -- else if base > 50 then
    --     50
    -- else
    base



-- Compare elements of a list for similarity using a similarity
-- function.


allSimilarWith : (a -> a -> Bool) -> List a -> Bool
allSimilarWith sim xs =
    case xs of
        y :: ys ->
            List.all (sim y) ys

        _ ->
            True



-- Test that all pairs of adjacent elements in a list pass a binary
-- test (with ordering of the list and so adjacency defined by a key
-- function).


testAllAdjacent : (a -> a -> Bool) -> (a -> comparable) -> List a -> Bool
testAllAdjacent test key xs =
    let
        sorted =
            List.sortBy key xs
    in
    List.foldl (&&) True <| List.map2 test sorted (List.drop 1 sorted)



-- Interleave elements of two lists until one list runs out of
-- elements.


interleave : List a -> List a -> List a
interleave xs ys =
    case xs of
        [] ->
            []

        x :: xss ->
            x :: interleave ys xss



-- Sort two lists side-by-side using comparison on elements of first
-- list.


sideBySideSort : (a -> comparable) -> List a -> List b -> List ( a, b )
sideBySideSort xcmp xs ys =
    List.map2 Tuple.pair xs ys
        |> List.sortBy (Tuple.first >> xcmp)



-- Find the old absolute child and apply the new derived layout to it.


toFlowChild : List AbsoluteElement -> PredictedOuterGeometry -> Maybe FlowElement
toFlowChild sourceElements { id, alignment, size } =
    case List.Extra.find (\el_ -> el_.shared.id == id) sourceElements of
        Nothing ->
            Nothing

        Just el_ ->
            Just
                { outerGeometry = { alignment = alignment, size = size }
                , shared = el_.shared
                }



-- Sort a list of absolute elements into an order defined by a list of
-- element IDs.


orderAbsoluteElementsByID : List AbsoluteElement -> List Id -> List AbsoluteElement
orderAbsoluteElementsByID es ids =
    let
        idmap =
            es
                |> List.map (\e -> ( e.shared.id, e ))
                |> I.dictFromList
    in
    values <| List.map (\i -> I.getFromDict i idmap) ids



-- Padding correction pixel tolerance.


paddingPxTol : Float
paddingPxTol =
    5.0



-- Detect common padding situations.


correctPadding : EdgeDimensions -> Padding
correctPadding dims =
    case ( dims.top, dims.bottom ) of
        ( Just t, Just b ) ->
            case ( dims.left, dims.right ) of
                ( Just l, Just r ) ->
                    let
                        xmean =
                            toFloat (l + r) / 2

                        ymean =
                            toFloat (t + b) / 2

                        allmean =
                            toFloat (l + r + t + b) / 4
                    in
                    if
                        abs (toFloat t - allmean)
                            < paddingPxTol
                            && abs (toFloat b - allmean)
                            < paddingPxTol
                            && abs (toFloat l - allmean)
                            < paddingPxTol
                            && abs (toFloat r - allmean)
                            < paddingPxTol
                    then
                        EqualPadding (Just t)

                    else if
                        abs (toFloat t - ymean)
                            < paddingPxTol
                            && abs (toFloat b - ymean)
                            < paddingPxTol
                            && abs (toFloat l - xmean)
                            < paddingPxTol
                            && abs (toFloat r - xmean)
                            < paddingPxTol
                    then
                        PaddingXY (Just l) (Just t)

                    else
                        PaddingEach dims

                _ ->
                    PaddingEach dims

        _ ->
            PaddingEach dims


topCheck : Drawn -> Padding -> Rectangle.Rectangle -> Bool
topCheck data padding r =
    let
        delta =
            case padding of
                EqualPadding (Just p) ->
                    p

                PaddingXY _ (Just p) ->
                    p

                PaddingEach dims ->
                    Maybe.withDefault 0 dims.top

                _ ->
                    0
    in
    abs (Rectangle.y1 r - toFloat delta - Rectangle.y1 data.parent)
        < comparison Rectangle.height data


bottomCheck : Drawn -> Padding -> Rectangle.Rectangle -> Bool
bottomCheck data padding r =
    let
        delta =
            case padding of
                EqualPadding (Just p) ->
                    p

                PaddingXY _ (Just p) ->
                    p

                PaddingEach dims ->
                    Maybe.withDefault 0 dims.bottom

                _ ->
                    0
    in
    abs (Rectangle.y2 r + toFloat delta - Rectangle.y2 data.parent)
        < comparison Rectangle.height data


leftCheck : Drawn -> Padding -> Rectangle.Rectangle -> Bool
leftCheck data padding r =
    let
        delta =
            case padding of
                EqualPadding (Just p) ->
                    p

                PaddingXY (Just p) _ ->
                    p

                PaddingEach dims ->
                    Maybe.withDefault 0 dims.left

                _ ->
                    0
    in
    abs (Rectangle.x1 r - toFloat delta - Rectangle.x1 data.parent)
        < comparison Rectangle.width data


rightCheck : Drawn -> Padding -> Rectangle.Rectangle -> Bool
rightCheck data padding r =
    let
        delta =
            case padding of
                EqualPadding (Just p) ->
                    p

                PaddingXY (Just p) _ ->
                    p

                PaddingEach dims ->
                    Maybe.withDefault 0 dims.right

                _ ->
                    0
    in
    abs (Rectangle.x2 r - toFloat delta - Rectangle.x2 data.parent)
        < comparison Rectangle.width data


centerCheckX : Drawn -> Rectangle.Rectangle -> Bool
centerCheckX data r =
    abs ((Rectangle.center r).x - (Rectangle.center data.parent).x)
        < comparison Rectangle.width data


centerCheckY : Drawn -> Rectangle.Rectangle -> Bool
centerCheckY data r =
    abs ((Rectangle.center r).y - (Rectangle.center data.parent).y)
        < comparison Rectangle.height data


rowHeight : Drawn -> Padding -> Rectangle -> Layout.Length
rowHeight data padding r =
    case ( topCheck data padding r, bottomCheck data padding r ) of
        ( True, True ) ->
            fillHeight r

        _ ->
            shrinkHeight r


shrinkHeight : Rectangle -> Layout.Length
shrinkHeight r =
    let
        h =
            Length.initNullable <| toFloat <| round <| Rectangle.height r
    in
    { behavior = Layout.Shrink, minMax = Layout.MinMax h Length.null }


shrinkWidth : Rectangle -> Layout.Length
shrinkWidth r =
    let
        w =
            Length.initNullable <| toFloat <| round <| Rectangle.width r
    in
    { behavior = Layout.Shrink, minMax = Layout.MinMax w Length.null }


fillHeight : Rectangle -> Layout.Length
fillHeight r =
    let
        h =
            Length.initNullable <| toFloat <| round <| Rectangle.height r
    in
    { behavior = Layout.Fill
    , minMax = Layout.MinMax h Length.null
    }


fillWidth : Maybe Rectangle -> Layout.Length
fillWidth mr =
    let
        w =
            case mr of
                Just r ->
                    Length.initNullable <| toFloat <| round <| Rectangle.width r

                Nothing ->
                    Length.null
    in
    { behavior = Layout.Fill
    , minMax = Layout.MinMax w Length.null
    }


fillWidthFlipped : Maybe Rectangle -> Layout.Length
fillWidthFlipped mr =
    let
        w =
            case mr of
                Just r ->
                    Length.initNullable <| toFloat <| round <| Rectangle.width r

                Nothing ->
                    Length.null
    in
    { behavior = Layout.Fill
    , minMax = Layout.MinMax Length.null w
    }
