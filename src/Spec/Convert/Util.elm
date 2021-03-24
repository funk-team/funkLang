module Spec.Convert.Util exposing (correctSpacing, distanceToNextEdge, distanceToPreviousEdge, getSpacing)

{-| Helpers for the responsify algorithm
-}

import List
import List.Extra
import Rectangle
import Spec.Element.Layout


{-| Find the spacing, i.e. the minimum distance between elements
We can inject the boundary getters.
-}
getSpacing :
    (Rectangle.Rectangle -> Float)
    -> (Rectangle.Rectangle -> Float)
    -> List Rectangle.Rectangle
    -> Maybe Float
getSpacing boundaryMin boundaryMax drawn =
    let
        {- Given the next rectangle and the edge of the last rectangle determine the distance
           and always keep the shortest one
        -}
        init =
            ( Nothing, Nothing )

        keepSmallestDistance : Rectangle.Rectangle -> ( Maybe Float, Maybe Float ) -> ( Maybe Float, Maybe Float )
        keepSmallestDistance rect ( minPadding, lastEnd ) =
            case ( minPadding, lastEnd ) of
                -- there has been a rect before - so calculate the first padding
                ( Nothing, Just end ) ->
                    ( Just (boundaryMin rect - end), Just (boundaryMax rect) )

                -- keep the smallest padding
                ( Just padding, Just end ) ->
                    ( Just (min padding (boundaryMin rect - end)), Just (boundaryMax rect) )

                -- No last rect to compare to - propagate the next rect
                ( paddingSoFar, Nothing ) ->
                    ( paddingSoFar, Just (boundaryMax rect) )

        ( smallestDistance, _ ) =
            List.foldl
                keepSmallestDistance
                init
                drawn
    in
    smallestDistance



-- type alias Alignment =
--     { x : Maybe AlignmentX, y : Maybe AlignmentY }
--
--
-- type AlignmentX
--     = Left
--     | CenterX
--     | Right
--
--
-- type AlignmentY
--     = Top
--     | CenterY
--     | Bottom


{-| -}
correctSpacing :
    Rectangle.Rectangle -- parent
    -> List Rectangle.Rectangle -- children
    -> Maybe Spec.Element.Layout.Flow -- detected flow
    -> Maybe Int -- detected spacing
    -> Maybe Rectangle.Rectangle -- bounding box
    -> Maybe Int
correctSpacing parent children flow_ spacing_ boundingBox =
    let
        -- get the last child rectangle for this row
        lastRectangle =
            List.Extra.last children

        newSpacing =
            case lastRectangle of
                Just lastRectangle_ ->
                    let
                        -- is the spacing more then 25% of the width of the parent
                        isSpacingLarge =
                            if round (toFloat (spacing_ |> Maybe.withDefault 0) / Rectangle.width parent * 100) >= 25 then
                                True

                            else
                                False

                        -- if the distance to the RHS is within 15% of the parent width
                        -- we assume the distance is small.
                        isDistanceToRightSmall =
                            if ((Rectangle.x2 parent - Rectangle.x2 lastRectangle_) / Rectangle.x2 parent * 100) <= 15 then
                                True

                            else
                                False
                    in
                    case ( isSpacingLarge, isDistanceToRightSmall ) of
                        ( True, True ) ->
                            Just 0

                        ( False, False ) ->
                            spacing_

                        ( _, _ ) ->
                            spacing_

                Nothing ->
                    spacing_
    in
    newSpacing


distanceToPreviousEdge :
    List Rectangle.Rectangle
    -> Int
    -> Rectangle.Rectangle
    -> Int
distanceToPreviousEdge children index child =
    let
        previousElementDistance =
            if index /= 0 then
                case List.Extra.getAt (index - 1) children of
                    Just prevElement ->
                        abs (Rectangle.x2 prevElement - Rectangle.x1 child) |> round

                    Nothing ->
                        0

            else
                0

        -- distance =
        --   abs (Rectangle.x2 previousElement - Rectangle.x1 child)
    in
    previousElementDistance


distanceToNextEdge :
    List Rectangle.Rectangle
    -> Int
    -> Rectangle.Rectangle
    -> Int
distanceToNextEdge children index child =
    let
        previousElementDistance =
            if index /= (List.length children - 1) then
                case List.Extra.getAt (index + 1) children of
                    Just nextElement ->
                        abs (Rectangle.x1 nextElement - Rectangle.x2 child) |> round

                    Nothing ->
                        0

            else
                0

        -- distance =
        --   abs (Rectangle.x2 previousElement - Rectangle.x1 child)
    in
    previousElementDistance
