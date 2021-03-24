-- What this needs to do:
--
--  1. All widths to Fill(min=Nill, max=AD)
--  2. All heights to Fill(min=AD, max=Null)
--  3. All x-align to Left or Right, depending on side child is aligned to
--  4. All y-align to Top
--  5. Set spacing for all to mean spacing
--  6. Set arrangement to column


module Canvas.Tool.Responsify.Rules.SideAlignedColumn exposing (sideAlignedColumn, sideAlignedColumnMatch)

import Canvas.Tool.Responsify.Facts.Alignment as Alignment exposing (allHorizCentreAligned, allHorizSideAligned, firstAlignedTop, lastAlignedBottom)
import Canvas.Tool.Responsify.Facts.Basic exposing (allSimilarHeight)
import Canvas.Tool.Responsify.Facts.Layouts exposing (isAColumn)
import Canvas.Tool.Responsify.Facts.Spacing exposing (distinctStartEndGroups)
import Canvas.Tool.Responsify.Rules exposing (Match, MatchResult(..), Rule)
import Canvas.Tool.Responsify.Transforms exposing (FlowBuilder, TransformExtras, columnTransformWith, defaultFlowConfig)
import Canvas.Tool.Responsify.Types exposing (Drawn)
import Canvas.Tool.Responsify.Utils exposing (comparison, correctPadding, fillHeight, fillWidthFlipped)
import Rectangle
import Spec.Element.Layout as Layout
import Spec.Element.Layout.Padding exposing (Padding, Padding_(..))
import Spec.Element.Style.Edges exposing (EdgeDimensions)


{-| Matcher for "side-aligned column" rule. This rule matches
situations that:

     1. Is a column layout (in terms of child elements with, no
        overlap in the Y-direction and aligned in the X-direction).

     2. All child elements are similar heights.

     3. The topmost child element is aligned to the top edge of the
        parent and the bottommost child element is aligned to the
        bottom edge of the parent.

     4. Each element is aligned horizontally to either the left- or
        right-hand edge of the parent.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)
    import Canvas.Tool.Responsify.Rules exposing (MatchResult(..))

    F.matcherCheck sideAlignedColumnMatch
      [ (F.gappedRow,          Inapplicable)
      , (F.filledRow,          Inapplicable)
      , (F.centredRow,         Inapplicable)
      , (F.singleton1,         Inapplicable)
      , (F.singleton2,         Inapplicable)
      , (F.rowOf3,             Inapplicable)
      , (F.centredColumn1,     Inapplicable)
      , (F.centredColumn2,     Inapplicable)
      , (F.sideAlignedColumn1, Applicable)
      , (F.sideAlignedColumn2, Applicable)
      , (F.columnOf2,          Inapplicable)
      , (F.columnOf2Centred,   Inapplicable)
      , (F.columnFromTop1,     Inapplicable)
      ]
    --> True

-}
sideAlignedColumnMatch : Match
sideAlignedColumnMatch info data =
    let
        ( isColumn, info2 ) =
            isAColumn info data

        ( distinctGroups, info3 ) =
            distinctStartEndGroups info2 data

        ( simHeight, info4 ) =
            allSimilarHeight info3 data

        ( topAligned, info5 ) =
            firstAlignedTop info4 data

        ( bottomAligned, info6 ) =
            lastAlignedBottom info5 data

        ( allSideAligned, info7 ) =
            allHorizSideAligned info6 data

        ( allCentreAligned, info8 ) =
            allHorizCentreAligned info7 data

        matches =
            (List.length data.children > 1)
                && isColumn
                && not distinctGroups
                && simHeight
                && topAligned
                && bottomAligned
                && allSideAligned
                && not allCentreAligned
    in
    ( if matches then
        Applicable

      else
        Inapplicable
    , info8
    )



-- Flow geometry builder for side-aligned columns for use with generic
-- column layout transformer.


sideAlignedColumnBuilder : FlowBuilder Layout.AlignmentX
sideAlignedColumnBuilder align info data padding ch rect =
    { alignment =
        { x = Just align
        , y = Just Layout.Top
        }
    , size =
        { width = fillWidthFlipped (Just rect)
        , height = fillHeight rect
        }
    }



-- "Extras" generator for side aligned columns for use with generic
-- layout transformer: for each child element, which side is the
-- element aligned to?


sideAlignedColumnExtras : TransformExtras Layout.AlignmentX
sideAlignedColumnExtras info data =
    let
        sorted =
            List.sortBy Rectangle.y1 data.children

        tolerance =
            comparison Rectangle.width data

        chooseSide r =
            if abs (Rectangle.x1 r - Rectangle.x1 data.parent) < tolerance then
                Layout.Left

            else
                Layout.Right
    in
    List.map chooseSide sorted



-- We need to treat padding specially here: if all the child elements
-- are aligned to the right, we ignore the padding value at the left
-- side, using the right-side padding instead, and likewise if all
-- child elements are aligned to the left, we ignore the right-side
-- padding value and use the left-side padding instead.


specialCorrectPadding : Drawn -> EdgeDimensions -> Padding
specialCorrectPadding data dims =
    let
        delta =
            comparison Rectangle.width data

        left r =
            abs (Rectangle.x1 r - Rectangle.x1 data.parent) < delta

        allLeft =
            List.foldl (&&) True <| List.map left data.children

        right r =
            abs (Rectangle.x2 r - Rectangle.x2 data.parent) < delta

        allRight =
            List.foldl (&&) True <| List.map right data.children
    in
    case ( allLeft, allRight ) of
        ( True, False ) ->
            correctPadding { dims | right = dims.left }

        ( False, True ) ->
            correctPadding { dims | left = dims.right }

        _ ->
            correctPadding dims



-- Side-aligned column rule definition using generic layout
-- transformer.


sideAlignedColumn : Rule m
sideAlignedColumn =
    let
        config =
            \info data ->
                { defaultFlowConfig | correctPadding = specialCorrectPadding data }
    in
    { name = "side-aligned-column"
    , match = sideAlignedColumnMatch
    , transform = columnTransformWith config sideAlignedColumnExtras sideAlignedColumnBuilder
    }
