-- What this needs to do:
--
--  1. All widths to Shrink(min=AD, max=Null)
--  2. All heights to Fill(min=AD, max=Null)
--  3. All y-align to CenterY
--  4. First x-align to Left, last to Right, others to CenterX.
--  5. Spacing as drawn.
--  6. Set arrangement to row.


module Canvas.Tool.Responsify.Rules.RowWithCentreGroup exposing (rowWithCentreGroup, rowWithCentreGroupMatch)

import Canvas.Tool.Responsify.Facts.Alignment exposing (firstAlignedLeft, interiorGroupHorizCentreAligned, lastAlignedRight)
import Canvas.Tool.Responsify.Facts.Basic exposing (firstLastSimilarWidth)
import Canvas.Tool.Responsify.Facts.Layouts exposing (isARow)
import Canvas.Tool.Responsify.Facts.Spacing exposing (interiorGroupHorizEvenlySpaced)
import Canvas.Tool.Responsify.Info exposing (Dim(..), getDim)
import Canvas.Tool.Responsify.Rules exposing (Match, MatchResult(..), Rule)
import Canvas.Tool.Responsify.Transforms exposing (IndexedFlowBuilder, defaultFlowConfig, genericRowYAlign, indexedRowTransformWith)
import Canvas.Tool.Responsify.Utils exposing (rowHeight, shrinkWidth)
import Spec.Element.Layout as Layout


{-| Matcher for "row with centre group" rule. This rule matches
situations that:

     1. Is a row layout (in terms of child elements of similar
        heights, no overlap in the X-direction and aligned in the
        Y-direction).

     2. There are more than 3 child elements.

     3. The leftmost child element is aligned to the left-hand end of
        the parent and the rightmost child element is aligned to the
        right-hand end of the parent.

     4. The leftmost and rightmost child elements are the same width.

     5. The other child element are spaced equally and the centroid of
        the elements as a whole is horizontally centred in the parent.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Rules exposing (MatchResult(..))

    F.matcherCheck rowWithCentreGroupMatch
      [ (F.gappedRow,          Inapplicable)
      , (F.filledRow,          Inapplicable)
      , (F.centredRow,         Inapplicable)
      , (F.singleton1,         Inapplicable)
      , (F.singleton2,         Inapplicable)
      , (F.rowOf3,             Inapplicable)
      , (F.centredColumn1,     Inapplicable)
      , (F.centredColumn2,     Inapplicable)
      , (F.sideAlignedColumn1, Inapplicable)
      , (F.sideAlignedColumn2, Inapplicable)
      , (F.columnOf2,          Inapplicable)
      , (F.columnOf2Centred,   Inapplicable)
      , (F.columnFromTop1,     Inapplicable)
      ]
    --> True

-}
rowWithCentreGroupMatch : Match
rowWithCentreGroupMatch info data =
    let
        nchildren =
            List.length data.children

        ( isRow, info2 ) =
            isARow info data

        ( leftAligned, info3 ) =
            firstAlignedLeft info2 data

        ( rightAligned, info4 ) =
            lastAlignedRight info3 data

        ( leftAndRightSimilarWidths, info5 ) =
            firstLastSimilarWidth info4 data

        ( notLeftRightAreEquallySpaced, info6 ) =
            interiorGroupHorizEvenlySpaced info5 data

        ( notLeftRightCentroidCentreAligned, info7 ) =
            interiorGroupHorizCentreAligned info6 data

        matches =
            (nchildren > 3)
                && isRow
                && leftAligned
                && rightAligned
                && leftAndRightSimilarWidths
                && notLeftRightAreEquallySpaced
                && notLeftRightCentroidCentreAligned
    in
    ( if matches then
        Applicable

      else
        Inapplicable
    , info7
    )



-- Flow geometry builder for filled rows for use with generic row
-- layout transformer.


rowWithCentreGroupBuilder : IndexedFlowBuilder
rowWithCentreGroupBuilder idx info data padding ch rect =
    { alignment =
        { x =
            if idx == 0 then
                Just Layout.Left

            else if idx == List.length data.children - 1 then
                Just Layout.Right

            else
                Just Layout.CenterX
        , y = genericRowYAlign info data padding rect
        }
    , size =
        { width = shrinkWidth rect
        , height = rowHeight data padding rect
        }
    }



-- Filled row rule definition using generic row layout transformer.


rowWithCentreGroup : Rule m
rowWithCentreGroup =
    let
        config =
            \info data ->
                { defaultFlowConfig
                    | spacing =
                        let
                            s =
                                Maybe.map round (getDim InteriorGroupMeanSpacing info)
                        in
                        case s of
                            Just 0 ->
                                Nothing

                            _ ->
                                s
                }
    in
    { name = "row-with-centre-group"
    , match = rowWithCentreGroupMatch
    , transform = indexedRowTransformWith config rowWithCentreGroupBuilder
    }
