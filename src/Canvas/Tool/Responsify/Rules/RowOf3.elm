-- What this needs to do:
--
--  1. All widths to Shrink(min=AD, max=Null)
--  2. All heights to Fill(min=AD, max=Null)
--  3. All y-align to CenterY
--  4. First x-align to Left, second to CenterX, third to Right.
--  5. No spacing.
--  6. Set arrangement to wrapped row.


module Canvas.Tool.Responsify.Rules.RowOf3 exposing (rowOf3, rowOf3Match)

import Canvas.Tool.Responsify.Facts.Alignment exposing (firstAlignedLeft, indexHorizCentreAligned, lastAlignedRight)
import Canvas.Tool.Responsify.Facts.Layouts exposing (isARow)
import Canvas.Tool.Responsify.Rules exposing (Match, MatchResult(..), Rule)
import Canvas.Tool.Responsify.Transforms exposing (IndexedFlowBuilder, genericRowYAlign, indexedRowTransform)
import Canvas.Tool.Responsify.Utils exposing (rowHeight, shrinkWidth)
import Spec.Element.Layout as Layout


{-| Matcher for "row of 3" rule. This rule matches situations that:

     1. Is a row layout (in terms of child elements of similar
        heights, no overlap in the X-direction and aligned in the
        Y-direction).

     2. There are three child elements.

     3. The leftmost child element is aligned to the left-hand end of
        the parent, the rightmost child element is aligned to the
        right-hand end of the parent and the other child element is
        horizontally centred in the parent.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Rules exposing (MatchResult(..))

    F.matcherCheck rowOf3Match
      [ (F.gappedRow,          Inapplicable)
      , (F.filledRow,          Inapplicable)
      , (F.centredRow,         Inapplicable)
      , (F.singleton1,         Inapplicable)
      , (F.singleton2,         Inapplicable)
      , (F.rowOf3,             Applicable)
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
rowOf3Match : Match
rowOf3Match info data =
    let
        ( isRow, info2 ) =
            isARow info data

        ( leftAligned, info3 ) =
            firstAlignedLeft info2 data

        ( rightAligned, info4 ) =
            lastAlignedRight info3 data

        ( centreAligned, info5 ) =
            indexHorizCentreAligned 1 info4 data

        matches =
            (List.length data.children == 3)
                && isRow
                && leftAligned
                && rightAligned
                && centreAligned
    in
    ( if matches then
        Applicable

      else
        Inapplicable
    , info5
    )



-- Flow geometry builder for filled rows for use with generic row
-- layout transformer.


rowOf3Builder : IndexedFlowBuilder
rowOf3Builder idx info data padding ch rect =
    { alignment =
        { x =
            case idx of
                1 ->
                    Just Layout.CenterX

                2 ->
                    Just Layout.Right

                _ ->
                    Just Layout.Left
        , y = genericRowYAlign info data padding rect
        }
    , size =
        { width = shrinkWidth rect
        , height = rowHeight data padding rect
        }
    }



-- Filled row rule definition using generic row layout transformer.


rowOf3 : Rule m
rowOf3 =
    { name = "row-of-3"
    , match = rowOf3Match
    , transform = indexedRowTransform rowOf3Builder
    }
