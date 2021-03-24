-- What this needs to do:
--
--  1. All widths to Shrink(min=AD, max=Null)
--  2. All heights to Fill(min=AD, max=Null)
--  3. All y-align to CenterY
--  4. All in start group: x-align to Left.
--  5. All in end group: x-align to Right.
--  6. Set spacing for all to mean spacing in left group.
--  7. Set arrangement to wrapped row.


module Canvas.Tool.Responsify.Rules.GappedRow exposing (gappedRow, gappedRowMatch)

import Canvas.Tool.Responsify.Facts.Layouts exposing (isARow, isATopBottomRow)
import Canvas.Tool.Responsify.Facts.Spacing exposing (distinctStartEndGroups)
import Canvas.Tool.Responsify.Info exposing (Fact(..), Param(..), getParam)
import Canvas.Tool.Responsify.Rules exposing (Match, MatchResult(..), Rule)
import Canvas.Tool.Responsify.Transforms exposing (FlowBuilder, TransformExtras, genericRowYAlign, rowTransform)
import Canvas.Tool.Responsify.Utils exposing (rowHeight, shrinkWidth)
import Spec.Element.Layout as Layout


{-| Matcher for "gapped row" rule. This rule matches situations that:

     1. Is a row layout (in terms of child elements of similar
        heights, no overlap in the X-direction and aligned in the
        Y-direction).

     2. Have distinct groups of child elements at the left and right
        ends of the parent, where "group" means child elements of
        similar widths with similar spacing between them, and
        "distinct" means that there is a larger space in between the
        two groups. Either of the left or right groups (but not both)
        can be empty.

     3. The leftmost child element of any left group is aligned to the
        left-hand end of the parent and the rightmost child element of
        any right group is aligned to the right-hand end of the
        parent.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Rules exposing (MatchResult(..))

    F.matcherCheck gappedRowMatch
      [ (F.gappedRow,          Applicable)
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
gappedRowMatch : Match
gappedRowMatch info data =
    let
        ( isRow, info2 ) =
            isARow info data

        ( isTBRow, info3 ) =
            isATopBottomRow info2 data

        ( distinctGroups, info4 ) =
            distinctStartEndGroups info3 data
    in
    ( if (isRow || isTBRow) && distinctGroups then
        Applicable

      else
        Inapplicable
    , info4
    )



-- Flow geometry builder for gapped rows for use with generic row
-- layout transformer.


gappedRowBuilder : FlowBuilder Layout.AlignmentX
gappedRowBuilder xalign info data padding ch rect =
    { alignment =
        { x = Just xalign
        , y = genericRowYAlign info data padding rect
        }
    , size =
        { width = shrinkWidth rect
        , height = rowHeight data padding rect
        }
    }



-- "Extras" generator for gapped rows for use with generic row layout
-- transformer: for each child element, is it in the left-hand group
-- or the right-hand group?


gappedRowExtras : TransformExtras Layout.AlignmentX
gappedRowExtras info data =
    let
        leftCount =
            Maybe.withDefault 0 <| getParam StartGroupSize info

        rightCount =
            List.length data.children - leftCount
    in
    List.repeat leftCount Layout.Left ++ List.repeat rightCount Layout.Right



-- Gapped row rule definition using generic row layout transformer.


gappedRow : Rule m
gappedRow =
    { name = "gapped-row"
    , match = gappedRowMatch
    , transform = rowTransform gappedRowExtras gappedRowBuilder
    }
