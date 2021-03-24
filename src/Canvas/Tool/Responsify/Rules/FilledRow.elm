-- What this needs to do:
--
--  1. If all elements are the same size:
--       => All widths to Fill(min=AD, max=Null)
--     Otherwise:
--       => All widths but last to Shrink(min=AD, max=Null)
--       => Last width to Fill(min=AD, max=Null)
--  2. All heights to Fill(min=AD, max=Null)
--  3. All y-align to CenterY
--  4. All x-align to Left.
--  5. Set spacing for all to mean spacing.
--  6. Set arrangement to wrapped row.


module Canvas.Tool.Responsify.Rules.FilledRow exposing (filledRow, filledRowMatch)

import Canvas.Tool.Responsify.Facts.Alignment exposing (firstAlignedLeft, lastAlignedRight)
import Canvas.Tool.Responsify.Facts.Basic exposing (allSimilarWidth, allSimilarWidthButLast)
import Canvas.Tool.Responsify.Facts.Layouts exposing (isARow)
import Canvas.Tool.Responsify.Facts.Spacing exposing (distinctStartEndGroups, horizEvenlySpaced)
import Canvas.Tool.Responsify.Info exposing (Fact(..), getFact)
import Canvas.Tool.Responsify.Rules exposing (Match, MatchResult(..), Rule)
import Canvas.Tool.Responsify.Transforms exposing (FlowBuilder, TransformExtras, genericRowYAlign, rowTransform)
import Canvas.Tool.Responsify.Utils exposing (fillWidth, rowHeight, shrinkWidth)
import Spec.Element.Layout as Layout


{-| Matcher for "filled row" rule. This rule matches situations that:

     1. Is a row layout (in terms of child elements of similar
        heights, no overlap in the X-direction and aligned in the
        Y-direction).

     2. All child elements are similar widths.

     3. The leftmost child element is aligned to the left-hand end of
        the parent and the rightmost child element is aligned to the
        right-hand end of the parent.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)
    import Canvas.Tool.Responsify.Rules exposing (MatchResult(..))

    F.matcherCheck filledRowMatch
      [ (F.gappedRow,          Inapplicable)
      , (F.filledRow,          Applicable)
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
filledRowMatch : Match
filledRowMatch info data =
    let
        ( isRow, info2 ) =
            isARow info data

        ( distinctGroups, info3 ) =
            distinctStartEndGroups info2 data

        ( simWidth, info4 ) =
            allSimilarWidth info3 data

        ( simWidthButLast, info5 ) =
            allSimilarWidthButLast info4 data

        ( leftAligned, info6 ) =
            firstAlignedLeft info5 data

        ( rightAligned, info7 ) =
            lastAlignedRight info6 data

        ( evenlySpaced, info8 ) =
            horizEvenlySpaced info7 data

        matches =
            (List.length data.children > 1)
                && isRow
                && not distinctGroups
                && (simWidth || simWidthButLast)
                && leftAligned
                && rightAligned
                && evenlySpaced
    in
    ( if matches then
        Applicable

      else
        Inapplicable
    , info8
    )



-- Flow geometry builder for filled rows for use with generic row
-- layout transformer.


filledRowBuilder : FlowBuilder Bool
filledRowBuilder last info data padding ch rect =
    let
        allsim =
            getFact AllSimilarWidth info
    in
    { alignment =
        { x = Just Layout.Left
        , y = genericRowYAlign info data padding rect
        }
    , size =
        { width =
            case allsim of
                Just True ->
                    fillWidth <| Just rect

                _ ->
                    if not last then
                        shrinkWidth rect

                    else
                        fillWidth <| Just rect
        , height = rowHeight data padding rect
        }
    }


filledRowExtras : TransformExtras Bool
filledRowExtras info data =
    List.repeat (List.length data.children - 1) False ++ [ True ]



-- Filled row rule definition using generic row layout transformer.


filledRow : Rule m
filledRow =
    { name = "filled-row"
    , match = filledRowMatch
    , transform = rowTransform filledRowExtras filledRowBuilder
    }
