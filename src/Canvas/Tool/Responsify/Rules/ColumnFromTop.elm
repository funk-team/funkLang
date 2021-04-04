-- What this needs to do:
--
--  1. All widths to: Fill(min=Null, max=AD)
--  2. All heights to Shrink(min=AD, max=Null)
--  3. All x-align to CenterX
--  4. All y-align to none
--  5. Spacing as drawn
--  6. Set arrangement to column


module Canvas.Tool.Responsify.Rules.ColumnFromTop exposing (columnFromTop, columnFromTopMatch)

import Canvas.Tool.Responsify.Facts.Alignment exposing (allHorizFill, firstAlignedTop, indexVertCentreAligned, lastAlignedBottom)
import Canvas.Tool.Responsify.Facts.Layouts exposing (isAColumn)
import Canvas.Tool.Responsify.Facts.Spacing exposing (vertEvenlySpaced)
import Canvas.Tool.Responsify.Info exposing (Info)
import Canvas.Tool.Responsify.Rules exposing (Match, MatchResult(..), Rule)
import Canvas.Tool.Responsify.Transforms exposing (SimpleFlowBuilder, defaultFlowConfig, genericColumnXAlign, simpleColumnTransformWith)
import Canvas.Tool.Responsify.Types exposing (Drawn)
import Canvas.Tool.Responsify.Utils exposing (correctPadding, fillWidthFlipped, shrinkHeight)
import Spec.Element.Layout.Padding exposing (Padding)
import Spec.Element.Style.Edges exposing (EdgeDimensions)


{-| Matcher for "column from top" rule. This rule matches situations
that:

     1. Is a column layout (in terms of child elements with, no
        overlap in the Y-direction and aligned in the X-direction).

     2. Has multiple children or more or less equal height.

     3. The topmost child element is aligned to the top edge of the
        parent and the rest of the children are spaced more or less
        evenly below.

     4. Each child element horizontally fills the parent.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)
    import Canvas.Tool.Responsify.Rules exposing (MatchResult(..))

    F.matcherCheck columnFromTopMatch
      [ (F.gappedRow, Inapplicable)
      , (F.filledRow, Inapplicable)
      , (F.centredRow, Inapplicable)
      , (F.singleton1, Inapplicable)
      , (F.singleton2, Inapplicable)
      , (F.rowOf3, Inapplicable)
      , (F.centredColumn1, Applicable)
      , (F.centredColumn2, Inapplicable)
      , (F.sideAlignedColumn1, Inapplicable)
      , (F.sideAlignedColumn2, Inapplicable)
      , (F.columnOf2, Inapplicable)
      , (F.columnOf2Centred, Inapplicable)
      , (F.columnFromTop1, Applicable)
      ]
    --> True

-}
columnFromTopMatch : Match
columnFromTopMatch info data =
    let
        ( isColumn, info2 ) =
            isAColumn info data

        ( topAligned, info3 ) =
            firstAlignedTop info2 data

        ( bottomAligned, info4 ) =
            lastAlignedBottom info3 data

        ( centreAligned, info5 ) =
            indexVertCentreAligned 1 info4 data

        ( allFill, info6 ) =
            allHorizFill info5 data

        ( evenSpacing, info7 ) =
            vertEvenlySpaced info6 data

        excludeCol2 =
            if List.length data.children == 2 then
                bottomAligned || centreAligned

            else
                False

        matches =
            isColumn
                && topAligned
                && allFill
                && evenSpacing
                && not excludeCol2
    in
    ( if matches then
        Applicable

      else
        Inapplicable
    , info5
    )



-- Flow geometry builder for "column from top" for use with generic
-- column layout transformer.


columnFromTopBuilder : SimpleFlowBuilder
columnFromTopBuilder info data padding ch rect =
    { alignment =
        { x = genericColumnXAlign info data padding rect
        , y = Nothing
        }
    , size =
        { width = fillWidthFlipped (Just rect)
        , height = shrinkHeight rect
        }
    }



-- We need to treat padding specially here: we ignore the padding
-- value at the bottom side, using the top-side padding instead.


specialCorrectPadding : Info -> Drawn -> EdgeDimensions -> Padding
specialCorrectPadding info data dims =
    correctPadding { dims | bottom = dims.top }



-- "Column from top" rule definition using generic layout transformer.


columnFromTop : Rule m
columnFromTop =
    let
        config =
            \info data ->
                { defaultFlowConfig
                    | correctPadding = specialCorrectPadding info data
                }
    in
    { name = "column-from-top"
    , match = columnFromTopMatch
    , transform = simpleColumnTransformWith config columnFromTopBuilder
    }
