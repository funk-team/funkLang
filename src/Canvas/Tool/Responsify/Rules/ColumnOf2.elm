-- What this needs to do:
--
--  1. All widths to: Fill(min=AD, max=Null)
--  2. All heights to Shrink(min=AD, max=Null)
--  3. All x-align to CenterX
--  4. First element y-align to Top, second element y-align to Bottom
--     or CenterY, depending on whether it's aligned to the bottom
--     edge of the parent or vertically centred in the parent.
--  5. No spacing
--  6. Set arrangement to column


module Canvas.Tool.Responsify.Rules.ColumnOf2 exposing (columnOf2, columnOf2Match)

import Canvas.Tool.Responsify.Facts.Alignment exposing (allHorizFill, firstAlignedTop, indexVertCentreAligned, lastAlignedBottom)
import Canvas.Tool.Responsify.Facts.Layouts exposing (isAColumn)
import Canvas.Tool.Responsify.Info exposing (Info)
import Canvas.Tool.Responsify.Rules exposing (Match, MatchResult(..), Rule)
import Canvas.Tool.Responsify.Transforms exposing (FlowBuilder, TransformExtras, columnTransformWith, defaultFlowConfig)
import Canvas.Tool.Responsify.Types exposing (Drawn)
import Canvas.Tool.Responsify.Utils exposing (correctPadding, fillWidthFlipped, shrinkHeight)
import Spec.Element.Layout as Layout
import Spec.Element.Layout.Padding exposing (Padding)
import Spec.Element.Style.Edges exposing (EdgeDimensions)


{-| Matcher for "column of 2" rule. This rule matches situations
that:

     1. Is a column layout (in terms of child elements with, no
        overlap in the Y-direction and aligned in the X-direction).

     2. Has two children.

     3. The topmost child element is aligned to the top edge of the
        parent and the bottommost child element is either aligned to
        the bottom edge of the parent or is centred vertically in the
        parent.

     4. Each element horizontally fills the parent.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)
    import Canvas.Tool.Responsify.Rules exposing (MatchResult(..))

    F.matcherCheck columnOf2Match
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
      , (F.columnOf2,          Applicable)
      , (F.columnOf2Centred,   Applicable)
      , (F.columnFromTop1,     Inapplicable)
      ]
    --> True

-}
columnOf2Match : Match
columnOf2Match info data =
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

        matches =
            (List.length data.children == 2)
                && isColumn
                && topAligned
                && (bottomAligned || centreAligned)
                && allFill
    in
    ( if matches then
        Applicable

      else
        Inapplicable
    , info6
    )



-- Flow geometry builder for "columns of 2" for use with generic
-- column layout transformer.


columnOf2Builder : FlowBuilder Layout.AlignmentY
columnOf2Builder align info data padding ch rect =
    { alignment =
        { x = Just Layout.CenterX
        , y = Just align
        }
    , size =
        { width = fillWidthFlipped (Just rect)
        , height = shrinkHeight rect
        }
    }



-- "Extras" generator for side aligned columns for use with generic
-- layout transformer: for each child element, which side is the
-- element aligned to?


columnOf2Extras : TransformExtras Layout.AlignmentY
columnOf2Extras info data =
    let
        ( centreAligned, _ ) =
            indexVertCentreAligned 1 info data

        secondAlign =
            if centreAligned then
                Layout.CenterY

            else
                Layout.Bottom
    in
    [ Layout.Top, secondAlign ]



-- We need to treat padding specially here: if the second child
-- element is vertically centred, we ignore the padding value at the
-- bottom side, using the top-side padding instead.


specialCorrectPadding : Info -> Drawn -> EdgeDimensions -> Padding
specialCorrectPadding info data dims =
    if Tuple.first <| indexVertCentreAligned 1 info data then
        correctPadding { dims | bottom = dims.top }

    else
        correctPadding dims



-- "Column of 2" rule definition using generic layout transformer.


columnOf2 : Rule m
columnOf2 =
    let
        config =
            \info data ->
                { defaultFlowConfig
                    | correctPadding = specialCorrectPadding info data
                }
    in
    { name = "column-of-2"
    , match = columnOf2Match
    , transform = columnTransformWith config columnOf2Extras columnOf2Builder
    }
