-- What this needs to do:
--
--  1. All widths to Shrink(min=AD, max=Null)
--  2. All heights to Fill(min=AD, max=Null)
--  3. All y-align to CenterY
--  4. All x-align to CenterX
--  6. Set spacing for all to mean spacing
--  7. Set arrangement to row
--  8. Add padding on top and bottom only as required


module Canvas.Tool.Responsify.Rules.CentredRow exposing (centredRow, centredRowMatch)

import Canvas.Tool.Responsify.Facts.Alignment exposing (firstAlignedLeft, groupHorizCentreAligned, lastAlignedRight)
import Canvas.Tool.Responsify.Facts.Basic exposing (allSimilarWidth)
import Canvas.Tool.Responsify.Facts.Layouts exposing (isARow)
import Canvas.Tool.Responsify.Facts.Spacing exposing (distinctStartEndGroups)
import Canvas.Tool.Responsify.Info exposing (Dim(..), Info, getDim)
import Canvas.Tool.Responsify.Rules exposing (Match, MatchResult(..), Rule)
import Canvas.Tool.Responsify.Transforms exposing (SimpleFlowBuilder, defaultFlowConfig, genericRowYAlign, simpleRowTransformWith)
import Canvas.Tool.Responsify.Types exposing (Drawn)
import Canvas.Tool.Responsify.Utils exposing (rowHeight, shrinkWidth)
import Spec.Element.Layout as Layout exposing (Flow(..))
import Spec.Element.Layout.Padding exposing (Padding, Padding_(..))
import Spec.Element.Style.Edges exposing (EdgeDimensions)


{-| Matcher for "centred row" rule. This rule matches situations that:

     1. Is a row layout (in terms of child elements of similar
        heights, no overlap in the X-direction and aligned in the
        Y-direction).

     2. Does not have distinct groups of child elements at the left
        and right ends of the parent.

     3. The spacing between child elements is similar.

     4. The X-coordinate of the centroid of the child elements is
        close to the X-coordinate of the centre of the parent.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Rules exposing (MatchResult(..))

    F.matcherCheck centredRowMatch
      [ (F.gappedRow,          Inapplicable)
      , (F.filledRow,          Inapplicable)
      , (F.centredRow,         Applicable)
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
centredRowMatch : Match
centredRowMatch info data =
    let
        ( isRow, info2 ) =
            isARow info data

        ( distinctGroups, info3 ) =
            distinctStartEndGroups info2 data

        ( simWidth, info4 ) =
            allSimilarWidth info3 data

        ( leftAligned, info5 ) =
            firstAlignedLeft info4 data

        ( rightAligned, info6 ) =
            lastAlignedRight info5 data

        ( centreAligned, info7 ) =
            groupHorizCentreAligned info6 data

        match1 =
            isRow
                && not distinctGroups
                && simWidth
                && not leftAligned
                && not rightAligned
                && centreAligned
    in
    if not match1 then
        ( Inapplicable, info7 )

    else
        let
            minSpacing =
                getDim MinSpacing info6

            maxSpacing =
                getDim MaxSpacing info6
        in
        case Maybe.map2 (\mx mn -> mx - mn < 10.0) maxSpacing minSpacing of
            Just True ->
                ( Applicable, info6 )

            _ ->
                ( Inapplicable, info6 )


specialCorrectPadding : Info -> Drawn -> (EdgeDimensions -> Padding)
specialCorrectPadding info data dims =
    if dims.top == dims.bottom then
        PaddingXY Nothing dims.top

    else
        PaddingEach { dims | left = Nothing, right = Nothing }



-- Flow geometry builder for centred rows for use with generic row
-- layout transformer.


centredRowBuilder : SimpleFlowBuilder
centredRowBuilder info data padding ch rect =
    { alignment =
        { x = Just Layout.CenterX
        , y = genericRowYAlign info data padding rect
        }
    , size =
        { width = shrinkWidth rect
        , height = rowHeight data padding rect
        }
    }



-- Centred row rule definition using generic row layout transformer.


centredRow : Rule m
centredRow =
    let
        config =
            \info data ->
                { defaultFlowConfig
                    | spacing = Nothing
                    , flow = Just Row
                    , correctPadding = specialCorrectPadding info data
                }
    in
    { name = "centred-row"
    , match = centredRowMatch
    , transform = simpleRowTransformWith config centredRowBuilder
    }
