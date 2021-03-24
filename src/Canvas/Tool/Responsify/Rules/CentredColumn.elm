-- What this needs to do:
--
--  1. All widths to:
--       * Fill(min=AD, max=Null) if element fills parent width;
--       * Fill(min=Null, max=AD) if element does not fill parent width
--  2. All heights to Fill(min=AD, max=Null)
--  3. All x-align to CenterX
--  4. All y-align to Top
--  5. Set spacing for all to mean spacing
--  6. Set arrangement to column


module Canvas.Tool.Responsify.Rules.CentredColumn exposing (centredColumn, centredColumnMatch)

import Canvas.Tool.Responsify.Facts.Alignment exposing (allHorizCentreAligned, firstAlignedTop, lastAlignedBottom)
import Canvas.Tool.Responsify.Facts.Basic exposing (allSimilarHeight)
import Canvas.Tool.Responsify.Facts.Layouts exposing (isAColumn)
import Canvas.Tool.Responsify.Facts.Spacing exposing (distinctStartEndGroups)
import Canvas.Tool.Responsify.Rules exposing (Match, MatchResult(..), Rule)
import Canvas.Tool.Responsify.Transforms exposing (columnTransform, FlowBuilder, TransformExtras)
import Canvas.Tool.Responsify.Utils exposing (fillHeight, fillWidth, fillWidthFlipped)
import Rectangle
import Spec.Element.Layout as Layout


{-| Matcher for "centred column" rule. This rule matches situations
that:

     1. Is a column layout (in terms of child elements with, no
        overlap in the Y-direction and aligned in the X-direction).

     2. All child elements are similar heights.

     3. The topmost child element is aligned to the top edge of the
        parent and the bottommost child element is aligned to the
        bottom edge of the parent.

     4. Each element is centred horizontally in the parent.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)
    import Canvas.Tool.Responsify.Rules exposing (MatchResult(..))

    F.matcherCheck centredColumnMatch
      [ (F.gappedRow,          Inapplicable)
      , (F.filledRow,          Inapplicable)
      , (F.centredRow,         Inapplicable)
      , (F.singleton1,         Inapplicable)
      , (F.singleton2,         Inapplicable)
      , (F.rowOf3,             Inapplicable)
      , (F.centredColumn1,     Applicable)
      , (F.centredColumn2,     Applicable)
      , (F.sideAlignedColumn1, Inapplicable)
      , (F.sideAlignedColumn2, Inapplicable)
      , (F.columnOf2,          Inapplicable)
      , (F.columnOf2Centred,   Inapplicable)
      , (F.columnFromTop1,     Inapplicable)
      ]
    --> True

-}
centredColumnMatch : Match
centredColumnMatch info data =
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

        ( allCentred, info7 ) =
            allHorizCentreAligned info6 data

        matches =
            (List.length data.children > 2)
                && isColumn
                && not distinctGroups
                && simHeight
                && topAligned
                && bottomAligned
                && allCentred
    in
    ( if matches then
        Applicable

      else
        Inapplicable
    , info7
    )



-- Flow geometry builder for centred columns for use with generic
-- column layout transformer.


centredColumnBuilder : FlowBuilder Bool
centredColumnBuilder fillsWidth info data padding ch rect =
    { alignment =
        { x = Just Layout.CenterX
        , y = Just Layout.Top
        }
    , size =
        { width =
            if fillsWidth then
                fillWidth (Just rect)

            else
                fillWidthFlipped (Just rect)
        , height = fillHeight rect
        }
    }



-- "Extras" generator for centred columns for use with generic layout
-- transformer: for each child element, does the element fill the
-- parent horizontally?


centredColumnExtras : TransformExtras Bool
centredColumnExtras info data =
    let
        sorted =
            List.sortBy Rectangle.y1 data.children

        tolerance =
            0.1 * Rectangle.width data.parent

        checkFills r =
            abs (Rectangle.x1 r - Rectangle.x1 data.parent)
                < tolerance
                && abs (Rectangle.x2 r - Rectangle.x2 data.parent)
                < tolerance
    in
    List.map checkFills sorted



-- Centred column rule definition using generic layout transformer.


centredColumn : Rule m
centredColumn =
    { name = "centred-column"
    , match = centredColumnMatch
    , transform = columnTransform centredColumnExtras centredColumnBuilder
    }
