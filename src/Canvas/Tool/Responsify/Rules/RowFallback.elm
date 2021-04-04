-- What this needs to do:
--
--  Just set arrangement to  row.


module Canvas.Tool.Responsify.Rules.RowFallback exposing (rowFallback, rowFallbackMatch)

import Canvas.Tool.Responsify.Facts.Layouts exposing (isARow, isATopBottomRow)
import Canvas.Tool.Responsify.Info exposing (Fact(..), Info, getFact)
import Canvas.Tool.Responsify.Rules exposing (Match, MatchResult(..), Rule)
import Canvas.Tool.Responsify.Transforms exposing (FlowBuilder, TransformExtras, defaultFlowConfig, genericRowYAlign, transform)
import Canvas.Tool.Responsify.Types exposing (Drawn)
import Canvas.Tool.Responsify.Utils exposing (correctPadding, rowHeight, shrinkWidth)
import Rectangle
import Spec.Element.Layout as Layout
import Spec.Element.Layout.Padding exposing (Padding)
import Spec.Element.Style.Edges exposing (EdgeDimensions)


{-| Matcher for "row fallback" rule. This rule matches situations that:

     1. Is a row layout (in terms of child elements of similar
        heights, no overlap in the X-direction and aligned in the
        Y-direction).

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Rules exposing (MatchResult(..))

    F.matcherCheck rowFallbackMatch
      [ (F.gappedRow,          Applicable)
      , (F.filledRow,          Applicable)
      , (F.centredRow,         Applicable)
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
rowFallbackMatch : Match
rowFallbackMatch info data =
    let
        ( isRow, info2 ) =
            isARow info data

        ( isTopBottomRow, info3 ) =
            isATopBottomRow info2 data
    in
    ( if isRow || isTopBottomRow then
        Applicable

      else
        Inapplicable
    , info3
    )



-- Flow geometry builder for gapped rows for use with generic row
-- layout transformer.


rowFallbackBuilder : FlowBuilder (Maybe Layout.AlignmentX)
rowFallbackBuilder xalign info data padding ch rect =
    { alignment =
        { x = xalign
        , y = genericRowYAlign info data padding rect
        }
    , size =
        { width = shrinkWidth rect
        , height = rowHeight data padding rect
        }
    }



-- "Extras" generator for row fallback rule for use with generic
-- layout transformer: for each child element, is it aligned to one
-- side or to the centre of the parent, or none?


rowFallbackExtras : TransformExtras (Maybe Layout.AlignmentX)
rowFallbackExtras info data =
    let
        sorted =
            List.sortBy Rectangle.x1 data.children

        chooseXSide i r =
            if i == 0 && getFact FirstAlignedLeft info == Just True then
                Just Layout.Left

            else if i == List.length sorted - 1 && getFact LastAlignedRight info == Just True then
                Just Layout.Right

            else
                Nothing
    in
    List.indexedMap chooseXSide sorted



-- We need to treat padding specially here: we never apply right-side
-- padding unless there is a last element aligned right.


specialCorrectPadding : Info -> Drawn -> EdgeDimensions -> Padding
specialCorrectPadding info data dims =
    case getFact LastAlignedRight info of
        Just True ->
            correctPadding dims

        _ ->
            correctPadding { dims | right = Nothing }



-- Fallback row rule definition using generic row layout transformer.


rowFallback : Rule m
rowFallback =
    let
        config =
            \info data ->
                { defaultFlowConfig
                    | correctPadding = specialCorrectPadding info data
                }
    in
    { name = "row-fallback"
    , match = rowFallbackMatch
    , transform = transform config rowFallbackExtras rowFallbackBuilder
    }
