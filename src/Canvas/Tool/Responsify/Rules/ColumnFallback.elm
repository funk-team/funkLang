-- What this needs to do:
--
--  Just set arrangement to column


module Canvas.Tool.Responsify.Rules.ColumnFallback exposing (columnFallback, columnFallbackMatch)

import Canvas.Tool.Responsify.Facts.Layouts exposing (isAColumn)
import Canvas.Tool.Responsify.Facts.Spacing exposing (vertEvenlySpaced)
import Canvas.Tool.Responsify.Info exposing (Fact(..), Info, getFact)
import Canvas.Tool.Responsify.Rules exposing (Match, MatchResult(..), Rule)
import Canvas.Tool.Responsify.Transforms exposing (FlowBuilder, TransformExtras, columnTransformWith, defaultFlowConfig)
import Canvas.Tool.Responsify.Types exposing (Drawn)
import Canvas.Tool.Responsify.Utils exposing (comparison, correctPadding, fillWidthFlipped, shrinkHeight)
import Rectangle
import Spec.Element.Layout as Layout
import Spec.Element.Layout.Padding exposing (Padding, Padding_(..))
import Spec.Element.Style.Edges exposing (EdgeDimensions)


{-| Matcher for "column fallback" rule. This rule matches situations
that:

     1. Is a column layout (in terms of child elements with, no
        overlap in the Y-direction and aligned in the X-direction).

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)
    import Canvas.Tool.Responsify.Rules exposing (MatchResult(..))

    F.matcherCheck columnFallbackMatch
      [ (F.gappedRow,          Inapplicable)
      , (F.filledRow,          Inapplicable)
      , (F.centredRow,         Inapplicable)
      , (F.singleton1,         Inapplicable)
      , (F.singleton2,         Inapplicable)
      , (F.rowOf3,             Inapplicable)
      , (F.centredColumn1,     Applicable)
      , (F.centredColumn2,     Applicable)
      , (F.sideAlignedColumn1, Applicable)
      , (F.sideAlignedColumn2, Applicable)
      , (F.columnOf2,          Applicable)
      , (F.columnOf2Centred,   Applicable)
      , (F.columnFromTop1,     Applicable)
      ]
    --> True

-}
columnFallbackMatch : Match
columnFallbackMatch info data =
    let
        ( isColumn, info2 ) =
            isAColumn info data

        ( _, info3 ) =
            vertEvenlySpaced info2 data
    in
    ( if isColumn then
        Applicable

      else
        Inapplicable
    , info3
    )



-- Flow geometry builder for centred columns for use with generic
-- column layout transformer.


columnFallbackBuilder : FlowBuilder (Maybe Layout.AlignmentX)
columnFallbackBuilder align info data padding ch rect =
    { alignment =
        { x = align
        , y = Nothing
        }
    , size =
        { width = fillWidthFlipped <| Just rect
        , height = shrinkHeight rect
        }
    }



-- "Extras" generator for column fallback rule for use with generic
-- layout transformer: for each child element, is it aligned to one
-- side or to the centre of the parent, or none?


columnFallbackExtras : TransformExtras (Maybe Layout.AlignmentX)
columnFallbackExtras info data =
    let
        sorted =
            List.sortBy Rectangle.y1 data.children

        tolerance =
            comparison Rectangle.width data

        left r =
            abs (Rectangle.x1 r - Rectangle.x1 data.parent) < tolerance

        right r =
            abs (Rectangle.x2 r - Rectangle.x2 data.parent) < tolerance

        center r =
            abs
                ((Rectangle.center r).x
                    - (Rectangle.center data.parent).x
                )
                < tolerance

        chooseSide r =
            case ( left r, right r, center r ) of
                ( _, _, True ) ->
                    Just Layout.CenterX

                ( True, _, _ ) ->
                    Just Layout.Left

                ( _, True, _ ) ->
                    Just Layout.Right

                _ ->
                    Nothing
    in
    List.map chooseSide sorted



-- We need to treat padding specially here: we never apply bottom-side
-- padding, and if the elements in the column are centred in X, then
-- we apply padding only top-side padding.


specialCorrectPadding : Info -> Drawn -> EdgeDimensions -> Padding
specialCorrectPadding info data dims =
    case getFact AllHorizCentreAligned info of
        Just True ->
            correctPadding { dims | bottom = Nothing, left = Nothing, right = Nothing }

        _ ->
            correctPadding { dims | bottom = Nothing }



-- Column fallback rule definition using generic layout transformer.


columnFallback : Rule m
columnFallback =
    let
        config =
            \info data ->
                { defaultFlowConfig
                    | correctPadding = specialCorrectPadding info data
                }
    in
    { name = "column-fallback"
    , match = columnFallbackMatch
    , transform = columnTransformWith config columnFallbackExtras columnFallbackBuilder
    }
