-- What this needs to do:
--
--  1. Child width to Shrink(min=AD, max=Null)
--  2. Child height to Shrink(min=AD, max=Null)
--  3. Y-align to {Top|CenterY|Bottom} if child element drawn aligned
--  4. X-align to {Left|CentreX|Right} if child element drawn aligned
--  5. Set spacing to null
--  6. Set arrangement to row
--  7. Set no padding, except for non-aligned direction, as drawn on
--     gap with smallest padding in other direction


module Canvas.Tool.Responsify.Rules.Singleton exposing (singleton, singletonMatch)

import Canvas.Tool.Responsify.Facts.Alignment exposing (allHorizCentreAligned, allVertCentreAligned)
import Canvas.Tool.Responsify.Facts.Gravity exposing (Gravity(..), convertGravity, indexXGravity, indexYGravity)
import Canvas.Tool.Responsify.Info exposing (Fact(..), Info, Param(..), getFact, getParam)
import Canvas.Tool.Responsify.Rules exposing (Match, MatchResult(..), Rule)
import Canvas.Tool.Responsify.Transforms exposing (SimpleFlowBuilder, defaultFlowConfig, singletonTransform)
import Canvas.Tool.Responsify.Types exposing (Drawn)
import Canvas.Tool.Responsify.Utils exposing (correctPadding, shrinkHeight, shrinkWidth)
import Spec.Element.Layout as Layout exposing (Flow(..))
import Spec.Element.Layout.Padding exposing (Padding, Padding_(..))
import Spec.Element.Style.Edges exposing (EdgeDimensions)


{-| Matcher for "singleton" rule, i.e. a single child within a parent.
This rule matches any situation where there is a single child
within a parent.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Rules exposing (MatchResult(..))

    F.matcherCheck singletonMatch
      [ (F.gappedRow,          Inapplicable)
      , (F.filledRow,          Inapplicable)
      , (F.centredRow,         Inapplicable)
      , (F.singleton1,         Applicable)
      , (F.singleton2,         Applicable)
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
singletonMatch : Match
singletonMatch info data =
    let
        ( _, info2 ) =
            indexXGravity 0 info data

        ( _, info3 ) =
            indexYGravity 0 info2 data

        ( _, info4 ) =
            allHorizCentreAligned info3 data

        ( _, info5 ) =
            allVertCentreAligned info4 data
    in
    ( if List.length data.children == 1 then
        Applicable

      else
        Inapplicable
    , info5
    )



-- Flow geometry builder for singleton layout transformer.


singletonBuilder : SimpleFlowBuilder
singletonBuilder info data padding ch rect =
    let
        xalign =
            case
                ( getFact AllHorizCentreAligned info
                , convertGravity <| getParam (IndexXGravity 0) info
                )
            of
                ( Just True, _ ) ->
                    Just Layout.CenterX

                ( _, Zero ) ->
                    Just Layout.CenterX

                ( _, Minus ) ->
                    Just Layout.Left

                ( _, Plus ) ->
                    Just Layout.Right

                _ ->
                    Nothing

        yalign =
            case
                ( getFact AllVertCentreAligned info
                , convertGravity <| getParam (IndexYGravity 0) info
                )
            of
                ( Just True, _ ) ->
                    Just Layout.CenterY

                ( _, Zero ) ->
                    Just Layout.CenterY

                ( _, Minus ) ->
                    Just Layout.Top

                ( _, Plus ) ->
                    Just Layout.Bottom

                _ ->
                    Nothing
    in
    { alignment =
        { x = xalign
        , y = yalign
        }
    , size =
        { width = shrinkWidth rect
        , height = shrinkHeight rect
        }
    }


specialCorrectPadding : Info -> Drawn -> (EdgeDimensions -> Padding)
specialCorrectPadding info data =
    let
        xpadside =
            case
                ( getFact AllHorizCentreAligned info
                , convertGravity <| getParam (IndexXGravity 0) info
                )
            of
                ( Just True, _ ) ->
                    Nothing

                ( _, Minus ) ->
                    Just Layout.Left

                ( _, Plus ) ->
                    Just Layout.Right

                _ ->
                    Nothing

        ypadside =
            case
                ( getFact AllVertCentreAligned info
                , convertGravity <| getParam (IndexYGravity 0) info
                )
            of
                ( Just True, _ ) ->
                    Nothing

                ( _, Minus ) ->
                    Just Layout.Top

                ( _, Plus ) ->
                    Just Layout.Bottom

                _ ->
                    Nothing
    in
    \dims ->
        case ( xpadside, ypadside ) of
            ( Nothing, Nothing ) ->
                EqualPadding Nothing

            _ ->
                keepEdges dims (Layout.Alignment xpadside ypadside)


keepEdges : EdgeDimensions -> Layout.Alignment -> Padding
keepEdges edges align =
    let
        handleZero d =
            if d == Just 0 then
                Nothing

            else
                d

        left =
            case align.x of
                Just Layout.Left ->
                    handleZero edges.left

                _ ->
                    Nothing

        right =
            case align.x of
                Just Layout.Right ->
                    handleZero edges.right

                _ ->
                    Nothing

        top =
            case align.y of
                Just Layout.Top ->
                    handleZero edges.top

                _ ->
                    Nothing

        bottom =
            case align.y of
                Just Layout.Bottom ->
                    handleZero edges.bottom

                _ ->
                    Nothing
    in
    case ( ( left, right ), ( top, bottom ) ) of
        ( ( Nothing, Nothing ), ( Nothing, Nothing ) ) ->
            EqualPadding Nothing

        _ ->
            PaddingEach { left = left, right = right, top = top, bottom = bottom }



-- Singleton rule definition using generic row layout transformer.


singleton : Rule m
singleton =
    let
        config =
            \info data ->
                { defaultFlowConfig
                    | spacing = Nothing
                    , flow = Just Row
                    , correctPadding = specialCorrectPadding info data
                }
    in
    { name = "singleton"
    , match = singletonMatch
    , transform = singletonTransform config singletonBuilder
    }
