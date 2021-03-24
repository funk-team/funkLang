module Canvas.Tool.Responsify.Engine exposing
    ( evaluateRules
    , rules
    , runResponsifyEngine
    )

import Canvas.Tool.Responsify.Info exposing (Info, emptyInfo)
import Canvas.Tool.Responsify.Rules exposing (MatchResult(..), Rule)
import Canvas.Tool.Responsify.Rules.CentredColumn exposing (centredColumn)
import Canvas.Tool.Responsify.Rules.CentredRow exposing (centredRow)
import Canvas.Tool.Responsify.Rules.ColumnFallback exposing (columnFallback)
import Canvas.Tool.Responsify.Rules.ColumnFromTop exposing (columnFromTop)
import Canvas.Tool.Responsify.Rules.ColumnOf2 exposing (columnOf2)
import Canvas.Tool.Responsify.Rules.Fallback exposing (fallbackRule)
import Canvas.Tool.Responsify.Rules.FilledRow exposing (filledRow)
import Canvas.Tool.Responsify.Rules.GappedRow exposing (gappedRow)
import Canvas.Tool.Responsify.Rules.RowFallback exposing (rowFallback)
import Canvas.Tool.Responsify.Rules.RowOf3 exposing (rowOf3)
import Canvas.Tool.Responsify.Rules.RowWithCentreGroup exposing (rowWithCentreGroup)
import Canvas.Tool.Responsify.Rules.SideAlignedColumn exposing (sideAlignedColumn)
import Canvas.Tool.Responsify.Rules.Singleton exposing (singleton)
import Canvas.Tool.Responsify.Types exposing (Drawn, PredictionParams, predictionParamsToDrawn)
import Spec.Element.Model exposing (EitherElement)
import Spec.Model exposing (WithSpec)


runResponsifyEngine :
    WithSpec userModel
    -> PredictionParams
    -> ( Maybe String, Info, EitherElement )
runResponsifyEngine userModel data =
    let
        drawn =
            predictionParamsToDrawn data

        ( applicable, info ) =
            evaluateRules rules drawn

        transformed =
            case applicable of
                Nothing ->
                    data.element

                Just rule ->
                    rule.transform userModel drawn info
    in
    ( Maybe.map .name applicable, info, transformed )



-- All the  rules we know: at  the moment, these are  tested in order,
-- and the first one that matches is used.


rules : List (Rule m)
rules =
    [ singleton
    , rowOf3
    , gappedRow
    , centredRow
    , filledRow
    , rowWithCentreGroup
    , columnOf2
    , centredColumn
    , sideAlignedColumn
    , columnFromTop
    , rowFallback
    , columnFallback
    , fallbackRule
    ]



-- Evaluate a single rule, collecting information as needed.


evaluateRule : Rule m -> Info -> Drawn -> ( MatchResult, Info )
evaluateRule rule info data =
    rule.match info data


{-| For each rule:

  - Evaluate the rule with the current info, storing the new info away
    for the next step.
  - If the evaluation result is Applicable, add the current rule to the
    applicable rules list, and continue to next rule with new info.
  - If the evaluation result is Inapplicable, continue to next rule
    with new info.

When the rule list is exhausted, return the list of applicable
rules and the current info.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)
    import Canvas.Tool.Responsify.Rules exposing (MatchResult(..), Rule)

    Maybe.map .name <| Tuple.first <| evaluateRules rules F.gappedRow
    --> Just "gapped-row"
    Maybe.map .name <| Tuple.first <| evaluateRules rules F.filledRow
    --> Just "filled-row"
    Maybe.map .name <| Tuple.first <| evaluateRules rules F.centredRow
    --> Just "centred-row"
    Maybe.map .name <| Tuple.first <| evaluateRules rules F.rowOf3
    --> Just "row-of-3"

-}
evaluateRules : List (Rule m) -> Drawn -> ( Maybe (Rule m), Info )
evaluateRules rs data =
    evaluateRules_ data emptyInfo rs



-- Stateful worker for evaluateRules: keep track of information
-- collected during evaluation and current list of "Applicable" rules
-- we evaluate.


evaluateRules_ : Drawn -> Info -> List (Rule m) -> ( Maybe (Rule m), Info )
evaluateRules_ data info rs =
    case rs of
        [] ->
            -- Return accumulated rules, in reverse order so that the
            -- first rule we determined to be applicable comes first
            -- in the return list.
            ( Nothing, info )

        rule :: rest ->
            let
                -- Evaluate a single rule, accumulating info.
                ( applies, newInfo ) =
                    evaluateRule rule info data
            in
            case applies of
                -- Add applicable rules to the applicable list and
                -- continue.
                Applicable ->
                    ( Just rule, newInfo )

                -- Skip inapplicable rules (but keep info gathered
                -- during evaluating them).
                Inapplicable ->
                    evaluateRules_ data newInfo rest
