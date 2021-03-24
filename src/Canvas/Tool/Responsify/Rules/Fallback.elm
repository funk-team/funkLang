module Canvas.Tool.Responsify.Rules.Fallback exposing (fallbackRule)

import Canvas.Tool.Responsify.Rules exposing (MatchResult(..), Rule)
import Canvas.Tool.Responsify.Transforms exposing (SimpleFlowBuilder, simpleRowTransform)
import Canvas.Tool.Responsify.Utils exposing (fillHeight, fillWidth)
import Spec.Element.Layout as Layout


fallbackBuilder : SimpleFlowBuilder
fallbackBuilder info data padding ch rect =
    { alignment =
        { x = Just Layout.CenterX
        , y = Just Layout.Top
        }
    , size =
        { width = fillWidth <| Just rect
        , height = fillHeight rect
        }
    }


fallbackRule : Rule m
fallbackRule =
    { name = "fallback"
    , match = \info _ -> ( Applicable, info )
    , transform = simpleRowTransform fallbackBuilder
    }
