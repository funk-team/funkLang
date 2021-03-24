module Renderer.InteractivityAttribs exposing (schematicStyle, sheetEffects)

-- import Canvas.AttributesPanel.View

import Element
import Element.Background
import Element.Border
import Renderer.StyleCompositor


schematicStyle : Renderer.StyleCompositor.RenderedStyles msg
schematicStyle =
    { shadows = [ outlineShadow ]
    , other = []
    , typoStyles = []
    , background =
        [ Element.Background.color (Element.rgba 0 0 0 0.05)
        ]
    , imageCrop = Nothing
    }


sheetEffects : Renderer.StyleCompositor.RenderedStyles msg
sheetEffects =
    { shadows = [ outlineShadow, shadowDiffuse ]
    , typoStyles = []
    , background = []
    , other =
        [ Element.Border.rounded 3
        ]
    , imageCrop = Nothing
    }


outlineShadow =
    "0 0 0px 1px rgba(100, 100, 100, 0.15)"


shadowDiffuse =
    "0 0 20px 5px rgba(100, 100, 100, 0.1)"
