module Ui.Help exposing (..)

import Element
import Element.Font
import Html.Attributes
import Ui.Style


{-| implementation detail - pointerEvents pass through
-}
noPointerEvents =
    Element.htmlAttribute (Html.Attributes.class "pointer-events none")


allPointerEvents =
    Element.htmlAttribute (Html.Attributes.class "pointer-events all")


initFontStyles : List (Element.Attribute msg)
initFontStyles =
    [ Ui.Style.style "font-style" "initial"
    , Ui.Style.style "font-weight" "initial"
    , Ui.Style.style "text-shadow" "initial"
    , Ui.Style.style "font-family" "Inter"
    , Element.Font.letterSpacing 0
    ]
