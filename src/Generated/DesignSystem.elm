module Generated.DesignSystem exposing (..)

import Element
import Element.Border
import Element.Font


h10TypoAttributes =
    [ Element.Font.size 60, Element.Font.letterSpacing 0 ]


h21TypoAttributes =
    [ Element.Font.size 40, Element.Font.letterSpacing 0 ]


h22TypoAttributes =
    [ Element.Font.size 20, Element.Font.letterSpacing 0 ]


bodyCopy3TypoAttributes =
    [ Element.Font.size 16, Element.Font.letterSpacing 0 ]


backgroundBgColorElement =
    Element.rgba 1 1 1 1


textTextColorElement =
    Element.rgba 0.2 0.2 0.2 1


outer0ShadowAttributes =
    [ Element.Border.shadow { offset = ( 0, 0 ), size = 2, blur = 5, color = Element.rgba 0 0 0 0.1 } ]


inner1ShadowAttributes =
    [ Element.Border.innerShadow { offset = ( 0, 0 ), size = 2, blur = 5, color = Element.rgba 0 0 0 0.06 } ]
