module Ui exposing (viewGallery)

import Element
import Ui.Style
import Ui.Table2


{-| The funk design system
-}
viewGallery =
    { title = "Funk UI Framework"
    , body = [ Element.layout bodyStyles gallery ]
    }


gallery =
    Element.column [ Element.padding 20, Element.spacing 20, Element.width Element.fill ]
        [ Element.text "table"
        , Ui.Table2.viewExample
        ]


bodyStyles =
    Ui.Style.base
