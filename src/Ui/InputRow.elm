module Ui.InputRow exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Input
import Ui.Component
import Ui.Dropdown
import Ui.Style


view inputs =
    Element.row
        [ Element.Border.rounded 3
        , Element.Border.color Ui.Style.grey
        , Element.spacing 1
        ]
        inputs


dropdown =
    Ui.Dropdown.view
        [ Element.Background.color Ui.Style.grey
        ]


icon =
    Ui.Component.icon
        >> Element.el
            [ Element.Background.color Ui.Style.grey ]


textInput : { label : String, text : String, placeholder : String } -> Element.Element String
textInput { label, text, placeholder } =
    Element.Input.text
        [ Element.Background.color Ui.Style.grey
        ]
        { text = text
        , onChange = identity
        , label = Element.Input.labelHidden label
        , placeholder = Just <| Element.Input.placeholder [] (Element.text placeholder)
        }
