module Element.Input.Extra exposing (inputSlider)

import Element
import Element.Background
import Element.Border
import Element.Input
import Ui.Style


inputSlider : String -> Int -> Int -> Element.Element Int
inputSlider labelText max spacing =
    slider labelText (toFloat max) (toFloat spacing)
        |> Element.map round


slider labelText max val =
    Element.Input.slider
        [ Element.height (Element.px 30)
        , Element.width Element.fill

        -- Here is where we're creating/styling the "track"
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Element.Background.color Ui.Style.grey
                , Element.Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = identity
        , label =
            Element.Input.labelAbove [] (numberInputLabel labelText val)
        , min = 0
        , max = max
        , step = Nothing
        , value = val
        , thumb =
            Element.Input.defaultThumb
        }


numberInputLabel labelText val =
    Element.Input.text
        [ Element.width Element.fill, Element.padding 5 ]
        { onChange = String.toFloat >> Maybe.withDefault val
        , text = String.fromFloat val
        , placeholder = Just (Element.Input.placeholder [ Element.centerY ] <| Element.text <| String.fromFloat val)
        , label = Element.Input.labelAbove [ Element.centerY ] <| Element.text labelText
        }
