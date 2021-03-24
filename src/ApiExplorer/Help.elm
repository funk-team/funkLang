module ApiExplorer.Help exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Ui.Boxicons
import Ui.Component
import Ui.Style



---- LAYOUT ----


viewValidCheck isValid =
    if isValid then
        Element.none
        --        Element.row
        --            [ Element.Font.color (Element.rgb 0 0.8 0)
        --            , Element.Font.size 10
        --            ]
        --            [ Ui.Component.icon Ui.Boxicons.bxCheck
        --            , Element.text "valid"
        --            ]

    else
        Element.row
            [ Element.Font.color (Element.rgb 0.8 0 0)
            , Element.Font.size 10
            ]
            [ Ui.Component.icon Ui.Boxicons.bxX
            , Element.text "invalid"
            ]


viewSection : String -> List (Element.Element msg) -> Element.Element msg
viewSection headerTxt content =
    let
        header =
            Element.text headerTxt

        sideLine =
            Element.el
                [ Element.Border.widthEach { edges | left = 1 }
                , Element.Border.color (Element.rgb 0.8 0.8 0.8)
                , Element.height Element.fill
                ]
                Element.none
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 10
        , Element.padding 5
        ]
        [ header
        , Element.row
            [ Element.width Element.fill
            , Element.spacing 10
            , Element.paddingEach { edges | left = 5, right = 25 }
            ]
            [ sideLine
            , Element.column
                [ Element.width Element.fill ]
                content
            ]
        ]


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }


multilineInput =
    Element.Input.multiline
        ([ Element.height (Element.maximum 500 <| Element.minimum 100 Element.shrink)
         , Element.width Element.fill
         , Ui.Style.monospace
         ]
            ++ greyBackgroundStyle
        )


greyBackgroundStyle =
    [ Element.paddingXY 16 13
    , Element.Border.width 0
    , Element.Border.rounded 3
    , Element.Background.color (Element.rgb 0.95 0.95 0.95)
    ]


greyBorderStyle =
    [ Element.paddingXY 13 10
    , Element.Border.width 3
    , Element.Border.rounded 4
    , Element.Border.color (Element.rgb 0.95 0.95 0.95)
    ]
