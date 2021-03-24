module Ui.BigTabs exposing (view)

import Element
import Element.Border
import Element.Events
import Element.Font
import Ui.Style


type alias Option msg =
    { isSelected : Bool
    , label : String
    , sideNote : Maybe String
    , onClick : msg
    }


view :
    List (Option msg)
    -> Element.Element msg
view options =
    options
        |> List.map viewTab
        |> Element.row
            [ Element.spacing 40 ]


viewTab option =
    Element.el
        (List.append
            ([ Element.Events.onClick option.onClick
             , Element.Font.size 36
             , Element.Font.bold
             , Element.height (Element.px 42)
             ]
                ++ (case option.sideNote of
                        Nothing ->
                            [ Element.pointer ]

                        Just text ->
                            [ Element.el
                                [ Element.Font.size Ui.Style.baseFontSize
                                , Element.alignBottom
                                , Element.centerX
                                , Element.moveDown 4
                                , Element.Font.regular
                                ]
                                (Element.text text)
                                |> Element.inFront
                            ]
                   )
            )
            (if option.isSelected then
                [ Element.Font.color (Element.rgb255 0 0 0)
                , Element.Border.widthEach
                    { bottom = 6
                    , left = 0
                    , right = 0
                    , top = 0
                    }
                , Element.Border.color (Element.rgb255 0 0 0)
                ]

             else
                [ Element.Font.color (Element.rgb255 195 195 195) ]
            )
        )
        (Element.text
            (option.label
                |> String.toUpper
            )
        )
