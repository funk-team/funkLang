module Ui.RadioRow exposing (view)

{-| Render a list of options in horizonal arrangement
-}

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Ui.Style


{-| All the items, how to label and determine how individual items are selected
-}
type alias RadioRowParams a =
    { items : List a
    , toLabel : a -> String
    , selected : a -> Bool
    }


{-| Render radio row similar to iOS tab bars
-}
view : RadioRowParams a -> Element.Element a
view ({ items, toLabel, selected } as params) =
    items
        |> List.map (viewOption params)
        |> Element.row
            [ Element.Border.width 1
            , Element.Background.color Ui.Style.black
            , Element.Border.color Ui.Style.black
            , Element.spacing 1
            , Element.Border.rounded 2
            , Element.moveDown 3
            ]


{-| Render a single tab inside iOS-style tab bars
-}
viewOption : RadioRowParams a -> a -> Element.Element a
viewOption params option =
    let
        isSelected =
            params.selected option

        label =
            Element.text
                (params.toLabel option)

        background =
            Element.Background.color <|
                if isSelected then
                    Ui.Style.highlightColorSolid

                else
                    Ui.Style.white

        borderColor =
            Element.Border.color Ui.Style.grey

        textColor =
            Element.Font.color <|
                if not isSelected then
                    Ui.Style.black

                else
                    Ui.Style.white

        attribs =
            [ Element.paddingXY 8 5
            , background
            , textColor
            , Element.Border.rounded 1
            ]
    in
    Element.Input.button attribs
        { onPress =
            if isSelected then
                Nothing

            else
                Just option
        , label = label
        }
