module Spec.Element.Controls.Decoration exposing (..)

import Color
import Color.Extra
import Element
import Element.Input
import Ui.Boxicons
import Ui.ColorPicker.Advanced
import Ui.ColorPicker.Basic
import Ui.Component



-- for i in (viewBackgroundPicker pickerState styleAttribs):
--   i[0]
--
-- def double(x): x * 2
-- inputList = [1,2,3]
-- outputList = [double(el) for el in inputList]
-- --> 2,4,6
-- inputList.map(double)


viewBorderRadiusSelector : Maybe Int -> Element.Element (Maybe Int)
viewBorderRadiusSelector radius =
    input "Border radius" radius 0


input label val derivedVal =
    Element.Input.text []
        { label = Element.Input.labelAbove [] (Element.text label)
        , placeholder = Just (Element.Input.placeholder [] (Element.text (String.fromInt derivedVal)))
        , text =
            case val of
                Nothing ->
                    ""

                Just t ->
                    String.fromInt t
        , onChange =
            \strVal ->
                case ( String.toInt strVal, strVal ) of
                    ( _, "" ) ->
                        Nothing

                    ( Nothing, _ ) ->
                        val

                    ( Just newVal, _ ) ->
                        Just newVal
        }


offwhite =
    Color.rgba 0.9 0.9 0.9 1


viewColorPicker :
    Maybe ( Color.Extra.Color, Ui.ColorPicker.Basic.State )
    -> Element.Element (Maybe ( Color.Extra.Color, Ui.ColorPicker.Basic.State ))
viewColorPicker color_ =
    case color_ of
        Nothing ->
            Ui.Component.button
                (Just <| ( offwhite, Ui.ColorPicker.Basic.init offwhite ))
                Ui.Boxicons.bxPlus
                "Add"
                True

        Just color ->
            let
                picker =
                    Ui.ColorPicker.Advanced.view color

                delete =
                    Element.Input.button
                        []
                        { onPress = Just Nothing
                        , label = Element.text "Remove"
                        }

                control =
                    picker
                        |> Element.el [ Element.paddingXY 0 5 ]
                        |> Element.map Just
            in
            Element.column [] [ control, delete ]
