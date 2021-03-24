module Ui.Input exposing (..)

{-| Provide input fields with optimized interaction
-}

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Ui.Scrubbable
import Ui.Style


{-| Helper
-}
edges =
    { bottom = 0, left = 0, right = 0, top = 0 }


integer :
    String
    -> String
    -> Maybe Int
    -> Element.Element (Maybe Int)
integer label placeholderText value =
    let
        text =
            case value of
                Just int ->
                    String.fromInt int

                Nothing ->
                    ""

        placeholder =
            placeholderText
                |> Element.text
                |> Element.Input.placeholder []
    in
    Element.Input.text []
        { placeholder = Just placeholder
        , text = text
        , onChange =
            \newText ->
                case newText of
                    "" ->
                        Nothing

                    _ ->
                        case String.toInt newText of
                            Nothing ->
                                value

                            something ->
                                something
        , label = Element.Input.labelAbove [] (Element.text label)
        }


string :
    String
    -> String
    -> String
    -> Element.Element String
string label placeholderText text =
    let
        placeholder =
            placeholderText
                |> Element.text
                |> Element.Input.placeholder []
    in
    Element.Input.text
        [ Element.paddingEach { edges | bottom = 5, top = 2 }
        , Element.Border.widthEach { edges | bottom = 1 }
        , Element.Border.rounded 0
        ]
        { placeholder = Just placeholder
        , text = text
        , onChange = identity
        , label = Element.Input.labelAbove [ Element.Font.bold ] (Element.text label)
        }


{-| Render a rich number input that has an underline when hovered

  - allows scrubbing to values above zero

-}
smartInt :
    String
    -> (List (Element.Attribute (Maybe Int)) -> Element.Element (Maybe Int) -> Element.Input.Label (Maybe Int))
    -> Maybe Int
    -> Int
    -> Element.Element (Maybe Int)
smartInt label labelPosition val derivedVal =
    Element.Input.text
        [ Element.Border.widthEach { edges | bottom = 1 }
        , Element.paddingEach { edges | bottom = 3 }
        , Element.Border.rounded 0
        , Element.Border.color Ui.Style.transparent
        , Element.mouseOver [ Element.Border.color Ui.Style.grey ]
        , Element.Background.color Ui.Style.transparent
        ]
        { label = labelPosition [ Element.paddingEach { edges | bottom = 4 } ] (Ui.Scrubbable.text label derivedVal |> Element.map (max 0 >> Just))
        , placeholder = Just (Element.Input.placeholder [ Element.Font.color Ui.Style.grey ] (Element.text (String.fromInt derivedVal)))
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
