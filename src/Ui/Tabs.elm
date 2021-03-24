module Ui.Tabs exposing (tabs, tabsWithActiveState, tabsWithUnderline)

{-| Tabs organize content across different screens, data sets, and other interactions.
-}

import Element
import Element.Border
import Element.Background
import Element.Font
import Element.Input
import Ui.Style exposing (edges)


{-| Tabs without underline, evenly spaced
-}
tabsWithActiveState : (a -> String) -> List a -> a -> Maybe a -> Element.Element a
tabsWithActiveState toLabel entries selected maybeActive =
    let
        attribs =
            [ Element.spaceEvenly
            , Element.padding 16
            , Element.width Element.fill
            , Element.centerX
            , Element.spacing 8
            ]

        isSelected : a -> Bool
        isSelected =
            (==) selected

        isActive : a -> Bool
        isActive t =
            maybeActive
                |> Maybe.map ((==) t)
                |> Maybe.withDefault False

        viewEntries =
            List.map
                (\e -> tabWithActiveState e (toLabel e) isSelected isActive)
                entries
    in
    Element.wrappedRow attribs viewEntries


tabWithActiveState t label isSelected isActive =
    let
        activeStyles =
            case
                ( isSelected t
                , isActive t
                )
            of
                ( True, True ) ->
                    [ Element.Font.bold
                    , Element.Font.color (Element.rgb 0 0 0.7)
                    , Element.Border.color (Element.rgb 0 0 0.7)
                    , Element.Border.widthEach { edges | bottom = 2 }
                    ]

                ( True, False ) ->
                    [ Element.Font.bold
                    , Element.Border.widthEach { edges | bottom = 2 }
                    ]

                ( False, True ) ->
                    [ Element.Font.color (Element.rgb 0 0 0.7)
                    , Element.Border.color (Element.rgb 0 0 0.7)
                    , Element.Border.widthEach { edges | bottom = 1 }
                    ]

                ( False, False ) ->
                    [ Element.Font.color Ui.Style.grey
                    , Element.Border.widthEach { edges | bottom = 1 }
                    ]

        styles =
            [ Element.padding 3
            , Element.centerX
            ]
                |> List.append activeStyles

        content =
            Element.text label
    in
    Element.Input.button
        styles
        { label = content
        , onPress = Just t
        }



{-| Render tabs with an underline
-}
tabsWithUnderline : (a -> String) -> List a -> a -> Element.Element a
tabsWithUnderline label entries active =
    let
        attribs =
            [ Element.spacing 5 ]

        viewEntries =
            List.map
                (\e -> tabWithUnderline e (label e) active)
                entries
    in
    Element.row attribs viewEntries


{-| Render a single tab with an underline
-}
tabWithUnderline t label selected =
    let
        isActive =
            selected == t

        text =
            Element.text label

        line =
            Element.el
                [ Element.Background.color
                    (if isActive then
                        Element.rgb 0 0 0

                     else
                        Element.rgba 0 0 0 0.1
                    )
                , Element.width Element.fill
                , Element.height (Element.px 2)
                ]
                Element.none

        content =
            Element.column [ Element.spacing 5 ]
                [ text
                , line
                ]
    in
    Element.Input.button
        [ Element.width Element.fill
        , Element.paddingEach { edges | top = 5 }
        ]
        { label = content
        , onPress = Just t
        }


{-| Tabs without underline, evenly spaced
-}
tabs : (a -> String) -> List a -> a -> Element.Element a
tabs label entries active =
    let
        attribs =
            [ Element.spaceEvenly
            , Element.width Element.fill
            ]

        viewEntries =
            List.map
                (\e -> tab e (label e) active)
                entries
    in
    Element.row attribs viewEntries


tab t label selected =
    let
        isActive =
            selected == t

        text =
            Element.text label

        activeStyles =
            if isActive then
                [ Element.Font.bold ]

            else
                [ Element.Font.color Ui.Style.grey ]

        content =
            text
    in
    Element.Input.button
        activeStyles
        { label = content
        , onPress = Just t
        }


