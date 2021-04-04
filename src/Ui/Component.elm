module Ui.Component exposing (..)

{-| Funk component libray for building the UI of the funk platform
-}

import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Events.Extra
import Element.Font
import Element.Input
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Ui.Boxicons
import Ui.Style


collapsibleStep label content =
    Element.column [ Element.spacing 10, Element.width Element.fill ]
        [ Element.el [ Element.alpha 0.5, Element.width Element.fill ] label
        , case content of
            Just c ->
                Element.el
                    [ Element.Border.widthEach { edges | left = 1 }
                    , Element.paddingXY 10 0
                    , Element.Border.color Ui.Style.slightAccent
                    , Element.width Element.fill
                    ]
                    c

            Nothing ->
                Element.none
        ]


verticalLine =
    Element.el [ Element.height Element.fill, Element.width (Element.px 1), Element.Background.color Ui.Style.slightAccent ] Element.none


helpButton { helpOpen } msg =
    Element.Input.button
        (buttonStyle
            ++ (if helpOpen then
                    activeButtonStyles

                else
                    []
               )
        )
        { label = Element.text "?"
        , onPress = Just msg
        }


activeButtonStyles =
    [ Element.Font.color Ui.Style.white, Element.Background.color Ui.Style.black ]


{-| Like html.pre for displaying text with significant whitespace
-}
preFormattedElement : String -> Element.Element msg
preFormattedElement text =
    Element.paragraph
        [ Ui.Style.monospace
        , Ui.Style.style "white-space" "pre"
        ]
        [ Element.html
            (Html.text text)
        ]


textListWithHeader : Bool -> String -> List String -> Element.Element ()
textListWithHeader showClose title points =
    let
        pointRender point =
            Element.paragraph [ Element.width Element.fill ] [ Element.text point ]

        closeButton =
            if showClose then
                Element.Input.button [ Element.moveLeft 5 ] { onPress = Just (), label = icon Ui.Boxicons.bxX }

            else
                Element.none
    in
    Element.column
        [ Element.spacingXY 0 20, Element.width Element.fill ]
        ([ Element.row [ Element.Font.bold, Element.Font.size 20 ]
            [ closeButton
            , Element.text title
            ]
         ]
            ++ [ Element.column [ Element.spacingXY 0 5, Element.width Element.fill, Element.spacingXY 0 10 ] (List.map pointRender points) ]
        )


trashCan attribs msg =
    Element.el
        (attribs
            ++ [ Element.Events.onClick msg
               , Element.alpha 0.3
               , Element.scale 0.8
               , Element.mouseOver [ Element.alpha 1 ]
               ]
        )
        (icon Ui.Boxicons.bxTrash)


tooltip text =
    Element.htmlAttribute (Html.Attributes.title text)


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


section =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 5
        , Element.Border.widthEach { edges | left = 1 }
        , Element.paddingXY 10 5
        , Element.Border.color Ui.Style.grey
        ]


buttonWith :
    Element.Color
    -> Element.Color
    -> Element.Color
    -> msg
    -> (String -> Html.Html msg)
    -> String
    -> Bool
    -> Element.Element msg
buttonWith iconColor hover background action labelIcon labelText isActive =
    let
        -- more padding on the right because we need to manually move the icon to give some space
        -- Element.spacing does not work on button labels for some reason
        adjustedPadding =
            case labelText of
                "" ->
                    Element.paddingEach { bottom = 5, top = 5, right = 10, left = 10 }

                _ ->
                    Element.paddingEach { bottom = 5, top = 5, right = 15, left = 10 }

        styles =
            [ Element.Border.rounded 5
            , Element.Background.color background
            , Element.mouseOver <|
                [ Element.Background.color
                    (if isActive then
                        background

                     else
                        hover
                    )
                ]
            ]
    in
    Element.Input.button
        styles
        { onPress = Just action
        , label =
            Element.row
                [ adjustedPadding, Element.centerX, Element.centerY, Element.Font.color iconColor ]
                [ icon labelIcon, Element.el [ Element.moveRight 5 ] (Element.text labelText) ]
        }


icon i =
    Element.html (i "currentColor")


button action icon_ labelText isActive =
    let
        iconColor =
            if isActive then
                Element.rgba 1 1 1 1

            else
                Element.rgba 0.1 0.1 0.1 0.9

        hover =
            Element.rgba 0 0 0 0.2

        background =
            if isActive then
                Element.rgba 0 0 0 0.8

            else
                Element.rgba 0 0 0 0.1
    in
    buttonWith iconColor hover background action icon_ labelText isActive


buttonWarnIfInactive action icon_ labelText isActive =
    let
        iconColor =
            if isActive then
                Ui.Style.black

            else
                Ui.Style.white

        hover =
            Ui.Style.black

        background =
            if isActive then
                Element.rgba 0.7 0.7 0.7 0.3

            else
                Ui.Style.importantHighlightColorSolid
    in
    buttonWith iconColor hover background action icon_ labelText isActive


buttonInvert action icon_ labelText disabled =
    let
        isActive =
            not disabled

        iconColor =
            if isActive then
                Element.rgba 0.9 0.9 0.9 0.9

            else
                Element.rgba 0.9 0.9 0.9 0.2

        hover =
            Element.rgba 0.7 0.7 0.7 0.8

        background =
            if isActive then
                Element.rgba 0.7 0.7 0.7 0.6

            else
                Element.rgba 0.7 0.7 0.7 0.1
    in
    buttonWith iconColor hover background action icon_ labelText disabled


buttonExpandOnClick action labelText disabled =
    let
        isBold =
            if disabled then
                Element.Font.semiBold

            else
                Element.Font.regular

        adjustedPadding =
            case labelText of
                "" ->
                    Element.paddingEach { bottom = 5, top = 5, right = 10, left = 0 }

                _ ->
                    Element.paddingEach { bottom = 5, top = 5, right = 15, left = 0 }

        icon_ =
            case disabled of
                True ->
                    Ui.Boxicons.bxChevronDown

                False ->
                    Ui.Boxicons.bxChevronRight
    in
    Element.row
        [ Element.Events.Extra.onMouseDown <| Decode.succeed action
        , adjustedPadding
        , Element.centerY
        , Element.Font.color Ui.Style.highlightColorSolid
        , isBold
        ]
        [ icon icon_, Element.el [] (Element.text labelText) ]


smallButtonStyle =
    [ Element.paddingXY 7 4
    , Element.Background.color Ui.Style.black
    , Element.Border.rounded 3
    , Element.Font.color Ui.Style.white
    , Element.Border.width 1
    , Element.mouseOver
        [ Element.Background.color Ui.Style.transparent
        , Element.Border.color Ui.Style.black
        , Element.Font.color Ui.Style.black
        ]
    ]


buttonOnMouseDown action labelText disabled =
    Element.row
        (buttonStyle ++ [ Element.Events.Extra.onMouseDown action ])
        [ buttonLabel labelText ]


buttonOnClick action labelText disabled =
    Element.Input.button
        buttonStyle
        { onPress = Just action
        , label = buttonLabel labelText
        }


activableButton action labelText active =
    Element.Input.button
        (if active then
            hilightedButtonStyle

         else
            buttonStyle
        )
        { onPress = Just action
        , label = buttonLabel labelText
        }


iconOnClick action icon_ isActive =
    let
        activeIcon =
            [ Element.Background.color Ui.Style.lightGrey
            , Element.padding 5
            , Element.Border.rounded 3
            ]

        inactiveIcon =
            [ Element.padding 5
            , Element.Border.rounded 3
            , Element.Border.color Ui.Style.white
            , Element.pointer
            ]

        styles =
            case isActive of
                True ->
                    activeIcon

                False ->
                    inactiveIcon
    in
    Element.Input.button
        styles
        { onPress = Just action
        , label = icon icon_
        }


buttonLink url text_ =
    Element.link buttonStyle { url = url, label = buttonLabel text_ }


buttonLabel labelText =
    case labelText of
        "X" ->
            icon Ui.Boxicons.bxX

        _ ->
            Element.el [ Element.paddingEach { bottom = 2, top = 2, right = 2, left = 2 } ] (Element.text labelText)


buttonStyle =
    [ Element.Border.width 1
    , Element.paddingEach { top = 10, bottom = 10, left = 13, right = 13 }
    , Element.Border.rounded 3
    , Element.mouseOver <|
        [ Element.Background.color Ui.Style.black, Element.Font.color Ui.Style.white ]
    , Element.pointer
    ]


hilightedButtonStyle =
    [ Element.Border.width 0
    , Element.padding 11
    , Element.Border.rounded 3
    , Element.pointer
    , Element.Background.color Ui.Style.black
    , Element.Font.color Ui.Style.white
    ]


overLayHeaderText headerText =
    let
        stylesHeader =
            [ Element.Font.color Ui.Style.highlightColorSolid
            , Element.Font.size 36
            ]
    in
    Element.el stylesHeader (Element.text headerText)


{-| The contenteditable element allows for inline editing
-}
contenteditable : { text : String, placeholder : String, enabled : Bool } -> Element.Element String
contenteditable { text, placeholder, enabled } =
    let
        placeholderElement =
            Html.text placeholder
                |> List.singleton
                |> Html.span [ Html.Attributes.style "opacity" "0.5" ]

        decodeEvent =
            Decode.at [ "target", "innerText" ] Decode.string
                |> Decode.andThen (\a -> Decode.succeed { stopPropagation = True, preventDefault = False, message = a })
    in
    Html.node
        "funk-contenteditable"
        [ Html.Events.custom "input" decodeEvent
        , Html.Attributes.attribute "text" text
        , Html.Attributes.style "cursor" "text"
        ]
        [ Html.span [ Html.Attributes.contenteditable enabled ] []
        , placeholderElement
        ]
        |> Element.html
