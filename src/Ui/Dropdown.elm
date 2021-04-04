module Ui.Dropdown exposing
    ( Detail(..)
    , DropdownParams
    , DropdownRowParams
    , Row
    , dividingLine
    , view
    , viewAsLink
    , viewRow
    )

{-| Use this module to render a dropdown.

A dropdown has a button and some contents below it with options.

Note that the inclusion of a custom element is required for it to work.

-}

import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Html
import Ui.Boxicons
import Ui.Component as Component
import Ui.EntypoIcons
import Ui.Style


{-| The params that are required for a dropdown to work
-}
type alias DropdownParams msg =
    { label : String
    , contents : List (Row msg)
    }


viewAsLink : List (Row msg) -> Element.Element msg
viewAsLink rows =
    let
        button =
            Element.el
                [ Ui.Style.class "funk-dropdown-button"
                , Element.pointer
                , Element.scale 0.7
                ]
                (Component.icon Ui.Boxicons.bxLink)

        layoutedDropdown =
            Element.layoutWith
                { options = [ Element.noStaticStyleSheet ] }
                [ contentsPanel
                , Ui.Style.interFont
                , Element.width Element.fill
                , Ui.Style.style "font-size" "inherit"
                ]
                button

        contentsPanel =
            Element.column
                [ Element.Background.color Ui.Style.black
                , Element.Font.color Ui.Style.white
                , Element.width <| Element.minimum 200 <| Element.shrink
                , Element.paddingXY 0 5
                , Ui.Style.class "funk-dropdown-contents"
                , Element.height (Element.shrink |> Element.maximum 600)
                , Element.scrollbarX
                ]
                (rows |> List.map (\(Row row) -> row))
                |> Element.below

        result =
            Html.node
                "funk-dropdown"
                []
                [ layoutedDropdown
                ]
                |> Element.html
    in
    case rows of
        [] ->
            Element.none

        _ ->
            result


{-| Render a dropdown

If attribs are empty, use default styles

-}
view : List (Element.Attribute msg) -> DropdownParams msg -> Element.Element msg
view attribs { label, contents } =
    let
        layoutedDropdown =
            Element.layoutWith
                { options = [ Element.noStaticStyleSheet ] }
                [ contentsPanel
                , Ui.Style.interFont
                , Element.width Element.fill
                , Ui.Style.style "font-size" "inherit"
                ]
                button

        button =
            let
                icon =
                    Element.el
                        (if List.isEmpty attribs then
                            [ Element.Font.color Ui.Style.grey
                            , Element.alignRight
                            , Element.scale 0.7
                            , Element.width (Element.px 18)
                            ]

                         else
                            [ Element.alignRight
                            , Element.scale 0.7
                            , Element.width (Element.px 18)
                            ]
                        )
                    <|
                        Component.icon Ui.EntypoIcons.chevronThinDown
            in
            Element.row
                ([ Ui.Style.class "funk-dropdown-button"
                 , Element.paddingEach { top = 3, left = 10, bottom = 3, right = 3 }
                 , Element.width Element.fill
                 , Element.spacing 2
                 ]
                    |> List.append
                        (if List.isEmpty attribs then
                            defaultDropdownButtonStyle

                         else
                            attribs
                        )
                )
                [ Element.text label
                , icon
                ]

        defaultDropdownButtonStyle =
            [ Element.Border.rounded 3
            , Element.Border.width 1
            , Element.Border.color Ui.Style.transparent
            , Element.Font.size 12
            , Element.mouseOver [ Element.Border.color Ui.Style.grey ]
            ]

        contentsPanel =
            Element.column
                [ Element.Background.color Ui.Style.black
                , Element.Font.color Ui.Style.white
                , Element.width <| Element.minimum 200 <| Element.shrink
                , Element.paddingXY 0 5
                , Ui.Style.class "funk-dropdown-contents"
                , Element.height (Element.shrink |> Element.maximum 600)
                , Element.scrollbarX
                ]
                (contents |> List.map (\(Row row) -> row))
                |> Element.below
    in
    Html.node
        "funk-dropdown"
        []
        [ layoutedDropdown
        ]
        |> Element.html
        |> Element.el [ Element.width Element.fill ]


type alias State =
    { isOpen : Bool
    }


type Row msg
    = Row (Element.Element msg)


{-| All the info a dropdown row needs to know in order to make sense
-}
type alias DropdownRowParams a =
    { isSelected : Bool
    , label : Detail a
    , detail : Detail a
    , sideNote : Detail a
    , onSelect : a
    , rightHandText : Maybe String
    }


type Detail a
    = Description String
    | Preview (Element.Element a)
    | NoDetail


{-| Render a row inside the dropdown contents
-}
viewRow : DropdownRowParams a -> Row a
viewRow { isSelected, label, detail, onSelect, sideNote, rightHandText {- , isInsensitive -} } =
    let
        isInsensitive =
            isSelected

        -- the checkmar icon is hidden when the option is not selected
        checkIcon =
            Element.el
                [ Element.width <|
                    if isSelected then
                        Element.px 20

                    else
                        Element.px 0
                , Element.clip
                ]
            <|
                Element.html <|
                    Ui.Boxicons.bxCheck ""

        -- row with smaller text explaining a bit more about the option
        detailRow =
            case detail of
                Description "" ->
                    Element.none

                NoDetail ->
                    Element.none

                Description str ->
                    Element.paragraph
                        [ Element.paddingXY 5 10
                        , Element.width Element.fill
                        , Element.Font.size 11
                        , Element.Background.color isSelectedBackgroundColor
                        ]
                        [ Element.text str ]

                Preview el ->
                    Element.row
                        [ Element.Background.color isSelectedBackgroundColor
                        , Element.width Element.fill
                        ]
                        [ el ]

        -- row with checkmark and the name of the options
        labelRow =
            Element.row
                [ Element.spacing 5
                , Element.Background.color isSelectedBackgroundColor
                , Element.width Element.fill
                , Element.Font.size 12
                ]
                [ checkIcon
                , case label of
                    Description name ->
                        Element.text name

                    Preview el ->
                        el

                    NoDetail ->
                        Element.none
                , case rightHandText of
                    Nothing ->
                        Element.none

                    Just rightHandText_ ->
                        Element.el [ Element.alignRight, Element.Font.color Ui.Style.greyText ] (Element.text rightHandText_)
                , sideNoteElement
                ]

        sideNoteElement =
            case sideNote of
                Description "" ->
                    Element.none

                NoDetail ->
                    Element.none

                Description str ->
                    Element.el
                        [ Element.alignRight
                        , Element.paddingXY 5 0
                        ]
                        (Element.text str)

                Preview el ->
                    el

        events =
            if isSelected then
                []

            else
                [ Element.Events.onClick onSelect, Element.pointer ]

        isHovered =
            Element.mouseOver
                (if isSelected then
                    []

                 else
                    [ Element.Background.color Ui.Style.highlightColor ]
                )

        isInsensitiveStyle =
            Ui.Style.class
                (if isInsensitive then
                    "funk-insensitive-row"

                 else
                    ""
                )

        isSelectedBackgroundColor =
            if isSelected then
                Ui.Style.highlightColorSolid

            else
                Ui.Style.transparent

        row =
            Element.column
                ([ isHovered
                 , Element.Font.color Ui.Style.white
                 , Element.width Element.fill
                 , isInsensitiveStyle
                 ]
                    ++ events
                )
                [ labelRow
                , detailRow
                ]
    in
    Row row


dividingLine =
    Element.el
        [ Element.Border.widthEach { edges | top = 1 }
        , Element.Border.color Ui.Style.white
        , Element.width Element.fill
        ]
        Element.none
        |> Row



---- UTILS ----


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }
