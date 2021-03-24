module Ui.DesignSystem exposing (..)

{-| Use this module to render the design system menus layout.

The layout has a sidebar formed by selectable rows and an add button as well as
a custom main body.

-}

import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import EventsExtra
import Hover
import Ui.Boxicons
import Ui.Component
import Ui.Style



-- view


{-| Renders the layout of a design system sub menu like Typography or iconBrowser.

It has a sidebar formed by selectable rows and an add button as well as
a custom main body.

-}
type alias Parameters msg =
    { sidebar : Element.Element msg
    , mainBody : Element.Element msg
    }


view : Parameters msg -> Element.Element msg
view { sidebar, mainBody } =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.clipX
        , Element.alignTop
        , Element.Border.widthEach
            { top = 1, right = 0, bottom = 0, left = 0 }
        , Element.padding 0
        ]
        [ Element.el
            [ Element.alignTop
            , Element.alignLeft
            , Element.Border.widthEach
                { top = 0, right = 1, bottom = 0, left = 0 }
            , Element.height Element.fill
            ]
            sidebar
        , Element.el
            [ Element.alignTop
            , Element.alignLeft
            , Element.width Element.fill
            , Element.height Element.fill
            ]
            mainBody
        ]



-- sidebar


type alias SidebarParameters msg =
    { rows : List (Element.Element msg)
    , addButton : Maybe (Element.Element msg)
    }


viewSidebar : SidebarParameters msg -> Element.Element msg
viewSidebar params =
    Element.column
        [ Element.width (Element.px 300)
        , Element.spacing 0
        ]
        ([ case params.addButton of
            Just addButton ->
                Element.el
                    [ Element.height Element.fill
                    , Element.width Element.fill
                    , Element.paddingEach { top = 20, bottom = 20, right = 15, left = 15 }
                    , Element.Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                    , Element.Border.color Ui.Style.black
                    ]
                    addButton

            Nothing ->
                Element.none
         ]
            ++ params.rows
        )



-- row


type alias SimpleRowParams msg =
    { isSelected : Bool
    , label : Element.Element msg
    , msg : msg
    , attribs : List (Element.Attribute msg)
    , onRemove : Maybe msg
    }


viewSidebarRowSimple : SimpleRowParams msg -> Element.Element msg
viewSidebarRowSimple { isSelected, label, msg, attribs, onRemove } =
    Element.row [ Element.width Element.fill ]
        [ Element.row (styleRow isSelected msg ++ attribs)
            [ label ]
        , case onRemove of
            Nothing ->
                Element.none

            Just onRemove_ ->
                Element.el
                    [ removeButton True onRemove_
                    , Element.alignTop
                    ]
                    Element.none
        ]


{-| Renders a row of the sidebar
-}
type alias SidebarRowParameters msg =
    { isSelected : Bool
    , isHovered : Bool
    , title : Element.Element msg
    , preview : Element.Element msg
    , onSelect : msg
    , onRemove : Maybe msg
    , elementRef : String
    , onHover : Hover.Msg -> msg
    , customAttribs : List (Element.Attribute msg)
    }


styleRow isSelected msg =
    [ Element.width Element.fill
    , Element.Background.color
        (if isSelected then
            Ui.Style.highlightColor

         else
            Element.rgb 1 1 1
        )
    , Element.Events.onClick msg
    , Element.pointer
    , Element.padding 20
    , Element.spacing 5
    , Element.Border.widthEach
        { bottom = 1, top = 0, right = 0, left = 0 }
    ]


viewSidebarRow : SidebarRowParameters msg -> Element.Element msg
viewSidebarRow params =
    [ Element.el
        [ Element.Font.size 24
        ]
        params.title
    , Element.el
        [ Element.width Element.fill
        ]
        params.preview
    ]
        |> Element.column
            (styleRow params.isSelected params.onSelect
                |> List.append
                    (case params.onRemove of
                        Nothing ->
                            []

                        Just onRemove ->
                            removeButton
                                params.isHovered
                                onRemove
                                |> List.singleton
                    )
                |> List.append
                    (Hover.attributes params.elementRef
                        |> List.map (Element.mapAttribute params.onHover)
                    )
                |> List.append params.customAttribs
                |> List.append [ Element.height (Element.fill |> Element.maximum 200) ]
            )


{-| Renders a button that, once clicked, changes into a text input to enter the name
of the added element.

Used on the bottom of the sidebar.

-}
type alias AddButtonParameters msg =
    { openFieldMsg : msg
    , updateFieldMsg : String -> msg
    , submitMsg : msg
    , cancelMsg : msg
    , maybeInput : Maybe String
    , thingToAdd : String
    }


buttonStyle : msg -> Int -> List (Element.Attribute msg)
buttonStyle msg padding =
    [ Element.Border.width 1
    , Element.padding padding
    , Element.Border.rounded 3
    , Element.pointer
    , Element.mouseOver <|
        [ Element.Background.color Ui.Style.black, Element.Font.color Ui.Style.white ]
    , Element.Events.onClick msg
    ]


buttonActiveStyle : msg -> String -> Int -> List (Element.Attribute msg)
buttonActiveStyle msg val padding =
    if String.length val >= 1 then
        [ Element.Font.color Ui.Style.black
        , Element.Events.onClick msg
        , Element.mouseOver <|
            [ Element.Background.color Ui.Style.black, Element.Font.color Ui.Style.white ]
        , Element.pointer
        , Element.Border.width 1
        , Element.padding padding
        , Element.Border.rounded 3
        ]

    else
        [ Element.Font.color Ui.Style.grey
        , Element.Border.width 1
        , Element.padding padding
        , Element.Border.rounded 3
        ]


viewBasicAddButton label msg =
    Element.el
        [ Element.Font.size 24
        , Element.padding 12
        , Element.Events.onClick msg
        , Element.pointer
        ]
        (Element.text ("+ add " ++ label))


viewAddButton : AddButtonParameters msg -> Element.Element msg
viewAddButton { openFieldMsg, updateFieldMsg, submitMsg, maybeInput, thingToAdd, cancelMsg } =
    case maybeInput of
        Nothing ->
            Element.el
                (buttonStyle openFieldMsg 10)
                (Element.text ("New " ++ thingToAdd))

        Just val ->
            let
                iconSubmit =
                    Element.el
                        ([ Element.moveRight 5 ] ++ buttonActiveStyle submitMsg val 5)
                        (Element.text "Add")

                iconCancel =
                    Element.el
                        ([ Element.moveRight 5 ] ++ buttonStyle cancelMsg 5)
                        (Element.text "Cancel")
            in
            Element.column [ Element.width Element.fill ]
                [ Element.Input.text
                    [ Element.Font.size 16
                    , Element.width Element.fill
                    , EventsExtra.onEnter submitMsg
                    , Element.Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                    , Element.Border.color Ui.Style.black
                    , Element.Border.rounded 0
                    ]
                    { onChange = updateFieldMsg
                    , text = val
                    , placeholder =
                        Element.Input.placeholder []
                            (Element.text ("New " ++ thingToAdd))
                            |> Just
                    , label = Element.Input.labelHidden "add"
                    }
                , Element.row
                    [ Element.spacingXY 10 0
                    , Element.paddingEach { top = 10, bottom = 10, right = 0, left = 0 }
                    ]
                    [ iconSubmit
                    , iconCancel
                    ]
                ]


removeButton : Bool -> msg -> Element.Attribute msg
removeButton isHovered msg =
    (if not isHovered then
        Element.none

     else
        Element.el
            [ Element.Font.color Ui.Style.grey
            , Element.Font.size 12
            , Element.alignRight
            , Element.moveLeft 27
            , Element.moveDown 6
            , EventsExtra.onClickStopPropagation msg
            ]
            (Element.el [] <|
                Ui.Component.icon Ui.Boxicons.bxTrash
            )
    )
        |> Element.onRight
