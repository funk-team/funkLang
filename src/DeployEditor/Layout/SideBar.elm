module DeployEditor.Layout.Sidebar exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import EventsExtra
import Hover


type alias Parameters msg =
    { isSelected : Bool
    , isHovered : Bool
    , title : Element.Element msg
    , preview : Element.Element msg
    , onSelect : msg
    , onRemove : Maybe msg
    , elementRef : String
    , onHover : Hover.Msg -> msg
    }


columnAttributes : List (Element.Attribute msg)
columnAttributes =
    [ Element.width (Element.px 300)
    , Element.alignTop
    , Element.spacing 12
    ]


viewElem : Parameters msg -> Element.Element msg
viewElem params =
    let
        elementReference =
            "iconBrowser.viewSidebarElem:"
                ++ params.elementRef
    in
    [ Element.el
        [ Element.Font.size 24
        , Element.padding 12
        ]
        params.title
    , Element.el
        [ Element.paddingEach
            { top = 0
            , right = 12
            , bottom = 24
            , left = 12
            }
        , Element.width Element.fill
        , Element.clipX
        ]
        params.preview
    ]
        |> Element.column
            ([ Element.width Element.fill
             , Element.Background.color
                (if params.isSelected then
                    Element.rgb255 210 233 234

                 else
                    Element.rgb 1 1 1
                )
             , Element.Events.onClick params.onSelect
             , Element.pointer
             ]
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
                    (Hover.attributes elementReference
                        |> List.map (Element.mapAttribute params.onHover)
                    )
            )



-- utils


removeButton : Bool -> msg -> Element.Attribute msg
removeButton isHovered msg =
    (if not isHovered then
        Element.none

     else
        Element.el
            [ Element.Background.color (Element.rgb 0 0 0)
            , Element.Border.rounded 100
            , Element.Border.glow (Element.rgba255 100 100 100 0.3) 3
            , Element.Font.size 12
            , Element.Font.color (Element.rgb 1 1 1)
            , Element.alignRight
            , Element.moveLeft 24
            , Element.moveDown 6
            , EventsExtra.onClickStopPropagation msg
            ]
            (Element.el
                [ Element.padding 3 ]
                (Element.text " X ")
            )
    )
        |> Element.onRight
