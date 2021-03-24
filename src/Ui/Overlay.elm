module Ui.Overlay exposing (view)

import Element
import Element.Background
import Element.Font
import Ui.Style


type alias Style =
    ( String, String )


viewOpenStyles isOpen =
    Ui.Style.styles <|
        if isOpen then
            openStyles

        else
            closedStyles


wrapStyles =
    [ Element.Background.color Ui.Style.lightGrey
    , Element.Font.color Ui.Style.black
    , Element.padding 50
    , Element.spacing 50
    , Element.width Element.fill
    , Ui.Style.style "height" "calc(100% - 100px)"
    , Element.moveDown 100
    , Element.scrollbarY
    ]


type alias ElmUiElement msg =
    List (Element.Attribute msg)
    -> List (Element.Element msg)
    -> Element.Element msg


view : ElmUiElement msg -> Bool -> List (Element.Element msg) -> Element.Attribute msg
view el isOpen content =
    let
        styles =
            wrapStyles ++ viewOpenStyles isOpen
    in
    if isOpen then
        el styles content
            |> Element.inFront

    else
        Element.none |> Element.inFront


closedStyles : List Style
closedStyles =
    [ ( "transform", "rotateX(0deg) rotateY(0deg) rotateZ(0.5deg) scale(0.99) skewX(-0.5deg) skewY(-0.5deg)" )
    , ( "opacity", "0" )
    , ( "pointer-events", "none" )
    , ( "transition", "all 0.25s " ++ Ui.Style.swiftOut )
    ]


openStyles : List Style
openStyles =
    [ ( "opacity", "1" )
    , ( "transition", "all 0.2s " ++ Ui.Style.swiftOut )
    ]
