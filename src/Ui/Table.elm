module Ui.Table exposing (Row, TableSettings, mapRow, padList, view)

import Element
import Element.Border
import Ui.Style


type alias TableSettings msg =
    { widths : List Element.Length
    , header : List String
    , body : List (Row msg)
    }


type alias Row msg =
    { depth : Int
    , cells : List (Element.Element msg)
    }


mapRow : (msg -> msg2) -> Row msg -> Row msg2
mapRow fn { depth, cells } =
    { depth = depth
    , cells = List.map (Element.map fn) cells
    }


{-|

    padList 4 "A" [ "X" ] --> ["X", "A", "A", "A"]

-}
padList : Int -> a -> List a -> List a
padList toSize with someList =
    let
        countToAdd =
            toSize - List.length someList
    in
    if countToAdd > 0 then
        someList ++ List.repeat countToAdd with

    else
        someList


view : TableSettings msg -> Element.Element msg
view { widths, header, body } =
    let
        columnCount : Int
        columnCount =
            List.length widths

        viewRow : Row msg -> Element.Element msg
        viewRow { depth, cells } =
            let
                viewFn =
                    if List.length cells == 1 then
                        viewEmptyBodyCell

                    else
                        viewBodyCell

                paddedCells =
                    padList columnCount Element.none cells
            in
            List.map2 Tuple.pair widths paddedCells
                |> List.indexedMap (viewFn columnCount depth)
                |> Element.row []

        viewHeader : Element.Element msg
        viewHeader =
            List.map2 Tuple.pair widths header
                |> List.indexedMap (viewHeaderCell columnCount)
                |> Element.row [ borderBottom, greyBorder ]

        viewBody : List (Element.Element msg)
        viewBody =
            List.map viewRow body
    in
    Element.column
        [ Element.width Element.fill ]
        (viewHeader :: viewBody)


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }


viewEmptyBodyCell : Int -> Int -> Int -> ( Element.Length, Element.Element msg ) -> Element.Element msg
viewEmptyBodyCell columnCount depth index ( width, el ) =
    if index == 0 then
        Element.el [ Element.width width, borderLeft, greyBorder ] el

    else
        Element.el [ Element.width width ] el


viewBodyCell : Int -> Int -> Int -> ( Element.Length, Element.Element msg ) -> Element.Element msg
viewBodyCell columnCount depth index ( width, el ) =
    let
        isFirst =
            index == 0

        isLast =
            index == columnCount - 1

        rightBorderIfNotLast =
            if isLast then
                []

            else
                [ greyBorder, borderRight ]
    in
    if isFirst then
        let
            lines =
                Element.el
                    [ Element.Border.widthEach { edges | left = 1 }
                    , Element.Border.color Ui.Style.grey
                    , Element.width (Element.px 30)
                    , Element.height Element.fill
                    , Element.paddingEach { edges | bottom = 10 }
                    ]
                    (Element.el
                        [ Element.Border.widthEach { edges | bottom = 1 }
                        , Element.width Element.fill
                        , Element.alignBottom
                        , Element.Border.color Ui.Style.grey
                        ]
                        Element.none
                    )
        in
        Element.row
            ([ Element.paddingEach { edges | left = depth * 10 }
             , Element.width width
             , Element.height Element.fill
             ]
                ++ rightBorderIfNotLast
            )
            [ lines, el ]

    else
        Element.el
            ([ Element.height Element.fill, Element.width width ] ++ rightBorderIfNotLast)
            el


borderRight =
    Element.Border.widthEach { edges | right = 1 }


borderLeft =
    Element.Border.widthEach { edges | left = 1 }


borderBottom =
    Element.Border.widthEach { edges | bottom = 1 }


greyBorder =
    Element.Border.color Ui.Style.grey


viewHeaderCell : Int -> Int -> ( Element.Length, String ) -> Element.Element msg
viewHeaderCell columnCount index ( width, label ) =
    let
        isLast =
            index == columnCount - 1

        attribs =
            if isLast then
                [ Element.width width, Element.padding 5, Element.height Element.fill ]

            else
                [ Element.Border.color Ui.Style.grey, Element.height Element.fill, Element.width width, Element.padding 5, borderRight ]
    in
    Element.el attribs (Element.text label)
