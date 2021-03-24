module Ui.Table2 exposing (button, buttonNoHover, dropdown, headerText, icon, input, line, textCell, textCellNoLine, view, viewExample)

{-| A ui component that is suitable for displaying lists of normalized data
-}

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Ui.Boxicons
import Ui.Component
import Ui.Dropdown
import Ui.Style


view =
    table [ Ui.Style.class "funk-table", Element.width Element.fill ]


{-| Same API as Element.table except that currently it does not support column widths
-}
table :
    List (Element.Attribute msg)
    ->
        { data : List records
        , columns : List (Element.Column records msg)
        }
    -> Element.Element msg
table attribs { columns, data } =
    let
        complete =
            Html.table [] [ head, body ]

        toHtml =
            Element.layoutWith
                { options = [ Element.noStaticStyleSheet ] }
                (Ui.Style.style "position" "initial" :: Ui.Style.base)

        headers : List (Html.Html msg)
        headers =
            List.map
                (\{ header } -> toHtml header |> List.singleton |> Html.th [])
                columns

        head =
            Html.thead [] headers

        {- View the data for each column -}
        viewCell : records -> Element.Column records msg -> Html.Html msg
        viewCell oneRecord column =
            column.view oneRecord
                |> toHtml
                |> List.singleton
                |> Html.td []

        viewRecord rec =
            List.map
                (viewCell rec)
                columns
                |> Html.tr []

        body =
            Html.tbody [] (List.map viewRecord data)
    in
    complete
        |> Element.html
        |> Element.el (Ui.Style.class "funk-table" :: attribs)



-- HEADERS


headerText text =
    Element.el
        [ Element.width Element.fill
        , Element.paddingXY 5 5
        , Element.Font.alignLeft
        , Element.Font.regular
        , Element.alpha 0.5
        ]
        (Element.text text)



-- DIFFERENT CELL TYPES


textCell text =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 10
        , Element.paddingXY 5 5
        ]
        [ Element.text text
        , line
        ]


textCellNoLine text =
    Element.el
        [ Element.width Element.fill
        , Element.paddingXY 5 5
        ]
    <|
        Element.text text


line =
    Element.el
        [ Element.width (Element.fill |> Element.minimum 50)
        , Element.height (Element.px 1)
        , Element.Background.color Ui.Style.slighterAccent
        , Ui.Style.class "funk-table-line"
        ]
        Element.none


dropdown { label, contents } =
    let
        styles =
            [ Element.mouseOver [ Element.Background.color Ui.Style.slighterAccent ]
            ]
    in
    Ui.Dropdown.view styles { label = label, contents = contents }


icon =
    Ui.Component.icon
        >> Element.el
            [ Element.height <| Element.px 16
            ]


input : { text : String, placeholder : String } -> Element.Element String
input { text, placeholder } =
    let
        editable =
            Ui.Component.contenteditable { text = text, placeholder = placeholder, enabled = True }
                |> Element.el
                    [ Element.mouseOver [ Element.Background.color Ui.Style.slighterAccent ]
                    , Element.height Element.fill
                    , Element.paddingXY 5 5
                    ]
    in
    Element.row [ Element.width Element.fill ] [ editable, line ]


button label msg =
    Element.Input.button
        [ Element.height Element.fill
        , Element.Background.color Ui.Style.white
        , Element.paddingXY 5 3
        , Element.Border.rounded 2
        , Element.alpha 0
        , Element.centerX
        , Element.centerY
        , Element.Font.size 14
        , Element.mouseOver
            [ Element.Font.color Ui.Style.white, Element.Background.color Ui.Style.black ]
        , Ui.Style.class "funk-table-button"
        ]
        { onPress = Just msg, label = label }
        |> Element.el [ Element.height Element.fill, Element.paddingXY 2 0 ]


buttonNoHover label msg =
    Element.Input.button
        [ Element.height Element.fill
        , Element.paddingXY 5 3
        , Element.Border.width 1
        , Element.Border.rounded 2
        , Element.Border.color Ui.Style.black
        , Element.alpha 0
        , Element.centerX
        , Element.centerY
        , Element.Font.color Ui.Style.black
        , Element.Font.size 14
        , Element.mouseOver
            [ Element.Font.color Ui.Style.white, Element.Background.color Ui.Style.black ]
        , Ui.Style.class "funk-table-button-no-hover"
        ]
        { onPress = Just msg, label = label }
        |> Element.el [ Element.height Element.fill, Element.paddingXY 2 0 ]



-- EXAMPLE


type alias Person =
    { firstName : String
    , lastName : String
    , maybeAlive : Bool
    , notes : String
    }


persons : List Person
persons =
    [ { firstName = "David"
      , lastName = "Bowie"
      , maybeAlive = True
      , notes = "Test2"
      }
    , { firstName = "Florence"
      , lastName = "Welch"
      , maybeAlive = False
      , notes = ""
      }
    , { firstName = "David"
      , lastName = "Bowie"
      , maybeAlive = True
      , notes = "Test2"
      }
    , { firstName = "Florence"
      , lastName = "Welch"
      , maybeAlive = True
      , notes = ""
      }
    , { firstName = "Florence"
      , lastName = "Welch"
      , maybeAlive = False
      , notes = ""
      }
    , { firstName = "Florence"
      , lastName = "Welch"
      , maybeAlive = False
      , notes = ""
      }
    , { firstName = "Florence"
      , lastName = "Welch"
      , maybeAlive = False
      , notes = ""
      }
    ]


viewExample =
    view
        { data = persons
        , columns =
            [ { header = headerText "First Name"
              , width = Element.px 120
              , view =
                    \person ->
                        textCell person.firstName
              }
            , { header = headerText "Last Name"
              , width = Element.fill
              , view =
                    \person ->
                        let
                            data =
                                [ "another name", "wquy;fpn", "another name", "wquy;fpn" ]

                            viewRow str =
                                Ui.Dropdown.viewRow
                                    { isSelected = False
                                    , label = Ui.Dropdown.Description str
                                    , sideNote = Ui.Dropdown.Description str
                                    , onSelect = str
                                    , detail = Ui.Dropdown.Description str
                                    , rightHandText = Nothing
                                    }
                        in
                        dropdown
                            { label = person.lastName
                            , contents = List.map viewRow data
                            }
                            |> Element.map (always ())
              }
            , { header = headerText "Notes"
              , width = Element.fill
              , view =
                    \{ notes } ->
                        input { text = notes, placeholder = "Take a note" }
                            |> Element.map (always ())
              }
            , { header = headerText "Maybe alive"
              , width = Element.fill
              , view =
                    \{ maybeAlive } ->
                        if maybeAlive then
                            icon Ui.Boxicons.bxCheck
                                |> Element.el [ Element.width (Element.fill |> Element.maximum 80) ]

                        else
                            button (Element.text "set") ()
                                |> Element.el [ Element.width (Element.fill |> Element.maximum 80) ]
              }
            ]
        }
