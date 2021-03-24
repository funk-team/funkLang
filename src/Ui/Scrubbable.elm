module Ui.Scrubbable exposing (float, text)

import Element
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode


text : String -> Int -> Element.Element Int
text label value =
    let
        handler =
            Html.Events.on "input"
                (Decode.field "detail" Decode.int
                    |> Decode.map (\movement -> value + movement)
                )

        style =
            Html.Attributes.style "cursor" "ew-resize"
    in
    Element.html
        (Html.node "funk-scrubbable"
            [ handler
            , style
            ]
            [ Html.text label
            ]
        )
        |> Element.el []


{-| Render an integer and emit an updated integer when scrubbing
-}
float : Float -> Element.Element Float
float value =
    let
        handler =
            Html.Events.on "input"
                (Decode.field "detail" Decode.float
                    |> Decode.map (\movement -> value + movement)
                )

        style =
            Html.Attributes.style "cursor" "ew-resize"
    in
    Element.html
        (Html.node "funk-scrubbable"
            [ handler
            , style
            ]
            [ Html.text (String.fromInt (round value))
            ]
        )
        |> Element.el []
