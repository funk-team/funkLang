module EventsExtra exposing (..)

import Element
import Html.Events
import Json.Decode as Decode


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


onClickStopPropagation : msg -> Element.Attribute msg
onClickStopPropagation msg =
    Html.Events.custom "click" (Decode.succeed { message = msg, stopPropagation = True, preventDefault = False })
        |> Element.htmlAttribute
