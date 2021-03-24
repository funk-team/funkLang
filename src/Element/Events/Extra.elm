module Element.Events.Extra exposing (..)

{-| Additional Events for the editor ui
-}

import Element
import Html.Events
import Json.Decode


{-| Replicates the onClick function but prevents bubbling
-}
onClickNoBubble : msg -> Element.Attribute msg
onClickNoBubble message =
    Json.Decode.succeed { stopPropagation = False, preventDefault = False, message = message }
        |> Html.Events.custom "click"
        |> Element.htmlAttribute


{-| Replicates the onClick function but prevents bubbling
-}
onClickStopPropagation : msg -> Element.Attribute msg
onClickStopPropagation message =
    Json.Decode.succeed { stopPropagation = True, preventDefault = False, message = message }
        |> Html.Events.custom "click"
        |> Element.htmlAttribute


{-| Replicates the onClick function but prevents bubbling
-}
onKeydownStopPropagation : msg -> Element.Attribute msg
onKeydownStopPropagation message =
    Json.Decode.succeed { stopPropagation = True, preventDefault = False, message = message }
        |> Html.Events.custom "keyup"
        |> Element.htmlAttribute


{-| Replicates the onClick function but prevents bubbling
-}
onClickoutside : msg -> Element.Attribute msg
onClickoutside message =
    Html.Events.on "clickoutside" (Json.Decode.succeed message)
        |> Element.htmlAttribute


{-| Replicates the onClick function but prevents bubbling
-}
onMousedownoutside : msg -> Element.Attribute msg
onMousedownoutside message =
    Html.Events.on "mousedownoutside" (Json.Decode.succeed message)
        |> Element.htmlAttribute


onMouseDown : Json.Decode.Decoder msg -> Element.Attribute msg
onMouseDown decoder =
    Json.Decode.map (\val -> { stopPropagation = False, preventDefault = False, message = val }) decoder
        |> Html.Events.custom "mousedown"
        |> Element.htmlAttribute


onMouseUp : Json.Decode.Decoder msg -> Element.Attribute msg
onMouseUp decoder =
    Json.Decode.map (\val -> { stopPropagation = False, preventDefault = False, message = val }) decoder
        |> Html.Events.custom "mouseup"
        |> Element.htmlAttribute


onMouseMove : Json.Decode.Decoder msg -> Element.Attribute msg
onMouseMove decoder =
    Json.Decode.map (\val -> { stopPropagation = False, preventDefault = False, message = val }) decoder
        |> Html.Events.custom "mousemove"
        |> Element.htmlAttribute


onMouseLeave : Json.Decode.Decoder msg -> Element.Attribute msg
onMouseLeave decoder =
    Json.Decode.map (\val -> { stopPropagation = False, preventDefault = False, message = val }) decoder
        |> Html.Events.custom "mouseleave"
        |> Element.htmlAttribute


onClickDecode : Json.Decode.Decoder msg -> Element.Attribute msg
onClickDecode decoder =
    Json.Decode.map (\val -> { stopPropagation = False, preventDefault = False, message = val }) decoder
        |> Html.Events.custom "click"
        |> Element.htmlAttribute


preventClickOnElementsBehind : msg -> List (Element.Attribute msg)
preventClickOnElementsBehind msg =
    let
        opts =
            { stopPropagation = False, preventDefault = False, message = msg }

        down =
            Json.Decode.map (always opts) (Json.Decode.succeed msg)
                |> Html.Events.custom "mousedown"
                |> Element.htmlAttribute
    in
    [ down ]
