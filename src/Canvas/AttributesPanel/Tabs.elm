module Canvas.AttributesPanel.Tabs exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode



---- DATA STRUCTURES ----


type Tab
    = Layout
    | Style
    | Content
    | Actions



---- USEFUL ----


tabs =
    [ Layout, Style, Content, Actions ]



---- UTILS ----


tabToString t =
    case t of
        Layout ->
            "Layout"

        Style ->
            "Style"

        Content ->
            "Content"

        Actions ->
            "Actions"



---- ENCODERS & DECODERS ----


decodeTab =
    let
        recover x =
            case x of
                "Layout" ->
                    Decode.succeed Layout

                "Style" ->
                    Decode.succeed Style

                "Content" ->
                    Decode.succeed Content

                "Actions" ->
                    Decode.succeed Actions

                other ->
                    Decode.fail <| "Unknown constructor for type Tab: " ++ other
    in
    Decode.string |> Decode.andThen recover


encodeTab a =
    case a of
        Layout ->
            Encode.string "Layout"

        Style ->
            Encode.string "Style"

        Content ->
            Encode.string "Content"

        Actions ->
            Encode.string "Actions"
