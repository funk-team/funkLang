module Canvas.AttributesPanel.Content.Tabs exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


availableTabs =
    [ Text, Model, Api, Icons, Media ]


contentTabToString t =
    case t of
        Model ->
            "Model"

        --
        Api ->
            "Api"

        --
        Media ->
            "Media"

        Text ->
            "Text"

        Icons ->
            "Icons"



---- DATA STRUCTURES ----
-- [generator-start]


type ContentTab
    = Model
    | Api
    | Media
    | Text
    | Icons



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeContentTab =
    let
        recover x =
            case x of
                "Model" ->
                    Decode.succeed Model

                "Api" ->
                    Decode.succeed Api

                "Media" ->
                    Decode.succeed Media

                "Text" ->
                    Decode.succeed Text

                "Icons" ->
                    Decode.succeed Icons

                other ->
                    Decode.fail <| "Unknown constructor for type ContentTab: " ++ other
    in
    Decode.string |> Decode.andThen recover


encodeContentTab a =
    case a of
        Model ->
            Encode.string "Model"

        Api ->
            Encode.string "Api"

        Media ->
            Encode.string "Media"

        Text ->
            Encode.string "Text"

        Icons ->
            Encode.string "Icons"



-- [generator-end]
