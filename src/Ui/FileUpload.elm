module Ui.FileUpload exposing (FileUploadData, Instructions, decodeMedia, fileUploadButton)

import Element
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Persistence


type alias FileUploadData =
    { name : String, path : String, mime : String }


type alias Instructions =
    { empty : String
    , addNew : String
    }


fileUploadButton :
    Persistence.ProjectMeta
    -> Instructions
    -> Maybe (Html.Html msg)
    -> (FileUploadData -> Decode.Decoder msg)
    -> Element.Element msg
fileUploadButton projectMeta instructions preview toMsg =
    let
        emptyInstructions =
            Html.text instructions.empty
                |> List.singleton
                |> Html.div []

        addNewInstructions =
            Html.text instructions.addNew
                |> List.singleton
                |> Html.div []

        viewInstructions =
            case preview of
                Nothing ->
                    emptyInstructions

                Just _ ->
                    addNewInstructions

        innerHtml =
            preview
                |> Maybe.withDefault (Html.text "")
    in
    Element.html <|
        Html.node "funk-media-picker"
            [ Html.Events.on "file-saved"
                (Decode.field "detail" (decodeMedia toMsg))
            , Html.Attributes.property "projectMeta" (Persistence.encodeProjectMeta projectMeta)
            ]
            [ innerHtml, viewInstructions ]


decodeMedia : (FileUploadData -> Decode.Decoder msg) -> Decode.Decoder msg
decodeMedia toMsg =
    Decode.map3 FileUploadData
        (Decode.field "name" Decode.string)
        (Decode.field "path" Decode.string)
        (Decode.field "mime" Decode.string)
        |> Decode.andThen toMsg
