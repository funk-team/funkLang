module Canvas.AttributesPanel.Content.Static exposing (viewMedia, viewTextEditor)

{-| Allow the user to manually add and edit content rather than retrieving it from an API or so.
-}

import Element
import Element.Font
import Element.Input
import Html
import Interface.Data
import Json.Decode as Decode
import Maybe.Extra
import MimeType
import Persistence
import Spec.DataConnection
import Ui.Boxicons
import Ui.Component
import Ui.FileUpload
import Ui.Input
import Ui.Style


viewTextEditor : Maybe Interface.Data.RefinedValue -> Element.Element (Maybe Spec.DataConnection.DataConnection)
viewTextEditor val =
    let
        textInputValue =
            case val of
                Just (Interface.Data.ParagraphText text) ->
                    text

                _ ->
                    ""

        textInput =
            Element.Input.multiline
                [ Element.width Element.fill ]
                { placeholder = Nothing
                , label = Element.Input.labelAbove [] (Element.text "Text")
                , spellcheck = True
                , onChange =
                    \str ->
                        case str of
                            "" ->
                                Nothing

                            _ ->
                                Interface.Data.ParagraphText str
                                    |> Spec.DataConnection.Static
                                    |> Just
                , text = textInputValue
                }
    in
    Element.column [ Element.width Element.fill, Element.paddingXY 10 19, Element.spacing 10 ]
        [ textInput
        , Element.paragraph [] [ Element.text "Support for inline styling coming soon." ]
        , Element.paragraph [] [ Element.text "Style text via the Style tab." ]
        ]


viewYoutube : Maybe Interface.Data.RefinedValue -> Element.Element (Maybe Spec.DataConnection.DataConnection)
viewYoutube val =
    let
        trimVideoId : String -> String
        trimVideoId =
            String.slice 0 11

        textInputValue =
            case val of
                Just (Interface.Data.YoutubeEmbed { videoId }) ->
                    videoId

                _ ->
                    ""

        textInput =
            Ui.Input.string "Youtube video ID" "y8OnoxKotPQ" textInputValue
                |> Element.map
                    (\newVideoId ->
                        case newVideoId of
                            "" ->
                                Nothing

                            _ ->
                                Interface.Data.YoutubeEmbed { videoId = trimVideoId newVideoId }
                                    |> Spec.DataConnection.Static
                                    |> Just
                    )
    in
    textInput
        |> Element.el [ Element.width (Element.px 100), Element.padding 5 ]


viewMedia :
    Persistence.ProjectMeta
    -> Maybe Interface.Data.RefinedValue
    -> Element.Element (Maybe Spec.DataConnection.DataConnection)
viewMedia projectMeta val =
    let
        media =
            case val of
                Just (Interface.Data.Media mediaDetails) ->
                    Just mediaDetails

                _ ->
                    Nothing

        hasMedia =
            Maybe.Extra.isJust media

        ui =
            Element.column
                [ Element.width Element.fill, Element.spacing 10 ]
                [ viewMediaFile
                , if hasMedia then
                    Element.none

                  else
                    viewYoutube val
                ]

        preview : Maybe (Html.Html msg)
        preview =
            case media of
                Nothing ->
                    Nothing

                Just { kind } ->
                    Interface.Data.mediaKindToIcon kind
                        |> Element.layoutWith
                            { options = [ Element.noStaticStyleSheet ] }
                            [ Element.height Element.shrink
                            , Element.width Element.shrink
                            , Ui.Style.style "min-height" "0"
                            ]
                        |> Just

        viewMediaFile =
            let
                removeButton =
                    if hasMedia then
                        Ui.Component.button Nothing Ui.Boxicons.bxTrash "Remove Content" False

                    else
                        Element.none

                retitleInput =
                    case media of
                        Nothing ->
                            Element.none

                        Just ({ meta, kind } as mediaDetails) ->
                            let
                                ( label, description ) =
                                    case kind of
                                        Interface.Data.Image ->
                                            ( "<img> alt tag"
                                            , Element.paragraph [ Element.Font.color Ui.Style.grey ]
                                                [ Element.text "Give this image a descriptive caption. This helps search engines and visually impaired users understanding your site."
                                                ]
                                            )

                                        Interface.Data.Audio ->
                                            ( "Audio Name", Element.none )

                                        Interface.Data.Video ->
                                            ( "Video Name", Element.none )

                                        Interface.Data.Pdf ->
                                            ( "Pdf Name", Element.none )

                                update newTitle =
                                    Spec.DataConnection.Media
                                        { mediaDetails
                                            | meta =
                                                { meta | title = newTitle }
                                        }
                                        |> Just

                                input =
                                    Ui.Input.string label "Portrait of Martin Luther King" meta.title
                                        |> Element.map update
                            in
                            Element.column
                                [ Element.spacing 10
                                , Element.width Element.fill
                                ]
                                [ input
                                , description
                                ]

                aspectRatioHint =
                    Element.paragraph
                        [ Element.alpha 0.5 ]
                        [ Element.text "TIP: You can set the aspect ratio of images and videos added here in the Style panel." ]
            in
            Element.column
                [ Element.width Element.fill, Element.padding 5, Element.spacing 10 ]
            <|
                [ fileUploadButton projectMeta preview
                    |> Element.el [ Element.width Element.fill ]
                    |> Element.map Just
                , aspectRatioHint
                , if hasMedia then
                    retitleInput

                  else
                    Element.none
                , removeButton
                ]
    in
    ui


fileUploadButton : Persistence.ProjectMeta -> Maybe (Html.Html Spec.DataConnection.DataConnection) -> Element.Element Spec.DataConnection.DataConnection
fileUploadButton projectMeta preview =
    Ui.FileUpload.fileUploadButton
        projectMeta
        { empty = "Drop a file here or click to upload. We currently support Images, Video, Audio and PDFs."
        , addNew = "File Selected. Drop a new file here or click to upload. We currently support Images, Audio and PDFs."
        }
        preview
        bakeMedia


bakeMedia : Ui.FileUpload.FileUploadData -> Decode.Decoder Spec.DataConnection.DataConnection
bakeMedia { name, path, mime } =
    case MimeType.parseMimeType mime of
        Nothing ->
            Decode.fail "could not recognize mime type"

        Just mimeType ->
            Decode.map Spec.DataConnection.Media <|
                case mimeType of
                    MimeType.Audio _ ->
                        { meta = { title = name, src = path }
                        , kind = Interface.Data.Audio
                        }
                            |> Decode.succeed

                    MimeType.Video _ ->
                        { meta = { title = name, src = path }
                        , kind = Interface.Data.Video
                        }
                            |> Decode.succeed

                    MimeType.App MimeType.Pdf ->
                        { meta = { title = name, src = path }
                        , kind = Interface.Data.Pdf
                        }
                            |> Decode.succeed

                    MimeType.Image _ ->
                        { meta = { title = name, src = path }
                        , kind = Interface.Data.Image
                        }
                            |> Decode.succeed

                    _ ->
                        Decode.fail "could not recognize mime type"
