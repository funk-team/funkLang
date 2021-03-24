module ApiExplorer.Mock exposing (..)

{-| A GUI for communication with backends.
Example: GET JSON to dict tree data structure with labels defined by the user
-}

import ApiExplorer.Help
import ApiExplorer.Model
import ApiExplorer.Msg
import ApiExplorer.Request
import Element
import Element.Border
import Element.Events
import Element.Font
import Json.Decode as Decode
import MimeType
import Persistence
import Regex
import RemoteData
import RemoteData.Http
import Ui.Boxicons
import Ui.Component
import Ui.FileUpload
import Ui.Style
import Url.Builder



---- URL PICKER ----


topSection : Persistence.ProjectMeta -> ApiExplorer.Model.ApiSpec -> Element.Element ApiExplorer.Msg.ApiSpecEdit
topSection projectMeta apiSpec =
    let
        explenation =
            "1. Mock an API response by uploading a JSON file, you can interact with it just like http response."
    in
    ApiExplorer.Help.viewSection
        explenation
        [ urlPicker projectMeta apiSpec
        ]


urlPicker : Persistence.ProjectMeta -> ApiExplorer.Model.ApiSpec -> Element.Element ApiExplorer.Msg.ApiSpecEdit
urlPicker projectMeta apiSpec =
    let
        submitButton =
            Ui.Component.buttonOnClick ApiExplorer.Msg.MakeRequestButtonClicked "Make Request" False

        clearButton =
            Ui.Component.buttonOnClick ApiExplorer.Msg.ClearResponse "Clear Request" False
    in
    Element.column
        [ Element.spacing 15
        , Element.width Element.fill
        ]
        [ fileUploadButton projectMeta apiSpec.mockUrl
        , if apiSpec.mockUrl == "" then
            case apiSpec.request of
                RemoteData.Success _ ->
                    clearButton

                _ ->
                    Element.none

          else
            Element.row [ Element.spacingXY 20 0 ]
                [ submitButton
                , case apiSpec.request of
                    RemoteData.Success _ ->
                        clearButton

                    _ ->
                        Element.none
                ]
        ]


fileUploadButton projectMeta url =
    let
        removeButton =
            Element.el
                [ Element.centerY
                , Element.Font.color Ui.Style.grey
                , Element.Events.onClick (ApiExplorer.Msg.MockFileUploaded "")
                , Element.padding 10
                ]
                (Ui.Component.icon Ui.Boxicons.bxTrash)

        fileUp =
            Ui.FileUpload.fileUploadButton
                projectMeta
                { empty = "Drag a JSON file here or click to upload"
                , addNew = "Drag a JSON file here or click to upload to replace" -- never happens
                }
                Nothing
                (\{ name, path, mime } ->
                    case MimeType.parseMimeType mime of
                        Just (MimeType.Text MimeType.Json) ->
                            Decode.succeed { title = name, src = path }

                        _ ->
                            Decode.fail "You need a .json file. The one you uploaded is not."
                )
                |> Element.el [ Element.height (Element.px 140), Element.width (Element.px 330) ]
                |> Element.map
                    (\{ title, src } ->
                        ApiExplorer.Msg.MockFileUploaded src
                    )

        parsedFileName str =
            case
                Regex.find
                    (Regex.fromString "^root>(.+).\\w{32}.json$"
                        |> Maybe.withDefault Regex.never
                    )
                    str
            of
                [ { submatches } ] ->
                    case submatches of
                        [ Just fileName ] ->
                            fileName ++ ".json"

                        _ ->
                            "Uploaded"

                _ ->
                    "Uploaded"
    in
    if url == "" then
        fileUp

    else
        Element.row
            [ Element.Border.color Ui.Style.grey
            , Element.Border.width 1
            , Element.Border.rounded 5
            ]
            [ Element.el
                [ Element.padding 10 ]
                (Element.text (parsedFileName url))
            , Element.el
                [ Element.Border.widthEach { edges | right = 1 }
                , Element.Border.color Ui.Style.grey
                , Element.height Element.fill
                ]
                Element.none
            , removeButton
            ]


get :
    String
    -> Cmd ApiExplorer.Msg.ApiSpecEdit
get url =
    let
        -- when the URL starts with a / we do not want to use the proxy
        -- this allows us to request assets from the browser filesystem
        withProxy =
            case String.startsWith "root>" url of
                True ->
                    Url.Builder.absolute
                        [ "api", "reverse-proxy" ]
                        [ Url.Builder.string "url" ("/" ++ url) ]

                False ->
                    ""

        decoder =
            Decode.oneOf
                [ ApiExplorer.Request.readJsonData
                , Decode.value
                ]
    in
    RemoteData.Http.get
        withProxy
        ApiExplorer.Msg.GotResponse
        decoder


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }
