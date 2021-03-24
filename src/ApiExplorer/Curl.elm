module ApiExplorer.Curl exposing (view)

import ApiExplorer.Api.UrlParser
import ApiExplorer.Help
import ApiExplorer.Model
import ApiExplorer.Msg
import Dict
import Element
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Ui.Component
import Ui.Style


decodeCurl : Decode.Decoder ApiExplorer.Model.CurlParseResult
decodeCurl =
    Decode.map4
        ApiExplorer.Model.CurlParseResult
        (Decode.field "url" Decode.string |> Decode.map ApiExplorer.Api.UrlParser.Raw)
        (Decode.field "method" Decode.string |> Decode.andThen parseMethod)
        (Decode.field "header" (Decode.dict Decode.string) |> Decode.map Dict.toList)
        (Decode.oneOf [ Decode.field "body" Decode.string, Decode.field "body" Decode.value |> Decode.map (Encode.encode 2), Decode.succeed "" ])


parseMethod : String -> Decode.Decoder ApiExplorer.Model.RequestMethod
parseMethod method =
    case method of
        "GET" ->
            Decode.succeed ApiExplorer.Model.Get

        "POST" ->
            Decode.succeed ApiExplorer.Model.Post

        _ ->
            Decode.fail <| "Method: '" ++ method ++ "' not recognized"


parseCurl : String -> Maybe ApiExplorer.Model.CurlParseResult
parseCurl input =
    Result.toMaybe
        (Decode.decodeString decodeCurl input)


view : ApiExplorer.Model.ApiSpec -> Element.Element ApiExplorer.Msg.ApiSpecEdit
view source =
    ApiExplorer.Help.viewSection
        "1. Paste a cURL command here"
        [ curlInput source.curl.input
            |> Element.map ApiExplorer.Msg.UpdateCurl
        , case source.curl.parsed of
            Nothing ->
                Element.none

            Just parseResult ->
                submitButton parseResult
        , case source.curl.parsed of
            Nothing ->
                ApiExplorer.Help.viewValidCheck False

            Just parseResult ->
                ApiExplorer.Help.viewValidCheck True
        ]


submitButton parsed =
    Ui.Component.buttonOnClick (ApiExplorer.Msg.ApplyCurlButtonClicked parsed) "Use parsed curl" False


decodeInput : Decode.Decoder ApiExplorer.Model.CurlModel
decodeInput =
    Decode.map2
        ApiExplorer.Model.CurlModel
        (Decode.at [ "target", "value" ] Decode.string)
        (Decode.oneOf
            [ Decode.at [ "target", "funk_curl_parsed_input" ] decodeCurl |> Decode.map Just
            , Decode.succeed Nothing
            ]
        )


{-| We are using a specail curl input here because elm-ui does not support custom decoders
-}
curlInput curlCmd =
    Html.textarea
        [ Html.Attributes.value curlCmd
        , Html.Events.on "input" decodeInput
        , Html.Attributes.class "cp hf pad-10-13-10-13 spacing-5-5 s e wf mv-0--510-0"
        , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.05)"
        , Html.Attributes.style "padding" "10px 10px"
        , Html.Attributes.style "height" "300px"
        , Html.Attributes.style "border-radius" "2px"
        ]
        []
        |> Element.html
        |> Element.el [ Ui.Style.monospace, Element.width Element.fill, Element.paddingEach { top = 0, bottom = 10, left = 0, right = 0 } ]
