module ApiExplorer.Request exposing (..)

import Html.Parser
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData


readJsonData : Decode.Decoder Decode.Value
readJsonData =
    Decode.field "json" Decode.value


readHtmlContent : Decode.Decoder HtmlResponseData
readHtmlContent =
    Decode.oneOf
        [ Decode.field "html" decodeHtmlResponseData
        , Decode.field "body" decodeHtmlResponseData
        ]


decodeHtmlResponseData : Decode.Decoder (List Html.Parser.Node)
decodeHtmlResponseData =
    Decode.string
        |> Decode.andThen
            (\html ->
                case Html.Parser.run html of
                    Ok nodes ->
                        Decode.succeed nodes

                    Err deadEnds ->
                        Decode.fail "Could not parse"
            )


{-| Read whether a response is a JSON or HTML response and parse the data accordingly.
-}
exploreResponse : Decode.Decoder SuccessData
exploreResponse =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "json" ->
                        readJsonData
                            |> Decode.map DataResponse

                    "html" ->
                        readHtmlContent
                            |> Decode.map HtmlResponse

                    _ ->
                        Decode.fail "Could not recognize type"
            )


type alias Request =
    RemoteData.WebData SuccessData


type alias JsonValue =
    Decode.Value


encodeValue =
    identity


decodeValue =
    Decode.value


encodeHtmlResponseData : HtmlResponseData -> Encode.Value
encodeHtmlResponseData =
    List.map Html.Parser.nodeToString
        >> String.join "\n"
        >> Encode.string



-- [generator-start]


type SuccessData
    = HtmlResponse HtmlResponseData
    | DataResponse JsonValue



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeSuccessData =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeSuccessDataHelp


decodeSuccessDataHelp constructor =
    case constructor of
        "HtmlResponse" ->
            Decode.map
                HtmlResponse
                (Decode.field "A1" decodeHtmlResponseData)

        "DataResponse" ->
            Decode.map
                DataResponse
                (Decode.field "A1" decodeValue)

        other ->
            Decode.fail <| "Unknown constructor for type SuccessData: " ++ other


encodeSuccessData a =
    case a of
        HtmlResponse a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "HtmlResponse" )
                , ( "A1", encodeHtmlResponseData a1 )
                ]

        DataResponse a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "DataResponse" )
                , ( "A1", encodeValue a1 )
                ]



-- [generator-end]


type alias HtmlResponseData =
    Nodes


type alias Nodes =
    List Html.Parser.Node


encodeRequest : Request -> Encode.Value
encodeRequest r =
    case r of
        RemoteData.Success v ->
            Encode.object [ ( "success", encodeSuccessData v ) ]

        _ ->
            Encode.null


decodeRequest : Decode.Decoder Request
decodeRequest =
    Decode.oneOf
        [ Decode.field "success" decodeSuccessData |> Decode.map RemoteData.Success
        , Decode.succeed RemoteData.NotAsked
        ]
