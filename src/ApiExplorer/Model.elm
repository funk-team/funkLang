module ApiExplorer.Model exposing (..)

import ApiExplorer.Api.UrlParser
import ApiExplorer.Menus
import Dict.Any
import IntDict
import IntDict.Typed
import Interface.JsonTree.Model
import Interface.Selection
import Json.Decode as Decode
import Json.Decode.Extra as Extra
import Json.Encode as Encode
import RemoteData


removeApiSpec key model =
    { model | sources = IntDict.Typed.remove unwrapKey key model.sources }


init : Model
init =
    { sources = IntDict.Typed.empty
    , selectedApiSpec = Nothing
    , menu = ApiExplorer.Menus.APIs
    , helpOpen = False
    }


encodeModel =
    encodeModelV2


wrapKey =
    ApiCallKey


unwrapKey (ApiCallKey k) =
    k


addApiSpec model =
    let
        key =
            IntDict.Typed.nextId wrapKey model.sources

        source =
            defaultApiSpec key
    in
    { model
        | sources = IntDict.Typed.insert unwrapKey key source model.sources
        , selectedApiSpec = Just key
    }


listApiSpecs : Model -> List ( ApiCallKey, ApiSpec )
listApiSpecs { sources } =
    IntDict.Typed.toList wrapKey sources


getApiSpec : ApiCallKey -> Model -> Maybe ApiSpec
getApiSpec sourceId { sources } =
    IntDict.Typed.get unwrapKey sourceId sources


setApiSpec : ApiCallKey -> ApiSpec -> Model -> Model
setApiSpec sourceId source model =
    { model
        | sources = IntDict.Typed.insert unwrapKey sourceId source model.sources
    }


mapApiSpec : ApiCallKey -> (ApiSpec -> ApiSpec) -> Model -> Model
mapApiSpec sourceId fn model =
    { model
        | sources =
            IntDict.Typed.update
                unwrapKey
                sourceId
                (Maybe.map fn)
                model.sources
    }


type alias ApiSpecDict =
    IntDict.Typed.Typed ApiCallKey ApiSpec


decodeApiSpecDict =
    IntDict.Typed.decodeTyped decodeApiSpec


encodeApiSpecDict a =
    IntDict.Typed.encodeTyped encodeApiSpec a


type alias Model =
    ModelV2


decodeModel =
    Decode.oneOf
        [ decodeModelV2
        , decodeModelV1
            |> Decode.map (\{ selectedApiSpec, sources, menu } -> { selectedApiSpec = selectedApiSpec, sources = sources, menu = menu, helpOpen = False })
        ]


type alias HelpOpen =
    Bool


encodeHelpOpen _ =
    Encode.null


decodeHelpOpen =
    Decode.succeed False



-- [generator-start]


type Kind
    = Api
    | Mock
    | Curl


type alias ModelV2 =
    { sources : ApiSpecDict
    , selectedApiSpec : Maybe ApiCallKey
    , menu : ApiExplorer.Menus.Menu
    , helpOpen : HelpOpen
    }


type alias ModelV1 =
    { sources : ApiSpecDict
    , selectedApiSpec : Maybe ApiCallKey
    , menu : ApiExplorer.Menus.Menu
    }


type ApiCallKey
    = ApiCallKey Int



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeApiCallKey =
    Decode.map ApiCallKey Decode.int


decodeKind =
    let
        recover x =
            case x of
                "Api" ->
                    Decode.succeed Api

                "Mock" ->
                    Decode.succeed Mock

                "Curl" ->
                    Decode.succeed Curl

                other ->
                    Decode.fail <| "Unknown constructor for type Kind: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeModelV1 =
    Decode.map3
        ModelV1
        (Decode.field "sources" decodeApiSpecDict)
        (Decode.field "selectedApiSpec" (Decode.maybe decodeApiCallKey))
        (Decode.field "menu" ApiExplorer.Menus.decodeMenu)


decodeModelV2 =
    Decode.map4
        ModelV2
        (Decode.field "sources" decodeApiSpecDict)
        (Decode.field "selectedApiSpec" (Decode.maybe decodeApiCallKey))
        (Decode.field "menu" ApiExplorer.Menus.decodeMenu)
        (Decode.field "helpOpen" decodeHelpOpen)


encodeApiCallKey (ApiCallKey a1) =
    Encode.int a1


encodeKind a =
    case a of
        Api ->
            Encode.string "Api"

        Mock ->
            Encode.string "Mock"

        Curl ->
            Encode.string "Curl"


encodeMaybeApiCallKey a =
    case a of
        Just b ->
            encodeApiCallKey b

        Nothing ->
            Encode.null


encodeModelV1 a =
    Encode.object
        [ ( "sources", encodeApiSpecDict a.sources )
        , ( "selectedApiSpec", encodeMaybeApiCallKey a.selectedApiSpec )
        , ( "menu", ApiExplorer.Menus.encodeMenu a.menu )
        ]


encodeModelV2 a =
    Encode.object
        [ ( "sources", encodeApiSpecDict a.sources )
        , ( "selectedApiSpec", encodeMaybeApiCallKey a.selectedApiSpec )
        , ( "menu", ApiExplorer.Menus.encodeMenu a.menu )
        , ( "helpOpen", encodeHelpOpen a.helpOpen )
        ]



-- [generator-end]-- @@TODO we could have more refined types like links, ...


type alias InternalTreeViewState =
    Maybe Interface.JsonTree.Model.State


type alias Request =
    RemoteData.WebData Decode.Value


type alias JsonValue =
    Decode.Value


defaultApiSpec : ApiCallKey -> ApiSpec
defaultApiSpec key =
    { name = "Unnamed source " ++ String.fromInt (unwrapKey key + 1)
    , treeViewState = Nothing
    , url = randomuser
    , mockUrl = ""
    , curl = initCurl
    , request = RemoteData.NotAsked
    , method = Get
    , requestBody = ""
    , requestBodyParams = emptySelection
    , headers = []
    , responseDataSelection = emptySelection
    , kind = Api
    }


emptySelection : Interface.Selection.Selection
emptySelection =
    Dict.Any.empty Interface.JsonTree.Model.keyPathToString


products =
    "https://reqres.in/api/products/3"


randomuser =
    ApiExplorer.Api.UrlParser.Raw "https://randomuser.me/api/?page=3&results=10"


initCurl : CurlModel
initCurl =
    { input = "", parsed = Nothing }



-- [generator-start]


type alias ApiSpec =
    { name : String
    , kind : Kind
    , treeViewState : InternalTreeViewState
    , url : ApiExplorer.Api.UrlParser.Url
    , method : RequestMethod
    , headers : List ( String, String )
    , requestBody : String
    , mockUrl : String
    , curl : CurlModel
    , request : Request
    , requestBodyParams : Interface.Selection.Selection
    , responseDataSelection : Interface.Selection.Selection
    }


type alias CurlModel =
    { input : String
    , parsed : Maybe CurlParseResult
    }


type alias CurlParseResult =
    { url : ApiExplorer.Api.UrlParser.Url
    , method : RequestMethod
    , headers : List ( String, String )
    , requestBody : String
    }


type RequestMethod
    = Post
    | Get



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeApiSpec =
    Decode.succeed
        ApiSpec
        |> Extra.andMap (Decode.field "name" Decode.string)
        |> Extra.andMap (Decode.field "kind" decodeKind)
        |> Extra.andMap (Decode.field "treeViewState" decodeInternalTreeViewState)
        |> Extra.andMap (Decode.field "url" ApiExplorer.Api.UrlParser.decodeUrl)
        |> Extra.andMap (Decode.field "method" decodeRequestMethod)
        |> Extra.andMap (Decode.field "headers" (Decode.list decodeTuple_String_String_))
        |> Extra.andMap (Decode.field "requestBody" Decode.string)
        |> Extra.andMap (Decode.field "mockUrl" Decode.string)
        |> Extra.andMap (Decode.field "curl" decodeCurlModel)
        |> Extra.andMap (Decode.field "request" decodeRequest)
        |> Extra.andMap (Decode.field "requestBodyParams" Interface.Selection.decodeSelection)
        |> Extra.andMap (Decode.field "responseDataSelection" Interface.Selection.decodeSelection)


decodeCurlModel =
    Decode.map2
        CurlModel
        (Decode.field "input" Decode.string)
        (Decode.field "parsed" (Decode.maybe decodeCurlParseResult))


decodeCurlParseResult =
    Decode.map4
        CurlParseResult
        (Decode.field "url" ApiExplorer.Api.UrlParser.decodeUrl)
        (Decode.field "method" decodeRequestMethod)
        (Decode.field "headers" (Decode.list decodeTuple_String_String_))
        (Decode.field "requestBody" Decode.string)


decodeRequestMethod =
    let
        recover x =
            case x of
                "Post" ->
                    Decode.succeed Post

                "Get" ->
                    Decode.succeed Get

                other ->
                    Decode.fail <| "Unknown constructor for type RequestMethod: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeTuple_String_String_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Decode.string)
        (Decode.field "A2" Decode.string)


encodeApiSpec a =
    Encode.object
        [ ( "name", Encode.string a.name )
        , ( "kind", encodeKind a.kind )
        , ( "treeViewState", encodeInternalTreeViewState a.treeViewState )
        , ( "url", ApiExplorer.Api.UrlParser.encodeUrl a.url )
        , ( "method", encodeRequestMethod a.method )
        , ( "headers", Encode.list encodeTuple_String_String_ a.headers )
        , ( "requestBody", Encode.string a.requestBody )
        , ( "mockUrl", Encode.string a.mockUrl )
        , ( "curl", encodeCurlModel a.curl )
        , ( "request", encodeRequest a.request )
        , ( "requestBodyParams", Interface.Selection.encodeSelection a.requestBodyParams )
        , ( "responseDataSelection", Interface.Selection.encodeSelection a.responseDataSelection )
        ]


encodeCurlModel a =
    Encode.object
        [ ( "input", Encode.string a.input )
        , ( "parsed", encodeMaybeCurlParseResult a.parsed )
        ]


encodeCurlParseResult a =
    Encode.object
        [ ( "url", ApiExplorer.Api.UrlParser.encodeUrl a.url )
        , ( "method", encodeRequestMethod a.method )
        , ( "headers", Encode.list encodeTuple_String_String_ a.headers )
        , ( "requestBody", Encode.string a.requestBody )
        ]


encodeMaybeCurlParseResult a =
    case a of
        Just b ->
            encodeCurlParseResult b

        Nothing ->
            Encode.null


encodeRequestMethod a =
    case a of
        Post ->
            Encode.string "Post"

        Get ->
            Encode.string "Get"


encodeTuple_String_String_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", Encode.string a1 )
        , ( "A2", Encode.string a2 )
        ]



-- [generator-end]


encodeInternalTreeViewState _ =
    Encode.null


decodeInternalTreeViewState : Decode.Decoder InternalTreeViewState
decodeInternalTreeViewState =
    Decode.succeed Nothing


encodeRequest : Request -> Encode.Value
encodeRequest =
    RemoteData.withDefault Encode.null


decodeRequest : Decode.Decoder Request
decodeRequest =
    Decode.oneOf
        [ Decode.value |> Decode.map RemoteData.Success, Decode.succeed RemoteData.NotAsked ]


encodeJsonValue =
    identity


decodeJsonValue =
    Decode.value


{-| The source of an api in the api explorer
-}
applyCurlResult : CurlParseResult -> ApiSpec -> ApiSpec
applyCurlResult { url, method, headers, requestBody } source =
    { source
        | url = url
        , method = method
        , headers = headers
        , requestBody = requestBody
        , request = RemoteData.NotAsked
        , kind = Api
    }
