module ApiExplorer.Api.UrlParser exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Url


type alias Protocol =
    Url.Protocol


urlToString : Url -> String
urlToString u =
    case u of
        Raw str ->
            str

        Parsed p ->
            toString p



-- [generator-start]


type Url
    = Raw String
    | Parsed ParsedUrl


type alias ParsedUrl =
    { protocol : Protocol
    , host : String
    , port_ : Maybe Int
    , path : List Entry
    , query : List ( String, Entry )
    , fragment : Maybe Entry
    }


type Entry
    = AsInInput String
    | Parameterized String String


parseUrl : String -> Maybe ParsedUrl
parseUrl str =
    let
        parsePath str_ =
            String.split "/" str_
                |> List.map AsInInput

        parseQuery maybeStr =
            let
                parseEqual str_ =
                    case String.split "=" str_ of
                        [ key, val ] ->
                            Just ( key, AsInInput val )

                        _ ->
                            Nothing
            in
            case maybeStr of
                Nothing ->
                    []

                Just str_ ->
                    String.split "&" str_
                        |> List.filterMap parseEqual
    in
    case Url.fromString str of
        Nothing ->
            Nothing

        Just { protocol, host, port_, path, query, fragment } ->
            { protocol = protocol
            , host = host
            , port_ = port_
            , path = parsePath path
            , query = parseQuery query
            , fragment = fragment |> Maybe.map AsInInput
            }
                |> Just


toString : ParsedUrl -> String
toString url =
    let
        { protocol, host, port_, path, query, fragment } =
            url

        urlUrl =
            { protocol = protocol
            , host = host
            , port_ = port_
            , path = pathToString path
            , query = queryToMaybeString query
            , fragment = Maybe.map entryToString fragment
            }

        pathToString path_ =
            List.map entryToString path_
                |> List.intersperse "/"
                |> List.foldr (++) ""

        queryToMaybeString query_ =
            List.map (\( key, val ) -> key ++ "=" ++ entryToString val) query_
                |> List.intersperse "&"
                |> List.foldr (++) ""
                |> nothingIfEmptyString

        nothingIfEmptyString str =
            if String.isEmpty str then
                Nothing

            else
                Just str
    in
    Url.toString urlUrl


entryToString entry =
    case entry of
        AsInInput str ->
            str

        Parameterized placeholder str ->
            str


protocolToString protocol =
    case protocol of
        Url.Http ->
            "http://"

        Url.Https ->
            "https://"


editPathEntry parsedUrl index newEntry =
    let
        updateEntryAtIndex indexToUpdate currentIndex currentEntry =
            if indexToUpdate == currentIndex then
                newEntry

            else
                currentEntry
    in
    { parsedUrl
        | path =
            List.indexedMap (updateEntryAtIndex index) parsedUrl.path
    }


editQueryEntry parsedUrl index newEntry =
    let
        updateEntryAtIndex indexToUpdate currentIndex ( key, currentEntry ) =
            if indexToUpdate == currentIndex then
                ( key, newEntry )

            else
                ( key, currentEntry )
    in
    { parsedUrl
        | query =
            List.indexedMap (updateEntryAtIndex index) parsedUrl.query
    }


editFragmentEntry parsedUrl newEntry =
    { parsedUrl | fragment = Just newEntry }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeEntry =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeEntryHelp


decodeEntryHelp constructor =
    case constructor of
        "AsInInput" ->
            Decode.map
                AsInInput
                (Decode.field "A1" Decode.string)

        "Parameterized" ->
            Decode.map2
                Parameterized
                (Decode.field "A1" Decode.string)
                (Decode.field "A2" Decode.string)

        other ->
            Decode.fail <| "Unknown constructor for type Entry: " ++ other


decodeParsedUrl =
    Decode.map6
        ParsedUrl
        (Decode.field "protocol" decodeProtocol)
        (Decode.field "host" Decode.string)
        (Decode.field "port_" (Decode.maybe Decode.int))
        (Decode.field "path" (Decode.list decodeEntry))
        (Decode.field "query" (Decode.list decodeTuple_String_Entry_))
        (Decode.field "fragment" (Decode.maybe decodeEntry))


decodeTuple_String_Entry_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Decode.string)
        (Decode.field "A2" decodeEntry)


decodeUrl =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeUrlHelp


decodeUrlHelp constructor =
    case constructor of
        "Raw" ->
            Decode.map
                Raw
                (Decode.field "A1" Decode.string)

        "Parsed" ->
            Decode.map
                Parsed
                (Decode.field "A1" decodeParsedUrl)

        other ->
            Decode.fail <| "Unknown constructor for type Url: " ++ other


encodeEntry a =
    case a of
        AsInInput a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "AsInInput" )
                , ( "A1", Encode.string a1 )
                ]

        Parameterized a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "Parameterized" )
                , ( "A1", Encode.string a1 )
                , ( "A2", Encode.string a2 )
                ]


encodeMaybeEntry a =
    case a of
        Just b ->
            encodeEntry b

        Nothing ->
            Encode.null


encodeMaybeInt a =
    case a of
        Just b ->
            Encode.int b

        Nothing ->
            Encode.null


encodeParsedUrl a =
    Encode.object
        [ ( "protocol", encodeProtocol a.protocol )
        , ( "host", Encode.string a.host )
        , ( "port_", encodeMaybeInt a.port_ )
        , ( "path", Encode.list encodeEntry a.path )
        , ( "query", Encode.list encodeTuple_String_Entry_ a.query )
        , ( "fragment", encodeMaybeEntry a.fragment )
        ]


encodeTuple_String_Entry_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", Encode.string a1 )
        , ( "A2", encodeEntry a2 )
        ]


encodeUrl a =
    case a of
        Raw a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Raw" )
                , ( "A1", Encode.string a1 )
                ]

        Parsed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Parsed" )
                , ( "A1", encodeParsedUrl a1 )
                ]



-- [generator-end]


decodeProtocol =
    let
        recover x =
            case x of
                "Http" ->
                    Decode.succeed Url.Http

                "Https" ->
                    Decode.succeed Url.Https

                other ->
                    Decode.fail <| "Unknown constructor for type Protocol: " ++ other
    in
    Decode.string |> Decode.andThen recover


encodeProtocol a =
    case a of
        Url.Http ->
            Encode.string "Http"

        Url.Https ->
            Encode.string "Https"
