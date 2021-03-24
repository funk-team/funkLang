module Interface.Data exposing (..)

{-| This module provides tools to work with actual data that comes from an interface.

It also provides utilities for refining raw JSON data to user-level types

#Important Functions

@docs getFromValue, makeDecoder

-}

import DesignSystem.IconBrowser.Model
import Dict.Any
import Element
import Interface.JsonTree.Model
import Interface.Model
import Interface.Selection
import Json.Decode as Decode
import Json.Encode as Encode
import Ui.Boxicons
import Ui.Component


type alias JsonValue =
    Decode.Value


encodeJsonValue =
    identity


decodeJsonValue =
    Decode.value


{-| The content for the canvas plus some metadata to enhance the editor experience
-}
type alias CanvasRenderable =
    { value : RefinedValue, inlineEditable : Bool }


supportsObjectFit refinedValue =
    case refinedValue of
        Nothing ->
            False

        Just (Media { kind }) ->
            case kind of
                Image ->
                    True

                Video ->
                    True

                _ ->
                    False

        _ ->
            False


{-| Given the data scope visible to an element, retrieve a value
-}
retrieveWithRefinedTypeFromContext :
    Interface.Model.ScopeId
    -> Interface.JsonTree.Model.KeyPath
    -> Interface.Model.LocalScopeData
    -> Maybe RefinedValue
retrieveWithRefinedTypeFromContext scopeId jsonPath apiContext =
    case ( getFromValue jsonPath apiContext.data, Dict.Any.get jsonPath apiContext.selectionForEachItemInListScope ) of
        ( Just value, Just settings ) ->
            refineValue
                (Tuple.first scopeId)
                jsonPath
                settings
                value

        _ ->
            Nothing


{-| Given a string and metadata settings by the user, return a refined value
-}
refineString refinedType str =
    case refinedType of
        Just Interface.Selection.Image ->
            toImageSource str

        Just (Interface.Selection.Text _) ->
            ParagraphText str

        Just Interface.Selection.Int ->
            ParagraphText str

        Just Interface.Selection.Float ->
            ParagraphText str

        Just Interface.Selection.Bool ->
            ParagraphText str

        _ ->
            ParagraphText str


{-| Given a path, retrieve the value at the path
-}
getFromValue :
    Interface.JsonTree.Model.KeyPath
    -> Decode.Value
    -> Maybe Decode.Value
getFromValue keyPath value =
    case Decode.decodeValue (makeDecoder keyPath) value of
        Ok tValue ->
            Just tValue

        Err err ->
            Nothing


{-| Make a decoder based on a Interface.JsonTree keypath
-}
makeDecoder :
    Interface.JsonTree.Model.KeyPath
    -> Decode.Decoder Decode.Value
makeDecoder keyPath =
    case keyPath of
        [] ->
            Decode.value

        firstAccessor :: otherAccessors ->
            case firstAccessor of
                Interface.JsonTree.Model.IndexAccessor i ->
                    Decode.index i (decodeChild otherAccessors)

                Interface.JsonTree.Model.ObjectAccessor fieldName ->
                    Decode.field fieldName (decodeChild otherAccessors)


refinedValueToLabel : RefinedValue -> String
refinedValueToLabel refinedValue =
    case refinedValue of
        ParagraphText str ->
            str

        Media { meta } ->
            meta.title

        _ ->
            ""


refinedValueToIcon : RefinedValue -> Element.Element msg
refinedValueToIcon refinedValue =
    case refinedValue of
        ParagraphText _ ->
            Ui.Component.icon <| Ui.Boxicons.bxText

        Media { kind } ->
            mediaKindToIcon kind

        _ ->
            Element.none


mediaKindToIcon mediaKind =
    case mediaKind of
        Image ->
            Ui.Boxicons.bxsFileImage |> Ui.Component.icon

        Audio ->
            Ui.Boxicons.bxsMusic |> Ui.Component.icon

        Video ->
            Ui.Boxicons.bxsVideo |> Ui.Component.icon

        Pdf ->
            Ui.Boxicons.bxsFilePdf |> Ui.Component.icon


toImageSource :
    String
    -> RefinedValue
toImageSource rawSrc =
    if String.startsWith "/" rawSrc then
        case addImdbMockBaseUrl rawSrc of
            completeUrl ->
                Media (MediaDetails Image { src = completeUrl, title = "title" })

    else
        Media (MediaDetails Image { src = rawSrc, title = "title" })



-- @@TODO: implement ability to specify the base url of things


addImdbMockBaseUrl : String -> String
addImdbMockBaseUrl rawSrc =
    let
        fakeBaseUrl =
            "https://image.tmdb.org/t/p/w500"
    in
    fakeBaseUrl ++ rawSrc


refineValue :
    Interface.Model.InterfaceKey
    -> Interface.JsonTree.Model.KeyPath
    -> Interface.Selection.SelectedEntrySettings
    -> Decode.Value
    -> Maybe RefinedValue
refineValue interfaceKey pathToValue { kind } value =
    let
        refineList selectionForEachItemInListScope listOfValues =
            InterfaceDataList
                { values = listOfValues
                , selections = selectionForEachItemInListScope
                , scopeId = ( interfaceKey, pathToValue )
                }

        decoder =
            case kind of
                Interface.Selection.Single refinedType ->
                    Decode.oneOf [ Decode.string, Decode.float |> Decode.map String.fromFloat ]
                        |> Decode.map (refineString refinedType)

                Interface.Selection.List selectionForEachItemInListScope ->
                    Decode.list Decode.value
                        |> Decode.map (refineList selectionForEachItemInListScope)
    in
    Decode.decodeValue decoder value
        |> Result.toMaybe


getFromValueAndTag value =
    getFromValue value >> andTag


{-| Given a possibly decoded value, tag it with a JsonTree type
-}
andTag : Maybe Decode.Value -> Maybe Interface.JsonTree.Model.TaggedValue
andTag =
    Maybe.andThen (Decode.decodeValue decodeTagged >> Result.toMaybe)


decodeChild :
    Interface.JsonTree.Model.KeyPath
    -> Decode.Decoder Decode.Value
decodeChild keyPath =
    case keyPath of
        [] ->
            Decode.value

        firstAccessor :: otherAccessors ->
            case firstAccessor of
                Interface.JsonTree.Model.IndexAccessor i ->
                    Decode.index i (decodeChild otherAccessors)

                Interface.JsonTree.Model.ObjectAccessor fieldName ->
                    Decode.field fieldName (decodeChild otherAccessors)


decodeTagged : Decode.Decoder Interface.JsonTree.Model.TaggedValue
decodeTagged =
    Decode.oneOf
        [ Decode.map Interface.JsonTree.Model.TString Decode.string
        , Decode.map Interface.JsonTree.Model.TFloat Decode.float
        , Decode.map Interface.JsonTree.Model.TBool Decode.bool
        ]



-- [generator-start]


type alias MediaDetails =
    { kind : MediaKind
    , meta : MediaMeta
    }


type alias MediaMeta =
    { src : String, title : String }


type MediaKind
    = Image
    | Audio
    | Pdf
    | Video


type RefinedValue
    = Media MediaDetails
    | ParagraphText String
    | InterfaceDataList ListData
    | Icon DesignSystem.IconBrowser.Model.LocallyCachedIcon
    | YoutubeEmbed { videoId : String }



-- | RichText ApiExplorer.Request.HtmlResponseData
-- | StaticList ApiExplorer.Site.ContentList


{-| The whole list bound to an element
-}
type alias ListData =
    { values : List JsonValue
    , selections : Interface.Selection.Selection
    , scopeId : Interface.Model.ScopeId
    }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


type alias Record_videoId_String_ =
    { videoId : String }


decodeListData =
    Decode.map3
        ListData
        (Decode.field "values" (Decode.list decodeJsonValue))
        (Decode.field "selections" Interface.Selection.decodeSelection)
        (Decode.field "scopeId" Interface.Model.decodeScopeId)


decodeMediaDetails =
    Decode.map2
        MediaDetails
        (Decode.field "kind" decodeMediaKind)
        (Decode.field "meta" decodeMediaMeta)


decodeMediaKind =
    let
        recover x =
            case x of
                "Image" ->
                    Decode.succeed Image

                "Audio" ->
                    Decode.succeed Audio

                "Pdf" ->
                    Decode.succeed Pdf

                "Video" ->
                    Decode.succeed Video

                other ->
                    Decode.fail <| "Unknown constructor for type MediaKind: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeMediaMeta =
    Decode.map2
        MediaMeta
        (Decode.field "src" Decode.string)
        (Decode.field "title" Decode.string)


decodeRecord_videoId_String_ =
    Decode.map
        Record_videoId_String_
        (Decode.field "videoId" Decode.string)


decodeRefinedValue =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeRefinedValueHelp


decodeRefinedValueHelp constructor =
    case constructor of
        "Media" ->
            Decode.map
                Media
                (Decode.field "A1" decodeMediaDetails)

        "ParagraphText" ->
            Decode.map
                ParagraphText
                (Decode.field "A1" Decode.string)

        "InterfaceDataList" ->
            Decode.map
                InterfaceDataList
                (Decode.field "A1" decodeListData)

        "Icon" ->
            Decode.map
                Icon
                (Decode.field "A1" DesignSystem.IconBrowser.Model.decodeLocallyCachedIcon)

        "YoutubeEmbed" ->
            Decode.map
                YoutubeEmbed
                (Decode.field "A1" decodeRecord_videoId_String_)

        other ->
            Decode.fail <| "Unknown constructor for type RefinedValue: " ++ other


encodeListData a =
    Encode.object
        [ ( "values", Encode.list encodeJsonValue a.values )
        , ( "selections", Interface.Selection.encodeSelection a.selections )
        , ( "scopeId", Interface.Model.encodeScopeId a.scopeId )
        ]


encodeMediaDetails a =
    Encode.object
        [ ( "kind", encodeMediaKind a.kind )
        , ( "meta", encodeMediaMeta a.meta )
        ]


encodeMediaKind a =
    case a of
        Image ->
            Encode.string "Image"

        Audio ->
            Encode.string "Audio"

        Pdf ->
            Encode.string "Pdf"

        Video ->
            Encode.string "Video"


encodeMediaMeta a =
    Encode.object
        [ ( "src", Encode.string a.src )
        , ( "title", Encode.string a.title )
        ]


encodeRecord_videoId_String_ a =
    Encode.object
        [ ( "videoId", Encode.string a.videoId )
        ]


encodeRefinedValue a =
    case a of
        Media a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Media" )
                , ( "A1", encodeMediaDetails a1 )
                ]

        ParagraphText a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "ParagraphText" )
                , ( "A1", Encode.string a1 )
                ]

        InterfaceDataList a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "InterfaceDataList" )
                , ( "A1", encodeListData a1 )
                ]

        Icon a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Icon" )
                , ( "A1", DesignSystem.IconBrowser.Model.encodeLocallyCachedIcon a1 )
                ]

        YoutubeEmbed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "YoutubeEmbed" )
                , ( "A1", encodeRecord_videoId_String_ a1 )
                ]



-- [generator-end]
