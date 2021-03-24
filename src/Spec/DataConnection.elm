module Spec.DataConnection exposing (..)

import DesignSystem.IconBrowser.Model
import Interface.Data
import Interface.Model
import Json.Decode as Decode
import Json.Encode as Encode



-- [generator-start]


{-| An element can be connected to some data. This type defines the different possible connections.
-}
type DataConnection
    = FromInterface Interface.Model.InterfacePointer
    | FromModel ModelTarget
    | FromValidation Int
    | Static Interface.Data.RefinedValue
    | Media Interface.Data.MediaDetails
      -- | StaticList ApiExplorer.Site.ContentList
    | Icon DesignSystem.IconBrowser.Model.ReferenceToIconInUserPickedIconSet
    | Embed YoutubeEmbed


type ModelTarget
    = Association AssociationTarget
    | PlainField Int


type alias AssociationTarget =
    { fieldKey : Int, associationKey : Int }


type alias YoutubeEmbed =
    { videoId : String }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeAssociationTarget =
    Decode.map2
        AssociationTarget
        (Decode.field "fieldKey" Decode.int)
        (Decode.field "associationKey" Decode.int)


decodeDataConnection =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeDataConnectionHelp


decodeDataConnectionHelp constructor =
    case constructor of
        "FromInterface" ->
            Decode.map
                FromInterface
                (Decode.field "A1" Interface.Model.decodeInterfacePointer)

        "FromModel" ->
            Decode.map
                FromModel
                (Decode.field "A1" decodeModelTarget)

        "FromValidation" ->
            Decode.map
                FromValidation
                (Decode.field "A1" Decode.int)

        "Static" ->
            Decode.map
                Static
                (Decode.field "A1" Interface.Data.decodeRefinedValue)

        "Media" ->
            Decode.map
                Media
                (Decode.field "A1" Interface.Data.decodeMediaDetails)

        "Icon" ->
            Decode.map
                Icon
                (Decode.field "A1" DesignSystem.IconBrowser.Model.decodeReferenceToIconInUserPickedIconSet)

        "Embed" ->
            Decode.map
                Embed
                (Decode.field "A1" decodeYoutubeEmbed)

        other ->
            Decode.fail <| "Unknown constructor for type DataConnection: " ++ other


decodeModelTarget =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeModelTargetHelp


decodeModelTargetHelp constructor =
    case constructor of
        "Association" ->
            Decode.map
                Association
                (Decode.field "A1" decodeAssociationTarget)

        "PlainField" ->
            Decode.map
                PlainField
                (Decode.field "A1" Decode.int)

        other ->
            Decode.fail <| "Unknown constructor for type ModelTarget: " ++ other


decodeYoutubeEmbed =
    Decode.map
        YoutubeEmbed
        (Decode.field "videoId" Decode.string)


encodeAssociationTarget a =
    Encode.object
        [ ( "fieldKey", Encode.int a.fieldKey )
        , ( "associationKey", Encode.int a.associationKey )
        ]


encodeDataConnection a =
    case a of
        FromInterface a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "FromInterface" )
                , ( "A1", Interface.Model.encodeInterfacePointer a1 )
                ]

        FromModel a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "FromModel" )
                , ( "A1", encodeModelTarget a1 )
                ]

        FromValidation a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "FromValidation" )
                , ( "A1", Encode.int a1 )
                ]

        Static a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Static" )
                , ( "A1", Interface.Data.encodeRefinedValue a1 )
                ]

        Media a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Media" )
                , ( "A1", Interface.Data.encodeMediaDetails a1 )
                ]

        Icon a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Icon" )
                , ( "A1", DesignSystem.IconBrowser.Model.encodeReferenceToIconInUserPickedIconSet a1 )
                ]

        Embed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Embed" )
                , ( "A1", encodeYoutubeEmbed a1 )
                ]


encodeModelTarget a =
    case a of
        Association a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Association" )
                , ( "A1", encodeAssociationTarget a1 )
                ]

        PlainField a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "PlainField" )
                , ( "A1", Encode.int a1 )
                ]


encodeYoutubeEmbed a =
    Encode.object
        [ ( "videoId", Encode.string a.videoId )
        ]



-- [generator-end]
