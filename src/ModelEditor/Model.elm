module ModelEditor.Model exposing (..)

import Canvas.AttributesPanel.Content.Tabs
import Dict
import Dynamic.Data
import IntDict
import Json.Decode as Decode
import Json.Encode as Encode
import Spec.DataConnection


emptyMatchers =
    Match Dict.empty


init : Model
init =
    { helpOpen = False
    , selectedField = Nothing
    , fields = IntDict.empty
    }


type alias Model =
    ModelV2


decodeModel =
    Decode.oneOf
        [ decodeModelV2
        , decodeModelV1
            |> Decode.map (\{ selectedField, fields } -> { selectedField = selectedField, fields = fields, helpOpen = False })
        ]


encodeModel =
    encodeModelV2


type alias HelpOpen =
    Bool


encodeHelpOpen _ =
    Encode.null


decodeHelpOpen =
    Decode.succeed False



-- [generator-start]


type alias ConnectTarget =
    { associationKey : Int, matcherKey : Int }


type alias ModelV1 =
    { selectedField : Maybe Int
    , fields : Fields
    }


type alias ModelV2 =
    { selectedField : Maybe Int
    , fields : Fields
    , helpOpen : HelpOpen
    }


type alias Fields =
    IntDict.Dict Field


type alias Associations =
    IntDict.Dict Association


type alias Association =
    { name : String
    , projection : Projection
    }


type Projection
    = FormatNumber
    | Match Matchers
    | Verbatim
    | RefinedInstance


type alias Matchers =
    IntDict.Dict Spec.DataConnection.DataConnection


type alias Field =
    { kind : FieldKind
    , name : String
    , associations : Associations
    , connectionWorkflowTarget : Maybe ConnectTarget
    , activeContentTab : Canvas.AttributesPanel.Content.Tabs.ContentTab
    , comment : String
    }


type FieldKind
    = Unspecified
    | Typed Dynamic.Data.Kind
    | WithDefault Dynamic.Data.Instance



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeAssociation =
    Decode.map2
        Association
        (Decode.field "name" Decode.string)
        (Decode.field "projection" decodeProjection)


decodeAssociations =
    IntDict.decodeDict decodeAssociation


decodeConnectTarget =
    Decode.map2
        ConnectTarget
        (Decode.field "associationKey" Decode.int)
        (Decode.field "matcherKey" Decode.int)


decodeField =
    Decode.map6
        Field
        (Decode.field "kind" decodeFieldKind)
        (Decode.field "name" Decode.string)
        (Decode.field "associations" decodeAssociations)
        (Decode.field "connectionWorkflowTarget" (Decode.maybe decodeConnectTarget))
        (Decode.field "activeContentTab" Canvas.AttributesPanel.Content.Tabs.decodeContentTab)
        (Decode.field "comment" Decode.string)


decodeFieldKind =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeFieldKindHelp


decodeFieldKindHelp constructor =
    case constructor of
        "Unspecified" ->
            Decode.succeed Unspecified

        "Typed" ->
            Decode.map
                Typed
                (Decode.field "A1" Dynamic.Data.decodeKind)

        "WithDefault" ->
            Decode.map
                WithDefault
                (Decode.field "A1" Dynamic.Data.decodeInstance)

        other ->
            Decode.fail <| "Unknown constructor for type FieldKind: " ++ other


decodeFields =
    IntDict.decodeDict decodeField


decodeMatchers =
    IntDict.decodeDict Spec.DataConnection.decodeDataConnection


decodeModelV1 =
    Decode.map2
        ModelV1
        (Decode.field "selectedField" (Decode.maybe Decode.int))
        (Decode.field "fields" decodeFields)


decodeModelV2 =
    Decode.map3
        ModelV2
        (Decode.field "selectedField" (Decode.maybe Decode.int))
        (Decode.field "fields" decodeFields)
        (Decode.field "helpOpen" decodeHelpOpen)


decodeProjection =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeProjectionHelp


decodeProjectionHelp constructor =
    case constructor of
        "FormatNumber" ->
            Decode.succeed FormatNumber

        "Match" ->
            Decode.map
                Match
                (Decode.field "A1" decodeMatchers)

        "Verbatim" ->
            Decode.succeed Verbatim

        "RefinedInstance" ->
            Decode.succeed RefinedInstance

        other ->
            Decode.fail <| "Unknown constructor for type Projection: " ++ other


encodeAssociation a =
    Encode.object
        [ ( "name", Encode.string a.name )
        , ( "projection", encodeProjection a.projection )
        ]


encodeAssociations a =
    IntDict.encodeDict encodeAssociation a


encodeConnectTarget a =
    Encode.object
        [ ( "associationKey", Encode.int a.associationKey )
        , ( "matcherKey", Encode.int a.matcherKey )
        ]


encodeField a =
    Encode.object
        [ ( "kind", encodeFieldKind a.kind )
        , ( "name", Encode.string a.name )
        , ( "associations", encodeAssociations a.associations )
        , ( "connectionWorkflowTarget", encodeMaybeConnectTarget a.connectionWorkflowTarget )
        , ( "activeContentTab", Canvas.AttributesPanel.Content.Tabs.encodeContentTab a.activeContentTab )
        , ( "comment", Encode.string a.comment )
        ]


encodeFieldKind a =
    case a of
        Unspecified ->
            Encode.object
                [ ( "Constructor", Encode.string "Unspecified" )
                ]

        Typed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Typed" )
                , ( "A1", Dynamic.Data.encodeKind a1 )
                ]

        WithDefault a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "WithDefault" )
                , ( "A1", Dynamic.Data.encodeInstance a1 )
                ]


encodeFields a =
    IntDict.encodeDict encodeField a


encodeMatchers a =
    IntDict.encodeDict Spec.DataConnection.encodeDataConnection a


encodeMaybeConnectTarget a =
    case a of
        Just b ->
            encodeConnectTarget b

        Nothing ->
            Encode.null


encodeMaybeInt a =
    case a of
        Just b ->
            Encode.int b

        Nothing ->
            Encode.null


encodeModelV1 a =
    Encode.object
        [ ( "selectedField", encodeMaybeInt a.selectedField )
        , ( "fields", encodeFields a.fields )
        ]


encodeModelV2 a =
    Encode.object
        [ ( "selectedField", encodeMaybeInt a.selectedField )
        , ( "fields", encodeFields a.fields )
        , ( "helpOpen", encodeHelpOpen a.helpOpen )
        ]


encodeProjection a =
    case a of
        FormatNumber ->
            Encode.object
                [ ( "Constructor", Encode.string "FormatNumber" )
                ]

        Match a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Match" )
                , ( "A1", encodeMatchers a1 )
                ]

        Verbatim ->
            Encode.object
                [ ( "Constructor", Encode.string "Verbatim" )
                ]

        RefinedInstance ->
            Encode.object
                [ ( "Constructor", Encode.string "RefinedInstance" )
                ]



-- [generator-end]
