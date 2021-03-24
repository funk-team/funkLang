module CodeEditor.Model exposing (..)

import ApiExplorer.Model
import IntDict.Typed
import Interface.Selection
import Json.Decode as Decode
import Json.Decode.Extra as Extra
import Json.Encode as Encode
import Time


getCurrentCode : Model -> Maybe Transformation
getCurrentCode model =
    case model.selected of
        Nothing ->
            Nothing

        Just key ->
            getItem key model


getItem : TransformationKey -> Model -> Maybe Transformation
getItem itemId { items } =
    IntDict.Typed.get unwrapKey itemId items


wrapKey =
    TransformationKey


unwrapKey (TransformationKey k) =
    k


listCodes : Model -> List ( TransformationKey, Transformation )
listCodes { items } =
    items |> IntDict.Typed.toList wrapKey


init : Model
init =
    { helpOpen = False
    , selected = Nothing
    , items = IntDict.Typed.empty
    }


type alias Value =
    Decode.Value


encodeValue =
    identity


decodeValue =
    Decode.value


decodeSourceDict =
    Decode.oneOf
        [ IntDict.Typed.decodeTyped decodeTransformation
        , Decode.succeed IntDict.Typed.empty
        ]


encodeSourceDict a =
    IntDict.Typed.encodeTyped encodeTransformation a


type alias SourceDict =
    IntDict.Typed.Typed TransformationKey Transformation


type alias Model =
    ModelV3


decodeModel =
    Decode.oneOf
        [ decodeModelV3
        , decodeModelV2
        , decodeModelV1
            |> Decode.map (\{ selected, items } -> { selected = selected, items = items, helpOpen = False })
            |> Decode.map (\{ selected, items, helpOpen } -> { selected = selected, items = items, helpOpen = helpOpen })
        ]


encodeModel =
    encodeModelV3


type alias HelpOpen =
    Bool


encodeHelpOpen _ =
    Encode.null


decodeHelpOpen =
    Decode.succeed False


type alias DataResult =
    Result String Value


type alias Timestamp =
    Time.Posix


decodeTimestamp =
    Decode.int |> Decode.map Time.millisToPosix


encodeTimestamp =
    Time.posixToMillis >> Encode.int


encodeDataResult d =
    case d of
        Err err ->
            Encode.list identity [ Encode.string "err", Encode.string err ]

        Ok d_ ->
            Encode.list identity [ Encode.string "ok", d_ ]


decodeDataResult =
    Decode.index 0 Decode.string
        |> Decode.andThen
            (\constructor ->
                case constructor of
                    "err" ->
                        Decode.index 1 Decode.string |> Decode.map Err

                    "ok" ->
                        Decode.index 1 Decode.value |> Decode.map Ok

                    _ ->
                        Decode.fail "constructor not found"
            )


type alias RawExecutionResult =
    { timestamp : Int -- UNIX timestamp for fetching the latest version
    , sourceId : Int -- id of the original transformation
    , return_ : Value
    }



-- [generator-start]


type TransformationKey
    = TransformationKey Int


type alias ExecutionReturn =
    { timestamp : Timestamp -- UNIX timestamp for fetching the latest version
    , sourceId : TransformationKey -- id of the original transformation
    , return : DataResult
    }


type alias ExecutionState =
    Maybe ExecutionReturn


type alias ModelV1 =
    { selected : Maybe TransformationKey
    , items : SourceDict
    }


type alias ModelV2 =
    { selected : Maybe TransformationKey
    , items : SourceDict
    , helpOpen : HelpOpen
    }


type alias ModelV3 =
    { selected : Maybe TransformationKey
    , items : SourceDict
    , helpOpen : HelpOpen
    }


type alias Transformation =
    { arguments : List Argument
    , code : String
    , name : String
    , outputSelection : Interface.Selection.Selection
    , executionState : ExecutionState

    -- view only
    , expandedArguments : List ApiExplorer.Model.ApiCallKey
    , argumentsCollapsed : Bool
    , codeSectionCollapsed : Bool
    , debugOpen : Bool
    }



-- the variable name is defined here so that the user can rename
-- api calls without breaking the code in the code editor


type alias Argument =
    ApiExplorer.Model.ApiCallKey



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeArgument =
    ApiExplorer.Model.decodeApiCallKey


decodeExecutionReturn =
    Decode.map3
        ExecutionReturn
        (Decode.field "timestamp" decodeTimestamp)
        (Decode.field "sourceId" decodeTransformationKey)
        (Decode.field "return" decodeDataResult)


decodeExecutionState =
    Decode.maybe decodeExecutionReturn


decodeModelV1 =
    Decode.map2
        ModelV1
        (Decode.field "selected" (Decode.maybe decodeTransformationKey))
        (Decode.field "items" decodeSourceDict)


decodeModelV2 =
    Decode.map3
        ModelV2
        (Decode.field "selected" (Decode.maybe decodeTransformationKey))
        (Decode.field "items" decodeSourceDict)
        (Decode.field "helpOpen" decodeHelpOpen)


decodeModelV3 =
    Decode.map3
        ModelV3
        (Decode.field "selected" (Decode.maybe decodeTransformationKey))
        (Decode.field "items" decodeSourceDict)
        (Decode.field "helpOpen" decodeHelpOpen)


decodeTransformation =
    Decode.succeed
        Transformation
        |> Extra.andMap (Decode.field "arguments" (Decode.list decodeArgument))
        |> Extra.andMap (Decode.field "code" Decode.string)
        |> Extra.andMap (Decode.field "name" Decode.string)
        |> Extra.andMap (Decode.field "outputSelection" Interface.Selection.decodeSelection)
        |> Extra.andMap (Decode.field "executionState" decodeExecutionState)
        |> Extra.andMap (Decode.field "expandedArguments" (Decode.list ApiExplorer.Model.decodeApiCallKey))
        |> Extra.andMap (Decode.field "argumentsCollapsed" Decode.bool)
        |> Extra.andMap (Decode.field "codeSectionCollapsed" Decode.bool)
        |> Extra.andMap (Decode.field "debugOpen" Decode.bool)


decodeTransformationKey =
    Decode.map TransformationKey Decode.int


encodeArgument a =
    ApiExplorer.Model.encodeApiCallKey a


encodeExecutionReturn a =
    Encode.object
        [ ( "timestamp", encodeTimestamp a.timestamp )
        , ( "sourceId", encodeTransformationKey a.sourceId )
        , ( "return", encodeDataResult a.return )
        ]


encodeExecutionState a =
    case a of
        Just b ->
            encodeExecutionReturn b

        Nothing ->
            Encode.null


encodeMaybeTransformationKey a =
    case a of
        Just b ->
            encodeTransformationKey b

        Nothing ->
            Encode.null


encodeModelV1 a =
    Encode.object
        [ ( "selected", encodeMaybeTransformationKey a.selected )
        , ( "items", encodeSourceDict a.items )
        ]


encodeModelV2 a =
    Encode.object
        [ ( "selected", encodeMaybeTransformationKey a.selected )
        , ( "items", encodeSourceDict a.items )
        , ( "helpOpen", encodeHelpOpen a.helpOpen )
        ]


encodeModelV3 a =
    Encode.object
        [ ( "selected", encodeMaybeTransformationKey a.selected )
        , ( "items", encodeSourceDict a.items )
        , ( "helpOpen", encodeHelpOpen a.helpOpen )
        ]


encodeTransformation a =
    Encode.object
        [ ( "arguments", Encode.list encodeArgument a.arguments )
        , ( "code", Encode.string a.code )
        , ( "name", Encode.string a.name )
        , ( "outputSelection", Interface.Selection.encodeSelection a.outputSelection )
        , ( "executionState", encodeExecutionState a.executionState )
        , ( "expandedArguments", Encode.list ApiExplorer.Model.encodeApiCallKey a.expandedArguments )
        , ( "argumentsCollapsed", Encode.bool a.argumentsCollapsed )
        , ( "codeSectionCollapsed", Encode.bool a.codeSectionCollapsed )
        , ( "debugOpen", Encode.bool a.debugOpen )
        ]


encodeTransformationKey (TransformationKey a1) =
    Encode.int a1



-- [generator-end]
