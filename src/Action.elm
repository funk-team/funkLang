module Action exposing (..)

import ApiExplorer.Model
import Dict
import Dict.Any
import Dynamic.Data
import Interface.JsonTree.Model
import Interface.Selection
import Json.Decode as Decode
import Json.Encode as Encode
import Spec.Element.Id
import Url


type alias Url =
    Url.Url


encodeUrl =
    Url.toString >> Encode.string


decodeUrl : Decode.Decoder Url
decodeUrl =
    Decode.string
        |> Decode.andThen
            (\urlString ->
                case Url.fromString urlString of
                    Just url ->
                        Decode.succeed url

                    Nothing ->
                        Decode.fail "Can not parse URL"
            )


default : ActionsForElement
default =
    ActionsForElement Nothing


defaultNavigate : ClickAction
defaultNavigate =
    Navigate <|
        NavigateParams
            (Internal Nothing)
            SameTab


defaultMakeApiCall : ClickAction
defaultMakeApiCall =
    MakeApiCall <|
        ApiCallParams
            Nothing
            (Dict.Any.empty apiCallSlotIdentifierToString)


defaultUpdateModel : ClickAction
defaultUpdateModel =
    UpdateModel Dict.empty


linkLocationToString : TargetLocation -> String
linkLocationToString linkLocation =
    case linkLocation of
        External _ ->
            "External"

        Scroll _ _ ->
            "Scroll"

        Internal _ ->
            "Internal"


type alias ModelToCallMap =
    Dict.Any.AnyDict String ApiCallSlotIdentifier Int


encodeModelToCallMap : ModelToCallMap -> Encode.Value
encodeModelToCallMap =
    Dict.Any.toList
        >> encodeEncodedModelToCallMap


apiCallSlotIdentifierToString : ApiCallSlotIdentifier -> String
apiCallSlotIdentifierToString =
    encodeApiCallSlotIdentifier >> Encode.encode 0


decodeModelToCallMap : Decode.Decoder ModelToCallMap
decodeModelToCallMap =
    decodeEncodedModelToCallMap
        |> Decode.map (Dict.Any.fromList apiCallSlotIdentifierToString)



-- [generator-start]


type alias EncodedModelToCallMap =
    List ( ApiCallSlotIdentifier, Int )


type alias Actions =
    Spec.Element.Id.Dict ActionsForElement


type alias ActionsForElement =
    { onClick : Maybe ClickAction
    }


type ClickAction
    = Unspecified
    | Navigate NavigateParams
    | UpdateModel UpdateModelParams
    | MakeApiCall ApiCallParams


type alias ApiCallSlotIdentifier =
    ( Interface.JsonTree.Model.KeyPath, Maybe Interface.Selection.InterpolationVariableKey )


type alias ApiCallParams =
    { apiCallKey : Maybe ApiExplorer.Model.ApiCallKey
    , modelToCallMap : ModelToCallMap
    }


type alias UpdateModelParams =
    Dict.Dict Int (Maybe Dynamic.Data.Instance)


type alias NavigateParams =
    { linkLocation : TargetLocation
    , openIn : OpenTarget
    }


type TargetLocation
    = External ExternalUrl
    | Internal (Maybe Spec.Element.Id.Id)
    | Scroll (Maybe Spec.Element.Id.Id) (Maybe Int)


type alias ExternalUrl =
    { input : String
    , valid : Maybe Url
    }


type OpenTarget
    = NewTab
    | SameTab



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeActions =
    Spec.Element.Id.decodeDict decodeActionsForElement


decodeActionsForElement =
    Decode.map
        ActionsForElement
        (Decode.field "onClick" (Decode.maybe decodeClickAction))


decodeApiCallParams =
    Decode.map2
        ApiCallParams
        (Decode.field "apiCallKey" (Decode.maybe ApiExplorer.Model.decodeApiCallKey))
        (Decode.field "modelToCallMap" decodeModelToCallMap)


decodeApiCallSlotIdentifier =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Interface.JsonTree.Model.decodeKeyPath)
        (Decode.field "A2" (Decode.maybe Interface.Selection.decodeInterpolationVariableKey))


decodeClickAction =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeClickActionHelp


decodeClickActionHelp constructor =
    case constructor of
        "Unspecified" ->
            Decode.succeed Unspecified

        "Navigate" ->
            Decode.map
                Navigate
                (Decode.field "A1" decodeNavigateParams)

        "UpdateModel" ->
            Decode.map
                UpdateModel
                (Decode.field "A1" decodeUpdateModelParams)

        "MakeApiCall" ->
            Decode.map
                MakeApiCall
                (Decode.field "A1" decodeApiCallParams)

        other ->
            Decode.fail <| "Unknown constructor for type ClickAction: " ++ other


decodeEncodedModelToCallMap =
    Decode.list decodeTuple_ApiCallSlotIdentifier_Int_


decodeExternalUrl =
    Decode.map2
        ExternalUrl
        (Decode.field "input" Decode.string)
        (Decode.field "valid" (Decode.maybe decodeUrl))


decodeNavigateParams =
    Decode.map2
        NavigateParams
        (Decode.field "linkLocation" decodeTargetLocation)
        (Decode.field "openIn" decodeOpenTarget)


decodeOpenTarget =
    let
        recover x =
            case x of
                "NewTab" ->
                    Decode.succeed NewTab

                "SameTab" ->
                    Decode.succeed SameTab

                other ->
                    Decode.fail <| "Unknown constructor for type OpenTarget: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeTargetLocation =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeTargetLocationHelp


decodeTargetLocationHelp constructor =
    case constructor of
        "External" ->
            Decode.map
                External
                (Decode.field "A1" decodeExternalUrl)

        "Internal" ->
            Decode.map
                Internal
                (Decode.field "A1" (Decode.maybe Spec.Element.Id.decodeId))

        "Scroll" ->
            Decode.map2
                Scroll
                (Decode.field "A1" (Decode.maybe Spec.Element.Id.decodeId))
                (Decode.field "A2" (Decode.maybe Decode.int))

        other ->
            Decode.fail <| "Unknown constructor for type TargetLocation: " ++ other


decodeTuple_ApiCallSlotIdentifier_Int_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" decodeApiCallSlotIdentifier)
        (Decode.field "A2" Decode.int)


decodeUpdateModelParams =
    let
        decodeUpdateModelParamsTuple =
            Decode.map2
                (\a1 a2 -> ( a1, a2 ))
                (Decode.field "A1" Decode.int)
                (Decode.field "A2" (Decode.maybe Dynamic.Data.decodeInstance))
    in
    Decode.map Dict.fromList (Decode.list decodeUpdateModelParamsTuple)


encodeActions a =
    Spec.Element.Id.encodeDict encodeActionsForElement a


encodeActionsForElement a =
    Encode.object
        [ ( "onClick", encodeMaybeClickAction a.onClick )
        ]


encodeApiCallParams a =
    Encode.object
        [ ( "apiCallKey", encodeMaybeApiExplorer_Model_ApiCallKey a.apiCallKey )
        , ( "modelToCallMap", encodeModelToCallMap a.modelToCallMap )
        ]


encodeApiCallSlotIdentifier ( a1, a2 ) =
    Encode.object
        [ ( "A1", Interface.JsonTree.Model.encodeKeyPath a1 )
        , ( "A2", encodeMaybeApiExplorer_Api_Selection_InterpolationVariableKey a2 )
        ]


encodeClickAction a =
    case a of
        Unspecified ->
            Encode.object
                [ ( "Constructor", Encode.string "Unspecified" )
                ]

        Navigate a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Navigate" )
                , ( "A1", encodeNavigateParams a1 )
                ]

        UpdateModel a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "UpdateModel" )
                , ( "A1", encodeUpdateModelParams a1 )
                ]

        MakeApiCall a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "MakeApiCall" )
                , ( "A1", encodeApiCallParams a1 )
                ]


encodeEncodedModelToCallMap a =
    Encode.list encodeTuple_ApiCallSlotIdentifier_Int_ a


encodeExternalUrl a =
    Encode.object
        [ ( "input", Encode.string a.input )
        , ( "valid", encodeMaybeUrl a.valid )
        ]


encodeMaybeClickAction a =
    case a of
        Just b ->
            encodeClickAction b

        Nothing ->
            Encode.null


encodeMaybeApiExplorer_Api_Selection_InterpolationVariableKey a =
    case a of
        Just b ->
            Interface.Selection.encodeInterpolationVariableKey b

        Nothing ->
            Encode.null


encodeMaybeApiExplorer_Model_ApiCallKey a =
    case a of
        Just b ->
            ApiExplorer.Model.encodeApiCallKey b

        Nothing ->
            Encode.null


encodeMaybeDynamic_Data_Instance a =
    case a of
        Just b ->
            Dynamic.Data.encodeInstance b

        Nothing ->
            Encode.null


encodeMaybeInt a =
    case a of
        Just b ->
            Encode.int b

        Nothing ->
            Encode.null


encodeMaybeSpec_Element_Model_Id_Id a =
    case a of
        Just b ->
            Spec.Element.Id.encodeId b

        Nothing ->
            Encode.null


encodeMaybeUrl a =
    case a of
        Just b ->
            encodeUrl b

        Nothing ->
            Encode.null


encodeNavigateParams a =
    Encode.object
        [ ( "linkLocation", encodeTargetLocation a.linkLocation )
        , ( "openIn", encodeOpenTarget a.openIn )
        ]


encodeOpenTarget a =
    case a of
        NewTab ->
            Encode.string "NewTab"

        SameTab ->
            Encode.string "SameTab"


encodeTargetLocation a =
    case a of
        External a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "External" )
                , ( "A1", encodeExternalUrl a1 )
                ]

        Internal a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Internal" )
                , ( "A1", encodeMaybeSpec_Element_Model_Id_Id a1 )
                ]

        Scroll a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "Scroll" )
                , ( "A1", encodeMaybeSpec_Element_Model_Id_Id a1 )
                , ( "A2", encodeMaybeInt a2 )
                ]


encodeTuple_ApiCallSlotIdentifier_Int_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", encodeApiCallSlotIdentifier a1 )
        , ( "A2", Encode.int a2 )
        ]


encodeUpdateModelParams a =
    let
        encodeUpdateModelParamsTuple ( a1, a2 ) =
            Encode.object
                [ ( "A1", Encode.int a1 )
                , ( "A2", encodeMaybeDynamic_Data_Instance a2 )
                ]
    in
    Encode.list encodeUpdateModelParamsTuple (Dict.toList a)



-- [generator-end]
