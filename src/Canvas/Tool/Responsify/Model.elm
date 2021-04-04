module Canvas.Tool.Responsify.Model exposing (..)

import Canvas.Events
import Canvas.Selection
import Json.Decode as Decode
import Json.Encode as Encode
import Spec.Element.Id
import Spec.Element.Model


init : State
init =
    Closed



-- [generator-start]


type alias ElementPressData =
    { sourceElement : Spec.Element.Model.Screen
    , siblingsDimensions : List Canvas.Events.SceneRectangle
    , point : Canvas.Events.ElementPointInContext
    }


type alias LayoutData =
    { parent : ( Spec.Element.Id.Id, Canvas.Events.AbsoluteRectangle )
    , children : List ( Spec.Element.Id.Id, Canvas.Events.ElementRectangle )
    }


type State
    = Closed
    | LayingOut LayingOutData


type alias LayingOutData =
    { targetSelectionItem : Canvas.Selection.SelectionItem
    , layoutData : LayoutData
    , predictionParams : PredictionParams
    , element : Spec.Element.Model.EitherElement
    , prediction : Maybe Spec.Element.Model.EitherElement
    }


type alias PredictionParams =
    { parentDimensions : ( Spec.Element.Id.Id, Canvas.Events.ElementRectangle )
    , siblingsDimensions : List ( Spec.Element.Id.Id, Canvas.Events.ElementRectangle )
    , element : Spec.Element.Model.EitherElement
    }


type RawEventData
    = OnSceneEventData Canvas.Events.ScenePoint
    | OnElementEventData ElementPressData


type alias ResponsifyExport =
    { input : PredictionParams
    , prediction : Spec.Element.Model.EitherElement
    }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeElementPressData =
    Decode.map3
        ElementPressData
        (Decode.field "sourceElement" Spec.Element.Model.decodeScreen)
        (Decode.field "siblingsDimensions" (Decode.list Canvas.Events.decodeSceneRectangle))
        (Decode.field "point" Canvas.Events.decodeElementPointInContext)


decodeLayingOutData =
    Decode.map5
        LayingOutData
        (Decode.field "targetSelectionItem" Canvas.Selection.decodeSelectionItem)
        (Decode.field "layoutData" decodeLayoutData)
        (Decode.field "predictionParams" decodePredictionParams)
        (Decode.field "element" Spec.Element.Model.decodeEitherElement)
        (Decode.field "prediction" (Decode.maybe Spec.Element.Model.decodeEitherElement))


decodeLayoutData =
    Decode.map2
        LayoutData
        (Decode.field "parent" decodeTuple_Spec_Element_Id_Id_Canvas_Events_AbsoluteRectangle_)
        (Decode.field "children" (Decode.list decodeTuple_Spec_Element_Id_Id_Canvas_Events_ElementRectangle_))


decodePredictionParams =
    Decode.map3
        PredictionParams
        (Decode.field "parentDimensions" decodeTuple_Spec_Element_Id_Id_Canvas_Events_ElementRectangle_)
        (Decode.field "siblingsDimensions" (Decode.list decodeTuple_Spec_Element_Id_Id_Canvas_Events_ElementRectangle_))
        (Decode.field "element" Spec.Element.Model.decodeEitherElement)


decodeRawEventData =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeRawEventDataHelp


decodeRawEventDataHelp constructor =
    case constructor of
        "OnSceneEventData" ->
            Decode.map
                OnSceneEventData
                (Decode.field "A1" Canvas.Events.decodeScenePoint)

        "OnElementEventData" ->
            Decode.map
                OnElementEventData
                (Decode.field "A1" decodeElementPressData)

        other ->
            Decode.fail <| "Unknown constructor for type RawEventData: " ++ other


decodeResponsifyExport =
    Decode.map2
        ResponsifyExport
        (Decode.field "input" decodePredictionParams)
        (Decode.field "prediction" Spec.Element.Model.decodeEitherElement)


decodeState =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeStateHelp


decodeStateHelp constructor =
    case constructor of
        "Closed" ->
            Decode.succeed Closed

        "LayingOut" ->
            Decode.map
                LayingOut
                (Decode.field "A1" decodeLayingOutData)

        other ->
            Decode.fail <| "Unknown constructor for type State: " ++ other


decodeTuple_Spec_Element_Id_Id_Canvas_Events_AbsoluteRectangle_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Spec.Element.Id.decodeId)
        (Decode.field "A2" Canvas.Events.decodeAbsoluteRectangle)


decodeTuple_Spec_Element_Id_Id_Canvas_Events_ElementRectangle_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Spec.Element.Id.decodeId)
        (Decode.field "A2" Canvas.Events.decodeElementRectangle)


encodeElementPressData a =
    Encode.object
        [ ( "sourceElement", Spec.Element.Model.encodeScreen a.sourceElement )
        , ( "siblingsDimensions", Encode.list Canvas.Events.encodeSceneRectangle a.siblingsDimensions )
        , ( "point", Canvas.Events.encodeElementPointInContext a.point )
        ]


encodeLayingOutData a =
    Encode.object
        [ ( "targetSelectionItem", Canvas.Selection.encodeSelectionItem a.targetSelectionItem )
        , ( "layoutData", encodeLayoutData a.layoutData )
        , ( "predictionParams", encodePredictionParams a.predictionParams )
        , ( "element", Spec.Element.Model.encodeEitherElement a.element )
        , ( "prediction", encodeMaybeSpec_Element_Model_EitherElement a.prediction )
        ]


encodeLayoutData a =
    Encode.object
        [ ( "parent", encodeTuple_Spec_Element_Id_Id_Canvas_Events_AbsoluteRectangle_ a.parent )
        , ( "children", Encode.list encodeTuple_Spec_Element_Id_Id_Canvas_Events_ElementRectangle_ a.children )
        ]


encodeMaybeSpec_Element_Model_EitherElement a =
    case a of
        Just b ->
            Spec.Element.Model.encodeEitherElement b

        Nothing ->
            Encode.null


encodePredictionParams a =
    Encode.object
        [ ( "parentDimensions", encodeTuple_Spec_Element_Id_Id_Canvas_Events_ElementRectangle_ a.parentDimensions )
        , ( "siblingsDimensions", Encode.list encodeTuple_Spec_Element_Id_Id_Canvas_Events_ElementRectangle_ a.siblingsDimensions )
        , ( "element", Spec.Element.Model.encodeEitherElement a.element )
        ]


encodeRawEventData a =
    case a of
        OnSceneEventData a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "OnSceneEventData" )
                , ( "A1", Canvas.Events.encodeScenePoint a1 )
                ]

        OnElementEventData a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "OnElementEventData" )
                , ( "A1", encodeElementPressData a1 )
                ]


encodeResponsifyExport a =
    Encode.object
        [ ( "input", encodePredictionParams a.input )
        , ( "prediction", Spec.Element.Model.encodeEitherElement a.prediction )
        ]


encodeState a =
    case a of
        Closed ->
            Encode.object
                [ ( "Constructor", Encode.string "Closed" )
                ]

        LayingOut a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "LayingOut" )
                , ( "A1", encodeLayingOutData a1 )
                ]


encodeTuple_Spec_Element_Id_Id_Canvas_Events_AbsoluteRectangle_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", Spec.Element.Id.encodeId a1 )
        , ( "A2", Canvas.Events.encodeAbsoluteRectangle a2 )
        ]


encodeTuple_Spec_Element_Id_Id_Canvas_Events_ElementRectangle_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", Spec.Element.Id.encodeId a1 )
        , ( "A2", Canvas.Events.encodeElementRectangle a2 )
        ]



-- [generator-end]
