module Canvas.Tool.Draw.Model exposing (..)

import Canvas.Events
import Canvas.Guides
import Canvas.Selection
import Json.Decode as Decode
import Json.Encode as Encode
import Rectangle
import Spec.Element.Id
import Spec.Element.Model


type alias SnapInfo =
    { alignments : List Canvas.Guides.Alignment
    , sources : Spec.Element.Id.Dict Rectangle.Rectangle
    }


type alias ElementDrawResultData =
    { snappedRect : Canvas.Events.ElementRectangle
    , containedSiblings : List Spec.Element.Model.AbsoluteElement
    , notContainedSiblings : List Spec.Element.Model.AbsoluteElement
    , snapInfo : SnapInfo
    , selectionItem : Canvas.Selection.SelectionItem
    }



-- [generator-start]


type Mode
    = Box
    | TextInput
    | Button
    | Text


type alias Containments =
    List ( Spec.Element.Id.Id, Canvas.Events.ElementRectangle )


type alias OnElementData =
    { sourceElement : ( Canvas.Selection.SelectionItem, Spec.Element.Model.EitherElement )
    , siblingsDimensions : List ( Spec.Element.Id.Id, Canvas.Events.SceneRectangle )
    , point : Canvas.Events.ElementPointInContext
    }


type alias OnSceneData =
    { point : Canvas.Events.ScenePoint
    , siblingsDimensions : List ( Spec.Element.Id.Id, Canvas.Events.SceneRectangle )
    }


type alias State =
    { drawState : DrawState
    , sticky : Bool
    , mode : Mode
    }


type DrawState
    = NotDrawing
    | Drawing RawEventData RawEventData


type RawEventData
    = OnSceneEventData OnSceneData
    | OnElementEventData OnElementData



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeContainments =
    Decode.list decodeTuple_Spec_Element_Id_Id_Canvas_Events_ElementRectangle_


decodeDrawState =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeDrawStateHelp


decodeDrawStateHelp constructor =
    case constructor of
        "NotDrawing" ->
            Decode.succeed NotDrawing

        "Drawing" ->
            Decode.map2
                Drawing
                (Decode.field "A1" decodeRawEventData)
                (Decode.field "A2" decodeRawEventData)

        other ->
            Decode.fail <| "Unknown constructor for type DrawState: " ++ other


decodeMode =
    let
        recover x =
            case x of
                "Box" ->
                    Decode.succeed Box

                "TextInput" ->
                    Decode.succeed TextInput

                "Button" ->
                    Decode.succeed Button

                "Text" ->
                    Decode.succeed Text

                other ->
                    Decode.fail <| "Unknown constructor for type Mode: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeOnElementData =
    Decode.map3
        OnElementData
        (Decode.field "sourceElement" decodeTuple_Canvas_Selection_SelectionItem_Spec_Element_Model_EitherElement_)
        (Decode.field "siblingsDimensions" (Decode.list decodeTuple_Spec_Element_Id_Id_Canvas_Events_SceneRectangle_))
        (Decode.field "point" Canvas.Events.decodeElementPointInContext)


decodeOnSceneData =
    Decode.map2
        OnSceneData
        (Decode.field "point" Canvas.Events.decodeScenePoint)
        (Decode.field "siblingsDimensions" (Decode.list decodeTuple_Spec_Element_Id_Id_Canvas_Events_SceneRectangle_))


decodeRawEventData =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeRawEventDataHelp


decodeRawEventDataHelp constructor =
    case constructor of
        "OnSceneEventData" ->
            Decode.map
                OnSceneEventData
                (Decode.field "A1" decodeOnSceneData)

        "OnElementEventData" ->
            Decode.map
                OnElementEventData
                (Decode.field "A1" decodeOnElementData)

        other ->
            Decode.fail <| "Unknown constructor for type RawEventData: " ++ other


decodeState =
    Decode.map3
        State
        (Decode.field "drawState" decodeDrawState)
        (Decode.field "sticky" Decode.bool)
        (Decode.field "mode" decodeMode)


decodeTuple_Canvas_Selection_SelectionItem_Spec_Element_Model_EitherElement_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Canvas.Selection.decodeSelectionItem)
        (Decode.field "A2" Spec.Element.Model.decodeEitherElement)


decodeTuple_Spec_Element_Id_Id_Canvas_Events_ElementRectangle_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Spec.Element.Id.decodeId)
        (Decode.field "A2" Canvas.Events.decodeElementRectangle)


decodeTuple_Spec_Element_Id_Id_Canvas_Events_SceneRectangle_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Spec.Element.Id.decodeId)
        (Decode.field "A2" Canvas.Events.decodeSceneRectangle)


encodeContainments a =
    Encode.list encodeTuple_Spec_Element_Id_Id_Canvas_Events_ElementRectangle_ a


encodeDrawState a =
    case a of
        NotDrawing ->
            Encode.object
                [ ( "Constructor", Encode.string "NotDrawing" )
                ]

        Drawing a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "Drawing" )
                , ( "A1", encodeRawEventData a1 )
                , ( "A2", encodeRawEventData a2 )
                ]


encodeMode a =
    case a of
        Box ->
            Encode.string "Box"

        TextInput ->
            Encode.string "TextInput"

        Button ->
            Encode.string "Button"

        Text ->
            Encode.string "Text"


encodeOnElementData a =
    Encode.object
        [ ( "sourceElement", encodeTuple_Canvas_Selection_SelectionItem_Spec_Element_Model_EitherElement_ a.sourceElement )
        , ( "siblingsDimensions", Encode.list encodeTuple_Spec_Element_Id_Id_Canvas_Events_SceneRectangle_ a.siblingsDimensions )
        , ( "point", Canvas.Events.encodeElementPointInContext a.point )
        ]


encodeOnSceneData a =
    Encode.object
        [ ( "point", Canvas.Events.encodeScenePoint a.point )
        , ( "siblingsDimensions", Encode.list encodeTuple_Spec_Element_Id_Id_Canvas_Events_SceneRectangle_ a.siblingsDimensions )
        ]


encodeRawEventData a =
    case a of
        OnSceneEventData a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "OnSceneEventData" )
                , ( "A1", encodeOnSceneData a1 )
                ]

        OnElementEventData a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "OnElementEventData" )
                , ( "A1", encodeOnElementData a1 )
                ]


encodeState a =
    Encode.object
        [ ( "drawState", encodeDrawState a.drawState )
        , ( "sticky", Encode.bool a.sticky )
        , ( "mode", encodeMode a.mode )
        ]


encodeTuple_Canvas_Selection_SelectionItem_Spec_Element_Model_EitherElement_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", Canvas.Selection.encodeSelectionItem a1 )
        , ( "A2", Spec.Element.Model.encodeEitherElement a2 )
        ]


encodeTuple_Spec_Element_Id_Id_Canvas_Events_ElementRectangle_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", Spec.Element.Id.encodeId a1 )
        , ( "A2", Canvas.Events.encodeElementRectangle a2 )
        ]


encodeTuple_Spec_Element_Id_Id_Canvas_Events_SceneRectangle_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", Spec.Element.Id.encodeId a1 )
        , ( "A2", Canvas.Events.encodeSceneRectangle a2 )
        ]



-- [generator-end]
