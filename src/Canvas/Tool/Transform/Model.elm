module Canvas.Tool.Transform.Model exposing (..)

import Canvas.Events
import Canvas.Selection
import Compass
import Json.Decode as Decode
import Json.Encode as Encode
import Spec.Element.Id
import Spec.Element.Model



-- [generator-start]


type alias MouseDownDataOnElement =
    { handleLocation : MouseDownLocation
    , element : Spec.Element.Model.EitherElement
    , selectionItem : Canvas.Selection.SelectionItem
    , originalElementGeometry : Canvas.Events.SceneRectangle
    , parentGeometry : Canvas.Events.SceneRectangle
    , siblingsDimensions : List ( Spec.Element.Id.Id, Canvas.Events.SceneRectangle )
    , parentElement : Spec.Element.Model.EitherElement
    }


type alias MouseDownDataOnScreen =
    { handleLocation : MouseDownLocation
    , element : Spec.Element.Model.Screen
    , selectionItem : Canvas.Selection.SelectionItem
    , originalElementGeometry : Canvas.Events.SceneRectangle
    , siblingsDimensions : List ( Spec.Element.Id.Id, Canvas.Events.SceneRectangle )
    }


type MouseDownLocation
    = CompassDirection Compass.Direction
    | SomewhereOnElement Canvas.Events.AbsolutePoint


type State
    = NotTransforming
    | HandleGrabbedOnElement MouseDownDataOnElement
    | TransformingElement MouseDownDataOnElement TransformTarget
    | TransformingScreen MouseDownDataOnScreen TransformTarget
    | HandleGrabbedOnScreen MouseDownDataOnScreen


type TransformTarget
    = ToScene Canvas.Events.ScenePoint
    | ToElement ElementTarget


type alias ElementTarget =
    { selectionItem : Canvas.Selection.SelectionItem
    , element : Spec.Element.Model.EitherElement
    , point : Canvas.Events.ElementPoint
    , targetAbs : Canvas.Events.AbsoluteRectangle
    , relChildren : List ( Spec.Element.Id.Id, Canvas.Events.ElementRectangle )
    }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeElementTarget =
    Decode.map5
        ElementTarget
        (Decode.field "selectionItem" Canvas.Selection.decodeSelectionItem)
        (Decode.field "element" Spec.Element.Model.decodeEitherElement)
        (Decode.field "point" Canvas.Events.decodeElementPoint)
        (Decode.field "targetAbs" Canvas.Events.decodeAbsoluteRectangle)
        (Decode.field "relChildren" (Decode.list decodeTuple_Spec_Element_Id_Id_Canvas_Events_ElementRectangle_))


decodeMouseDownDataOnElement =
    Decode.map7
        MouseDownDataOnElement
        (Decode.field "handleLocation" decodeMouseDownLocation)
        (Decode.field "element" Spec.Element.Model.decodeEitherElement)
        (Decode.field "selectionItem" Canvas.Selection.decodeSelectionItem)
        (Decode.field "originalElementGeometry" Canvas.Events.decodeSceneRectangle)
        (Decode.field "parentGeometry" Canvas.Events.decodeSceneRectangle)
        (Decode.field "siblingsDimensions" (Decode.list decodeTuple_Spec_Element_Id_Id_Canvas_Events_SceneRectangle_))
        (Decode.field "parentElement" Spec.Element.Model.decodeEitherElement)


decodeMouseDownDataOnScreen =
    Decode.map5
        MouseDownDataOnScreen
        (Decode.field "handleLocation" decodeMouseDownLocation)
        (Decode.field "element" Spec.Element.Model.decodeScreen)
        (Decode.field "selectionItem" Canvas.Selection.decodeSelectionItem)
        (Decode.field "originalElementGeometry" Canvas.Events.decodeSceneRectangle)
        (Decode.field "siblingsDimensions" (Decode.list decodeTuple_Spec_Element_Id_Id_Canvas_Events_SceneRectangle_))


decodeMouseDownLocation =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeMouseDownLocationHelp


decodeMouseDownLocationHelp constructor =
    case constructor of
        "CompassDirection" ->
            Decode.map
                CompassDirection
                (Decode.field "A1" Compass.decodeDirection)

        "SomewhereOnElement" ->
            Decode.map
                SomewhereOnElement
                (Decode.field "A1" Canvas.Events.decodeAbsolutePoint)

        other ->
            Decode.fail <| "Unknown constructor for type MouseDownLocation: " ++ other


decodeState =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeStateHelp


decodeStateHelp constructor =
    case constructor of
        "NotTransforming" ->
            Decode.succeed NotTransforming

        "HandleGrabbedOnElement" ->
            Decode.map
                HandleGrabbedOnElement
                (Decode.field "A1" decodeMouseDownDataOnElement)

        "TransformingElement" ->
            Decode.map2
                TransformingElement
                (Decode.field "A1" decodeMouseDownDataOnElement)
                (Decode.field "A2" decodeTransformTarget)

        "TransformingScreen" ->
            Decode.map2
                TransformingScreen
                (Decode.field "A1" decodeMouseDownDataOnScreen)
                (Decode.field "A2" decodeTransformTarget)

        "HandleGrabbedOnScreen" ->
            Decode.map
                HandleGrabbedOnScreen
                (Decode.field "A1" decodeMouseDownDataOnScreen)

        other ->
            Decode.fail <| "Unknown constructor for type State: " ++ other


decodeTransformTarget =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeTransformTargetHelp


decodeTransformTargetHelp constructor =
    case constructor of
        "ToScene" ->
            Decode.map
                ToScene
                (Decode.field "A1" Canvas.Events.decodeScenePoint)

        "ToElement" ->
            Decode.map
                ToElement
                (Decode.field "A1" decodeElementTarget)

        other ->
            Decode.fail <| "Unknown constructor for type TransformTarget: " ++ other


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


encodeElementTarget a =
    Encode.object
        [ ( "selectionItem", Canvas.Selection.encodeSelectionItem a.selectionItem )
        , ( "element", Spec.Element.Model.encodeEitherElement a.element )
        , ( "point", Canvas.Events.encodeElementPoint a.point )
        , ( "targetAbs", Canvas.Events.encodeAbsoluteRectangle a.targetAbs )
        , ( "relChildren", Encode.list encodeTuple_Spec_Element_Id_Id_Canvas_Events_ElementRectangle_ a.relChildren )
        ]


encodeMouseDownDataOnElement a =
    Encode.object
        [ ( "handleLocation", encodeMouseDownLocation a.handleLocation )
        , ( "element", Spec.Element.Model.encodeEitherElement a.element )
        , ( "selectionItem", Canvas.Selection.encodeSelectionItem a.selectionItem )
        , ( "originalElementGeometry", Canvas.Events.encodeSceneRectangle a.originalElementGeometry )
        , ( "parentGeometry", Canvas.Events.encodeSceneRectangle a.parentGeometry )
        , ( "siblingsDimensions", Encode.list encodeTuple_Spec_Element_Id_Id_Canvas_Events_SceneRectangle_ a.siblingsDimensions )
        , ( "parentElement", Spec.Element.Model.encodeEitherElement a.parentElement )
        ]


encodeMouseDownDataOnScreen a =
    Encode.object
        [ ( "handleLocation", encodeMouseDownLocation a.handleLocation )
        , ( "element", Spec.Element.Model.encodeScreen a.element )
        , ( "selectionItem", Canvas.Selection.encodeSelectionItem a.selectionItem )
        , ( "originalElementGeometry", Canvas.Events.encodeSceneRectangle a.originalElementGeometry )
        , ( "siblingsDimensions", Encode.list encodeTuple_Spec_Element_Id_Id_Canvas_Events_SceneRectangle_ a.siblingsDimensions )
        ]


encodeMouseDownLocation a =
    case a of
        CompassDirection a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "CompassDirection" )
                , ( "A1", Compass.encodeDirection a1 )
                ]

        SomewhereOnElement a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "SomewhereOnElement" )
                , ( "A1", Canvas.Events.encodeAbsolutePoint a1 )
                ]


encodeState a =
    case a of
        NotTransforming ->
            Encode.object
                [ ( "Constructor", Encode.string "NotTransforming" )
                ]

        HandleGrabbedOnElement a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "HandleGrabbedOnElement" )
                , ( "A1", encodeMouseDownDataOnElement a1 )
                ]

        TransformingElement a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "TransformingElement" )
                , ( "A1", encodeMouseDownDataOnElement a1 )
                , ( "A2", encodeTransformTarget a2 )
                ]

        TransformingScreen a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "TransformingScreen" )
                , ( "A1", encodeMouseDownDataOnScreen a1 )
                , ( "A2", encodeTransformTarget a2 )
                ]

        HandleGrabbedOnScreen a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "HandleGrabbedOnScreen" )
                , ( "A1", encodeMouseDownDataOnScreen a1 )
                ]


encodeTransformTarget a =
    case a of
        ToScene a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "ToScene" )
                , ( "A1", Canvas.Events.encodeScenePoint a1 )
                ]

        ToElement a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "ToElement" )
                , ( "A1", encodeElementTarget a1 )
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
