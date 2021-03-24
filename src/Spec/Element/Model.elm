module Spec.Element.Model exposing (..)

{-| Model and transform the elements that live on a canvas, including their layout constraints
-}

import Canvas.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Spec.Element.Id
import Spec.Element.Layout
import Spec.Element.Layout.Length
import Spec.Element.Layout.Padding
import Viewport



---- DATA STRUCTURE ----


type alias Element outerGeometry =
    { shared : Shared
    , outerGeometry : outerGeometry
    }


type alias Screen =
    Element ScreenSize


type alias AbsoluteElement =
    Element AbsoluteElementDimensions


type alias FlowElement =
    Element SizeAndAlignment



-- [generator-start]


type ScreenSize
    = Preset Viewport.Device { x : Float, y : Float }
    | Custom Canvas.Events.AbsoluteRectangle


type alias Shared =
    { id : Spec.Element.Id.Id
    , padding : Spec.Element.Layout.Padding.Padding
    , spacing : Spec.Element.Layout.Spacing
    , flow : Spec.Element.Layout.Flow
    , children : Children
    , label : String
    , kind : ElementKind
    }


type ElementKind
    = Box
    | TextInput { modelField : Int, placeholder : String }
    | Button


type Children
    = FlowChildren (List FlowElement)
    | AbsoluteChildren (List AbsoluteElement)


type alias EitherElement =
    { shared : Shared
    , outerGeometry : EitherOuterGeometry
    }


type EitherOuterGeometry
    = ScreenGeometry ScreenSize
    | AbsoluteElementGeometry AbsoluteElementDimensions
    | FlowElementGeometry SizeAndAlignment


{-| This type captures the dimensions of an absolutely positioned element

The user needs to be able to manually input dimensions which is why we have a string-typed input

TODO:

  - refactor element module
  - make this an opaque type with unit-tested modifiers to ensure that inputs are always in sync
  - think about the implications for relative elements as well

-}
type alias AbsoluteElementDimensions =
    { width : Spec.Element.Layout.Length.Length
    , height : Spec.Element.Layout.Length.Length
    , x : Spec.Element.Layout.Length.Length
    , y : Spec.Element.Layout.Length.Length
    }


type alias SizeAndAlignment =
    { size : Spec.Element.Layout.Size
    , alignment : Spec.Element.Layout.Alignment
    }


type alias Screens =
    List Screen


encodeElement : (a -> Encode.Value) -> Element a -> Encode.Value
encodeElement encodeOuterGeometry { shared, outerGeometry } =
    Encode.object
        [ ( "shared", encodeShared shared )
        , ( "outerGeometry", encodeOuterGeometry outerGeometry )
        ]


decodeElement : Decode.Decoder outerGeometry -> Decode.Decoder (Element outerGeometry)
decodeElement decodeOuterGeometry =
    Decode.map2 (\shared outerGeometry -> { shared = shared, outerGeometry = outerGeometry })
        (Decode.field "shared" decodeShared)
        (Decode.field "outerGeometry" decodeOuterGeometry)


{-| @@TODO: improve in coder-generator:

  - fix circulars
  - fix parameterization

-}
encodeScreen =
    encodeElement encodeScreenSize


decodeScreen : Decode.Decoder Screen
decodeScreen =
    decodeElement decodeScreenSize



-- Decode.map2
--   (\ rectangle viewport ->
--       { rectangle = rectangle
--       , viewport = viewport
--       }
--   )
--   (Decode.field "rectangle" Canvas.Events.decodeAbsoluteRectangle)
--   (Decode.field "viewport" Viewport.decodeViewport)
-- |> decodeElement


encodeAbsoluteElement =
    encodeElement encodeAbsoluteElementDimensions


decodeAbsoluteElement =
    Decode.lazy <| \_ -> decodeElement decodeAbsoluteElementDimensions



-- @@TODO: report compiler bug at previous version


encodeFlowElement : FlowElement -> Encode.Value
encodeFlowElement =
    encodeElement
        (\{ size, alignment } ->
            Encode.object
                [ ( "size", Spec.Element.Layout.encodeSize size )
                , ( "alignment", Spec.Element.Layout.encodeAlignment alignment )
                ]
        )


decodeFlowElement : Decode.Decoder FlowElement
decodeFlowElement =
    let
        decodeGeometry =
            Decode.map2
                (\size alignment ->
                    { size = size, alignment = alignment }
                )
                (Decode.field "size" Spec.Element.Layout.decodeSize)
                (Decode.field "alignment" Spec.Element.Layout.decodeAlignment)
    in
    Decode.lazy <| \_ -> decodeElement decodeGeometry



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


type alias Record_modelField_Int_placeholder_String_ =
    { modelField : Int, placeholder : String }


type alias Record_x_Float_y_Float_ =
    { x : Float, y : Float }


decodeAbsoluteElementDimensions =
    Decode.map4
        AbsoluteElementDimensions
        (Decode.field "width" Spec.Element.Layout.Length.decodeLength)
        (Decode.field "height" Spec.Element.Layout.Length.decodeLength)
        (Decode.field "x" Spec.Element.Layout.Length.decodeLength)
        (Decode.field "y" Spec.Element.Layout.Length.decodeLength)


decodeChildren =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeChildrenHelp


decodeChildrenHelp constructor =
    case constructor of
        "FlowChildren" ->
            Decode.map
                FlowChildren
                (Decode.field "A1" (Decode.list decodeFlowElement))

        "AbsoluteChildren" ->
            Decode.map
                AbsoluteChildren
                (Decode.field "A1" (Decode.list decodeAbsoluteElement))

        other ->
            Decode.fail <| "Unknown constructor for type Children: " ++ other


decodeEitherElement =
    Decode.map2
        EitherElement
        (Decode.field "shared" decodeShared)
        (Decode.field "outerGeometry" decodeEitherOuterGeometry)


decodeEitherOuterGeometry =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeEitherOuterGeometryHelp


decodeEitherOuterGeometryHelp constructor =
    case constructor of
        "ScreenGeometry" ->
            Decode.map
                ScreenGeometry
                (Decode.field "A1" decodeScreenSize)

        "AbsoluteElementGeometry" ->
            Decode.map
                AbsoluteElementGeometry
                (Decode.field "A1" decodeAbsoluteElementDimensions)

        "FlowElementGeometry" ->
            Decode.map
                FlowElementGeometry
                (Decode.field "A1" decodeSizeAndAlignment)

        other ->
            Decode.fail <| "Unknown constructor for type EitherOuterGeometry: " ++ other


decodeElementKind =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeElementKindHelp


decodeElementKindHelp constructor =
    case constructor of
        "Box" ->
            Decode.succeed Box

        "TextInput" ->
            Decode.map
                TextInput
                (Decode.field "A1" decodeRecord_modelField_Int_placeholder_String_)

        "Button" ->
            Decode.succeed Button

        other ->
            Decode.fail <| "Unknown constructor for type ElementKind: " ++ other


decodeRecord_modelField_Int_placeholder_String_ =
    Decode.map2
        Record_modelField_Int_placeholder_String_
        (Decode.field "modelField" Decode.int)
        (Decode.field "placeholder" Decode.string)


decodeRecord_x_Float_y_Float_ =
    Decode.map2
        Record_x_Float_y_Float_
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


decodeScreenSize =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeScreenSizeHelp


decodeScreenSizeHelp constructor =
    case constructor of
        "Preset" ->
            Decode.map2
                Preset
                (Decode.field "A1" Viewport.decodeDevice)
                (Decode.field "A2" decodeRecord_x_Float_y_Float_)

        "Custom" ->
            Decode.map
                Custom
                (Decode.field "A1" Canvas.Events.decodeAbsoluteRectangle)

        other ->
            Decode.fail <| "Unknown constructor for type ScreenSize: " ++ other


decodeScreens =
    Decode.list decodeScreen


decodeShared =
    Decode.map7
        Shared
        (Decode.field "id" Spec.Element.Id.decodeId)
        (Decode.field "padding" Spec.Element.Layout.Padding.decodePadding)
        (Decode.field "spacing" Spec.Element.Layout.decodeSpacing)
        (Decode.field "flow" Spec.Element.Layout.decodeFlow)
        (Decode.field "children" decodeChildren)
        (Decode.field "label" Decode.string)
        (Decode.field "kind" decodeElementKind)


decodeSizeAndAlignment =
    Decode.map2
        SizeAndAlignment
        (Decode.field "size" Spec.Element.Layout.decodeSize)
        (Decode.field "alignment" Spec.Element.Layout.decodeAlignment)


encodeAbsoluteElementDimensions a =
    Encode.object
        [ ( "width", Spec.Element.Layout.Length.encodeLength a.width )
        , ( "height", Spec.Element.Layout.Length.encodeLength a.height )
        , ( "x", Spec.Element.Layout.Length.encodeLength a.x )
        , ( "y", Spec.Element.Layout.Length.encodeLength a.y )
        ]


encodeChildren a =
    case a of
        FlowChildren a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "FlowChildren" )
                , ( "A1", Encode.list encodeFlowElement a1 )
                ]

        AbsoluteChildren a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "AbsoluteChildren" )
                , ( "A1", Encode.list encodeAbsoluteElement a1 )
                ]


encodeEitherElement a =
    Encode.object
        [ ( "shared", encodeShared a.shared )
        , ( "outerGeometry", encodeEitherOuterGeometry a.outerGeometry )
        ]


encodeEitherOuterGeometry a =
    case a of
        ScreenGeometry a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "ScreenGeometry" )
                , ( "A1", encodeScreenSize a1 )
                ]

        AbsoluteElementGeometry a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "AbsoluteElementGeometry" )
                , ( "A1", encodeAbsoluteElementDimensions a1 )
                ]

        FlowElementGeometry a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "FlowElementGeometry" )
                , ( "A1", encodeSizeAndAlignment a1 )
                ]


encodeElementKind a =
    case a of
        Box ->
            Encode.object
                [ ( "Constructor", Encode.string "Box" )
                ]

        TextInput a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "TextInput" )
                , ( "A1", encodeRecord_modelField_Int_placeholder_String_ a1 )
                ]

        Button ->
            Encode.object
                [ ( "Constructor", Encode.string "Button" )
                ]


encodeRecord_modelField_Int_placeholder_String_ a =
    Encode.object
        [ ( "modelField", Encode.int a.modelField )
        , ( "placeholder", Encode.string a.placeholder )
        ]


encodeRecord_x_Float_y_Float_ a =
    Encode.object
        [ ( "x", Encode.float a.x )
        , ( "y", Encode.float a.y )
        ]


encodeScreenSize a =
    case a of
        Preset a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "Preset" )
                , ( "A1", Viewport.encodeDevice a1 )
                , ( "A2", encodeRecord_x_Float_y_Float_ a2 )
                ]

        Custom a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Custom" )
                , ( "A1", Canvas.Events.encodeAbsoluteRectangle a1 )
                ]


encodeScreens a =
    Encode.list encodeScreen a


encodeShared a =
    Encode.object
        [ ( "id", Spec.Element.Id.encodeId a.id )
        , ( "padding", Spec.Element.Layout.Padding.encodePadding a.padding )
        , ( "spacing", Spec.Element.Layout.encodeSpacing a.spacing )
        , ( "flow", Spec.Element.Layout.encodeFlow a.flow )
        , ( "children", encodeChildren a.children )
        , ( "label", Encode.string a.label )
        , ( "kind", encodeElementKind a.kind )
        ]


encodeSizeAndAlignment a =
    Encode.object
        [ ( "size", Spec.Element.Layout.encodeSize a.size )
        , ( "alignment", Spec.Element.Layout.encodeAlignment a.alignment )
        ]



-- [generator-end]
