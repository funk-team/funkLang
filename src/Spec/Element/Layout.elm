module Spec.Element.Layout exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Spec.Element.Layout.Length
import Spec.Element.Layout.Padding



-- legacy type: TODO: remove


type Kind
    = Group Layout


flowToLabel f =
    case f of
        Column ->
            "Column"

        Row ->
            "Row"

        WrappedRow ->
            "Wrapped row"


flows =
    [ Column
    , Row
    , WrappedRow -- TODO: replace with GRID
    ]


decodeFlow_ : Flow -> String
decodeFlow_ flo =
    case flo of
        Column ->
            "Column"

        Row ->
            "Row"

        WrappedRow ->
            "Wrapped Row"


noLimits =
    MinMax Spec.Element.Layout.Length.null Spec.Element.Layout.Length.null



-- [generator-start]


type alias Layout =
    { layout : Flow
    , alignment : Alignment
    , size : Size
    , padding : Spec.Element.Layout.Padding.Padding
    , spacing : Spacing
    }


type alias Size =
    { width : Length
    , height : Length
    }


type alias Spacing =
    Maybe Int


type Flow
    = Column
    | Row
    | WrappedRow


{-| Dynamic representation of elm-ui length.
There is an auto value because row, col etc. have some default dimension. these we call auto.
-}
type alias Length =
    { behavior : Behavior
    , minMax : MinMax
    }


type Behavior
    = Static Spec.Element.Layout.Length.Length
    | Fill
    | Shrink


type alias MinMax =
    { min : Spec.Element.Layout.Length.NullableLength
    , max : Spec.Element.Layout.Length.NullableLength
    }


type AbsoluteLength
    = AbsoluteLength (Maybe Spec.Element.Layout.Length.Length)


type Constraint
    = NoConstraint
    | Minimium (Maybe Spec.Element.Layout.Length.Length)
    | Maximium (Maybe Spec.Element.Layout.Length.Length)


type alias Portion =
    Int


type alias Alignment =
    { x : Maybe AlignmentX, y : Maybe AlignmentY }


type AlignmentX
    = Left
    | CenterX
    | Right


type AlignmentY
    = Top
    | CenterY
    | Bottom



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeAbsoluteLength =
    Decode.map AbsoluteLength (Decode.maybe Spec.Element.Layout.Length.decodeLength)


decodeAlignment =
    Decode.map2
        Alignment
        (Decode.field "x" (Decode.maybe decodeAlignmentX))
        (Decode.field "y" (Decode.maybe decodeAlignmentY))


decodeAlignmentX =
    let
        recover x =
            case x of
                "Left" ->
                    Decode.succeed Left

                "CenterX" ->
                    Decode.succeed CenterX

                "Right" ->
                    Decode.succeed Right

                other ->
                    Decode.fail <| "Unknown constructor for type AlignmentX: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeAlignmentY =
    let
        recover x =
            case x of
                "Top" ->
                    Decode.succeed Top

                "CenterY" ->
                    Decode.succeed CenterY

                "Bottom" ->
                    Decode.succeed Bottom

                other ->
                    Decode.fail <| "Unknown constructor for type AlignmentY: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeBehavior =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeBehaviorHelp


decodeBehaviorHelp constructor =
    case constructor of
        "Static" ->
            Decode.map
                Static
                (Decode.field "A1" Spec.Element.Layout.Length.decodeLength)

        "Fill" ->
            Decode.succeed Fill

        "Shrink" ->
            Decode.succeed Shrink

        other ->
            Decode.fail <| "Unknown constructor for type Behavior: " ++ other


decodeConstraint =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeConstraintHelp


decodeConstraintHelp constructor =
    case constructor of
        "NoConstraint" ->
            Decode.succeed NoConstraint

        "Minimium" ->
            Decode.map
                Minimium
                (Decode.field "A1" (Decode.maybe Spec.Element.Layout.Length.decodeLength))

        "Maximium" ->
            Decode.map
                Maximium
                (Decode.field "A1" (Decode.maybe Spec.Element.Layout.Length.decodeLength))

        other ->
            Decode.fail <| "Unknown constructor for type Constraint: " ++ other


decodeFlow =
    let
        recover x =
            case x of
                "Column" ->
                    Decode.succeed Column

                "Row" ->
                    Decode.succeed Row

                "WrappedRow" ->
                    Decode.succeed WrappedRow

                other ->
                    Decode.fail <| "Unknown constructor for type Flow: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeLayout =
    Decode.map5
        Layout
        (Decode.field "layout" decodeFlow)
        (Decode.field "alignment" decodeAlignment)
        (Decode.field "size" decodeSize)
        (Decode.field "padding" Spec.Element.Layout.Padding.decodePadding)
        (Decode.field "spacing" decodeSpacing)


decodeLength =
    Decode.map2
        Length
        (Decode.field "behavior" decodeBehavior)
        (Decode.field "minMax" decodeMinMax)


decodeMinMax =
    Decode.map2
        MinMax
        (Decode.field "min" Spec.Element.Layout.Length.decodeNullableLength)
        (Decode.field "max" Spec.Element.Layout.Length.decodeNullableLength)


decodePortion =
    Decode.int


decodeSize =
    Decode.map2
        Size
        (Decode.field "width" decodeLength)
        (Decode.field "height" decodeLength)


decodeSpacing =
    Decode.maybe Decode.int


encodeAbsoluteLength (AbsoluteLength a1) =
    encodeMaybeSpec_Element_Layout_Length_Length a1


encodeAlignment a =
    Encode.object
        [ ( "x", encodeMaybeAlignmentX a.x )
        , ( "y", encodeMaybeAlignmentY a.y )
        ]


encodeAlignmentX a =
    case a of
        Left ->
            Encode.string "Left"

        CenterX ->
            Encode.string "CenterX"

        Right ->
            Encode.string "Right"


encodeAlignmentY a =
    case a of
        Top ->
            Encode.string "Top"

        CenterY ->
            Encode.string "CenterY"

        Bottom ->
            Encode.string "Bottom"


encodeBehavior a =
    case a of
        Static a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Static" )
                , ( "A1", Spec.Element.Layout.Length.encodeLength a1 )
                ]

        Fill ->
            Encode.object
                [ ( "Constructor", Encode.string "Fill" )
                ]

        Shrink ->
            Encode.object
                [ ( "Constructor", Encode.string "Shrink" )
                ]


encodeConstraint a =
    case a of
        NoConstraint ->
            Encode.object
                [ ( "Constructor", Encode.string "NoConstraint" )
                ]

        Minimium a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Minimium" )
                , ( "A1", encodeMaybeSpec_Element_Layout_Length_Length a1 )
                ]

        Maximium a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Maximium" )
                , ( "A1", encodeMaybeSpec_Element_Layout_Length_Length a1 )
                ]


encodeFlow a =
    case a of
        Column ->
            Encode.string "Column"

        Row ->
            Encode.string "Row"

        WrappedRow ->
            Encode.string "WrappedRow"


encodeLayout a =
    Encode.object
        [ ( "layout", encodeFlow a.layout )
        , ( "alignment", encodeAlignment a.alignment )
        , ( "size", encodeSize a.size )
        , ( "padding", Spec.Element.Layout.Padding.encodePadding a.padding )
        , ( "spacing", encodeSpacing a.spacing )
        ]


encodeLength a =
    Encode.object
        [ ( "behavior", encodeBehavior a.behavior )
        , ( "minMax", encodeMinMax a.minMax )
        ]


encodeMaybeAlignmentX a =
    case a of
        Just b ->
            encodeAlignmentX b

        Nothing ->
            Encode.null


encodeMaybeAlignmentY a =
    case a of
        Just b ->
            encodeAlignmentY b

        Nothing ->
            Encode.null


encodeMaybeSpec_Element_Layout_Length_Length a =
    case a of
        Just b ->
            Spec.Element.Layout.Length.encodeLength b

        Nothing ->
            Encode.null


encodeMinMax a =
    Encode.object
        [ ( "min", Spec.Element.Layout.Length.encodeNullableLength a.min )
        , ( "max", Spec.Element.Layout.Length.encodeNullableLength a.max )
        ]


encodePortion a =
    Encode.int a


encodeSize a =
    Encode.object
        [ ( "width", encodeLength a.width )
        , ( "height", encodeLength a.height )
        ]


encodeSpacing a =
    case a of
        Just b ->
            Encode.int b

        Nothing ->
            Encode.null



-- [generator-end]
