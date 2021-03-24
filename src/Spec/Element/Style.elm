module Spec.Element.Style exposing (..)

{-| The styling layer for elements.
-}

import DesignSystem.Shadow
import DesignSystem.Typography
import Json.Decode as Decode
import Json.Decode.Extra as Extra
import Json.Encode as Encode
import Spec.Element.Model
import Spec.Element.Style.Edges
import Ui.ColorPicker.Advanced
import Ui.ColorPicker.Basic
import Ui.ColorPicker.Gradient



-- when the user has selected auto height/width but then added content
-- auto height/width is not compatible any more


imageModesCompatibleWithBackgroundOnly =
    [ Cover
    , Contain
    ]


imageModes =
    [ Cover
    , Contain
    , AutoWidth
    , AutoHeight
    ]


type alias ColorPickerState =
    Ui.ColorPicker.Basic.State


encodeColorPickerState _ =
    Encode.null


decodeColorPickerState color =
    Decode.succeed (Ui.ColorPicker.Basic.init color)



-- same as elm-ui
-- defaultTextInputStyles =
--     { background = Just (Ui.ColorPicker.Advanced.initAdvancedWithCustomColor Color.white)
--     , shadow = Nothing
--     , roundedBorders = Just (CornerDimensions (Just 3) Nothing Nothing Nothing)
--     , borderSettings = Just (BorderSettings (Spec.Element.Style.Edges.EdgeDimensions (Just 1) Nothing Nothing Nothing) (Ui.ColorPicker.Advanced.initAdvancedWithCustomColor Color.darkGrey))
--     , clip = False
--     , elementText = NoTypo
--     , placeholderText = NoTypo
--
--     -- which text to style: input value or placeholder?
--     , typoTab = DesignSystem.Typography.Input
--     }


{-|

    deriveCornerDimensions {topLeft = Just 5, topRight = Just 10, bottomRight = Nothing, bottomLeft = Nothing}
    --> {topLeft = 5, topRight = 10, bottomRight = 5, bottomLeft = 10}

-}
deriveCornerDimensions : CornerDimensions -> ConcreteCornerDimensions
deriveCornerDimensions dimensions =
    let
        { topLeft, topRight, bottomRight, bottomLeft } =
            dimensions

        topLeft_ =
            topLeft |> Maybe.withDefault 0

        topRight_ =
            topRight |> Maybe.withDefault topLeft_

        bottomLeft_ =
            bottomLeft |> Maybe.withDefault topRight_

        bottomRight_ =
            bottomRight |> Maybe.withDefault topLeft_
    in
    ConcreteCornerDimensions topLeft_ topRight_ bottomRight_ bottomLeft_


defaultBorderSettings =
    BorderSettings
        Spec.Element.Style.Edges.init
        Ui.ColorPicker.Advanced.initAdvanced


default : Spec.Element.Model.ElementKind -> Style
default elementType =
    { background = Nothing
    , roundedBorders = Nothing
    , borderSettings = Nothing
    , clip = False
    , elementText = NoTypo
    , placeholderText = NoTypo

    -- to not confuse the user, we set the default element tab to be the one that makes visual changes on the Canvas
    -- that is, if the user creates an input, the input is empty and only displays the placeholder
    -- the visible typography is the placeholder so we also select the tab that displays the placeholder
    , typoTab =
        case elementType of
            Spec.Element.Model.TextInput _ ->
                DesignSystem.Typography.Placeholder

            _ ->
                DesignSystem.Typography.Box
    , imageCropMode = Cover
    , videoStyle = defaultVideoStyle
    , shadow = NoShadow
    }


defaultVideoStyle : VideoStyle
defaultVideoStyle =
    VideoStyle False False False



-- [generator-start]


type alias Style =
    { background : Maybe Ui.ColorPicker.Gradient.BackgroundColor
    , shadow : ShadowSelection
    , roundedBorders : Maybe CornerDimensions
    , borderSettings : Maybe BorderSettings
    , clip : Bool
    , elementText : TypographySelection
    , placeholderText : TypographySelection

    -- content-specific styles
    , imageCropMode : ImageCropMode
    , videoStyle : VideoStyle

    -- state
    , typoTab : DesignSystem.Typography.TypoTab
    }


type ImageCropMode
    = Contain
    | Cover
    | AutoHeight
    | AutoWidth


type TypographySelection
    = TypoFromDesignSystem Int
    | CustomTypo DesignSystem.Typography.Typo
    | NoTypo


type ShadowSelection
    = ShadowFromDesignSystem Int
    | CustomShadow DesignSystem.Shadow.Shadow
    | NoShadow


type alias BorderSettings =
    { dimensions : Spec.Element.Style.Edges.EdgeDimensions
    , color : Ui.ColorPicker.Advanced.AdvancedState
    }


{-| Similar to the CSS shorthand for corner radius
-}
type alias CornerDimensions =
    { topLeft : Maybe Int, topRight : Maybe Int, bottomRight : Maybe Int, bottomLeft : Maybe Int }


{-| Similar to the CSS shorthand for corner radius
-}
type alias ConcreteCornerDimensions =
    { topLeft : Int, topRight : Int, bottomRight : Int, bottomLeft : Int }


type alias VideoStyle =
    { loop : Bool
    , controls : Bool
    , autoplay : Bool
    }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeBorderSettings =
    Decode.map2
        BorderSettings
        (Decode.field "dimensions" Spec.Element.Style.Edges.decodeEdgeDimensions)
        (Decode.field "color" Ui.ColorPicker.Advanced.decodeAdvancedState)


decodeConcreteCornerDimensions =
    Decode.map4
        ConcreteCornerDimensions
        (Decode.field "topLeft" Decode.int)
        (Decode.field "topRight" Decode.int)
        (Decode.field "bottomRight" Decode.int)
        (Decode.field "bottomLeft" Decode.int)


decodeCornerDimensions =
    Decode.map4
        CornerDimensions
        (Decode.field "topLeft" (Decode.maybe Decode.int))
        (Decode.field "topRight" (Decode.maybe Decode.int))
        (Decode.field "bottomRight" (Decode.maybe Decode.int))
        (Decode.field "bottomLeft" (Decode.maybe Decode.int))


decodeImageCropMode =
    let
        recover x =
            case x of
                "Contain" ->
                    Decode.succeed Contain

                "Cover" ->
                    Decode.succeed Cover

                "AutoHeight" ->
                    Decode.succeed AutoHeight

                "AutoWidth" ->
                    Decode.succeed AutoWidth

                other ->
                    Decode.fail <| "Unknown constructor for type ImageCropMode: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeShadowSelection =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeShadowSelectionHelp


decodeShadowSelectionHelp constructor =
    case constructor of
        "ShadowFromDesignSystem" ->
            Decode.map
                ShadowFromDesignSystem
                (Decode.field "A1" Decode.int)

        "CustomShadow" ->
            Decode.map
                CustomShadow
                (Decode.field "A1" DesignSystem.Shadow.decodeShadow)

        "NoShadow" ->
            Decode.succeed NoShadow

        other ->
            Decode.fail <| "Unknown constructor for type ShadowSelection: " ++ other


decodeStyle =
    Decode.succeed
        Style
        |> Extra.andMap (Decode.field "background" (Decode.maybe Ui.ColorPicker.Gradient.decodeBackgroundColor))
        |> Extra.andMap (Decode.field "shadow" decodeShadowSelection)
        |> Extra.andMap (Decode.field "roundedBorders" (Decode.maybe decodeCornerDimensions))
        |> Extra.andMap (Decode.field "borderSettings" (Decode.maybe decodeBorderSettings))
        |> Extra.andMap (Decode.field "clip" Decode.bool)
        |> Extra.andMap (Decode.field "elementText" decodeTypographySelection)
        |> Extra.andMap (Decode.field "placeholderText" decodeTypographySelection)
        |> Extra.andMap (Decode.field "imageCropMode" decodeImageCropMode)
        |> Extra.andMap (Decode.field "videoStyle" decodeVideoStyle)
        |> Extra.andMap (Decode.field "typoTab" DesignSystem.Typography.decodeTypoTab)


decodeTypographySelection =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeTypographySelectionHelp


decodeTypographySelectionHelp constructor =
    case constructor of
        "TypoFromDesignSystem" ->
            Decode.map
                TypoFromDesignSystem
                (Decode.field "A1" Decode.int)

        "CustomTypo" ->
            Decode.map
                CustomTypo
                (Decode.field "A1" DesignSystem.Typography.decodeTypo)

        "NoTypo" ->
            Decode.succeed NoTypo

        other ->
            Decode.fail <| "Unknown constructor for type TypographySelection: " ++ other


decodeVideoStyle =
    Decode.map3
        VideoStyle
        (Decode.field "loop" Decode.bool)
        (Decode.field "controls" Decode.bool)
        (Decode.field "autoplay" Decode.bool)


encodeBorderSettings a =
    Encode.object
        [ ( "dimensions", Spec.Element.Style.Edges.encodeEdgeDimensions a.dimensions )
        , ( "color", Ui.ColorPicker.Advanced.encodeAdvancedState a.color )
        ]


encodeConcreteCornerDimensions a =
    Encode.object
        [ ( "topLeft", Encode.int a.topLeft )
        , ( "topRight", Encode.int a.topRight )
        , ( "bottomRight", Encode.int a.bottomRight )
        , ( "bottomLeft", Encode.int a.bottomLeft )
        ]


encodeCornerDimensions a =
    Encode.object
        [ ( "topLeft", encodeMaybeInt a.topLeft )
        , ( "topRight", encodeMaybeInt a.topRight )
        , ( "bottomRight", encodeMaybeInt a.bottomRight )
        , ( "bottomLeft", encodeMaybeInt a.bottomLeft )
        ]


encodeImageCropMode a =
    case a of
        Contain ->
            Encode.string "Contain"

        Cover ->
            Encode.string "Cover"

        AutoHeight ->
            Encode.string "AutoHeight"

        AutoWidth ->
            Encode.string "AutoWidth"


encodeMaybeBorderSettings a =
    case a of
        Just b ->
            encodeBorderSettings b

        Nothing ->
            Encode.null


encodeMaybeCornerDimensions a =
    case a of
        Just b ->
            encodeCornerDimensions b

        Nothing ->
            Encode.null


encodeMaybeInt a =
    case a of
        Just b ->
            Encode.int b

        Nothing ->
            Encode.null


encodeMaybeUi_ColorPicker_Gradient_BackgroundColor a =
    case a of
        Just b ->
            Ui.ColorPicker.Gradient.encodeBackgroundColor b

        Nothing ->
            Encode.null


encodeShadowSelection a =
    case a of
        ShadowFromDesignSystem a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "ShadowFromDesignSystem" )
                , ( "A1", Encode.int a1 )
                ]

        CustomShadow a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "CustomShadow" )
                , ( "A1", DesignSystem.Shadow.encodeShadow a1 )
                ]

        NoShadow ->
            Encode.object
                [ ( "Constructor", Encode.string "NoShadow" )
                ]


encodeStyle a =
    Encode.object
        [ ( "background", encodeMaybeUi_ColorPicker_Gradient_BackgroundColor a.background )
        , ( "shadow", encodeShadowSelection a.shadow )
        , ( "roundedBorders", encodeMaybeCornerDimensions a.roundedBorders )
        , ( "borderSettings", encodeMaybeBorderSettings a.borderSettings )
        , ( "clip", Encode.bool a.clip )
        , ( "elementText", encodeTypographySelection a.elementText )
        , ( "placeholderText", encodeTypographySelection a.placeholderText )
        , ( "imageCropMode", encodeImageCropMode a.imageCropMode )
        , ( "videoStyle", encodeVideoStyle a.videoStyle )
        , ( "typoTab", DesignSystem.Typography.encodeTypoTab a.typoTab )
        ]


encodeTypographySelection a =
    case a of
        TypoFromDesignSystem a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "TypoFromDesignSystem" )
                , ( "A1", Encode.int a1 )
                ]

        CustomTypo a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "CustomTypo" )
                , ( "A1", DesignSystem.Typography.encodeTypo a1 )
                ]

        NoTypo ->
            Encode.object
                [ ( "Constructor", Encode.string "NoTypo" )
                ]


encodeVideoStyle a =
    Encode.object
        [ ( "loop", Encode.bool a.loop )
        , ( "controls", Encode.bool a.controls )
        , ( "autoplay", Encode.bool a.autoplay )
        ]



-- [generator-end]
