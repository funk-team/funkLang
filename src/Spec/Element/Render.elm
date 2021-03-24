module Spec.Element.Render exposing (effectivelySelectedOption, getLineHeight, renderBackground, renderImageCrop, spacing, style)

import Color.Extra
import DesignSystem
import DesignSystem.Color.Model
import DesignSystem.Color.Selection
import DesignSystem.Shadow
import DesignSystem.Typography
import Dict
import Element
import Element.Background
import Element.Border
import Html
import Html.Attributes
import Interface.Data
import Interface.Model
import Model.Model
import Renderer.Help
import Renderer.InteractivityAttribs exposing (sheetEffects)
import Renderer.StyleCompositor exposing (noStyle)
import Spec
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Spec.Element.Style
import Spec.Element.Style.Edges
import Ui.ColorPicker.Gradient
import Ui.Style


bodyTypoId =
    3


getScreenTypo designSystem =
    Dict.get bodyTypoId designSystem.typoEditor.typos


{-| Render the styles for a given element
-}
style :
    Bool
    -> Interface.Model.ScopeData
    -> Model.Model.UserModel
    -> Spec.Element.Model.EitherElement
    -> Maybe (Renderer.StyleCompositor.RenderedStyles msg)
style isOnCanvas dataScope ({ elementStyles, designSystem } as userModel) element =
    let
        baseStyles =
            case element.outerGeometry of
                -- render only background when element is screen
                Spec.Element.Model.ScreenGeometry _ ->
                    let
                        sheetEffectsForContext =
                            if isOnCanvas then
                                sheetEffects

                            else
                                noStyle
                    in
                    { sheetEffectsForContext
                        | typoStyles =
                            getScreenTypo designSystem
                                |> Maybe.map (DesignSystem.Typography.typoToAttributes designSystem.colorEditor)
                                |> Maybe.withDefault []
                    }
                        |> Just

                _ ->
                    Nothing
    in
    case Spec.Element.Id.getFromDict element.shared.id elementStyles of
        Nothing ->
            let
                supportsObjectFit =
                    Renderer.Help.getContent
                        element.shared.id
                        userModel
                        dataScope
                        |> Maybe.map .value
                        |> Interface.Data.supportsObjectFit

                -- always render image crop when image is connected
            in
            case supportsObjectFit of
                False ->
                    baseStyles

                True ->
                    let
                        baseStyles_ =
                            baseStyles
                                |> Maybe.withDefault noStyle
                    in
                    Just
                        { baseStyles_
                            | imageCrop =
                                renderOverrides
                                    element
                                    designSystem
                                    (Spec.Element.Style.default Spec.Element.Model.Box)
                                    |> .imageCrop
                        }

        Just styles ->
            case element.outerGeometry of
                Spec.Element.Model.ScreenGeometry _ ->
                    let
                        customScreenStyles =
                            renderOverrides element designSystem styles

                        baseStyles_ =
                            baseStyles
                                |> Maybe.withDefault noStyle
                    in
                    { baseStyles_
                        | typoStyles =
                            case customScreenStyles.typoStyles of
                                [] ->
                                    baseStyles_.typoStyles

                                someCustomStyles ->
                                    someCustomStyles
                        , background =
                            case customScreenStyles.background of
                                [] ->
                                    designSystem.colorEditor.background.value
                                        |> Color.Extra.toElmUi
                                        |> Element.Background.color
                                        |> List.singleton

                                custom ->
                                    custom
                    }
                        |> Just

                _ ->
                    renderOverrides element designSystem styles
                        |> Just


renderBackground : Maybe Ui.ColorPicker.Gradient.BackgroundColor -> DesignSystem.Color.Model.Model -> List (Element.Attribute msg)
renderBackground background colorEditor =
    case background of
        Just (Ui.ColorPicker.Gradient.SolidBackground { selection }) ->
            case DesignSystem.Color.Selection.getSelectedColor colorEditor selection of
                Nothing ->
                    []

                Just color ->
                    color
                        |> Color.Extra.toElmUi
                        |> Element.Background.color
                        |> List.singleton

        Just (Ui.ColorPicker.Gradient.GradientBackground gradient) ->
            Ui.ColorPicker.Gradient.gradientToAttributes
                gradient.gradient
                colorEditor

        Nothing ->
            []


effectivelySelectedOption element styles =
    case
        ( Spec.Element.hasChildren element
        , List.any ((==) styles.imageCropMode) Spec.Element.Style.imageModesCompatibleWithBackgroundOnly
        )
    of
        ( True, False ) ->
            Spec.Element.Style.Cover

        _ ->
            styles.imageCropMode


{-| Render object fit cover or contain atomic CSS classes.
-}
renderImageCrop : Spec.Element.Model.Element a -> Spec.Element.Style.Style -> Maybe (Html.Attribute msg)
renderImageCrop element styles =
    case effectivelySelectedOption element styles of
        Spec.Element.Style.Cover ->
            Just <| Html.Attributes.class "object-fit-cover"

        Spec.Element.Style.Contain ->
            Just <| Html.Attributes.class "object-fit-contain"

        Spec.Element.Style.AutoHeight ->
            Nothing

        Spec.Element.Style.AutoWidth ->
            Nothing


getLineHeight : Spec.Element.Model.EitherElement -> Model.Model.UserModel -> Maybe Int
getLineHeight element userModel =
    let
        screenFallback =
            case element.outerGeometry of
                Spec.Element.Model.ScreenGeometry _ ->
                    case getScreenTypo userModel.designSystem of
                        Just { lineHeight } ->
                            lineHeight
                                |> Maybe.withDefault DesignSystem.Typography.defaultLineHeight
                                |> Just

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
    in
    case Spec.Element.Id.getFromDict element.shared.id userModel.elementStyles of
        Nothing ->
            screenFallback

        Just { elementText } ->
            case elementText of
                Spec.Element.Style.NoTypo ->
                    screenFallback

                Spec.Element.Style.TypoFromDesignSystem id ->
                    case Dict.get id userModel.designSystem.typoEditor.typos of
                        Just typo ->
                            typo.lineHeight
                                |> Maybe.withDefault DesignSystem.Typography.defaultLineHeight
                                |> Just

                        Nothing ->
                            Nothing

                Spec.Element.Style.CustomTypo customTypo ->
                    customTypo.lineHeight
                        |> Maybe.withDefault DesignSystem.Typography.defaultLineHeight
                        |> Just


renderOverrides :
    Spec.Element.Model.Element a
    -> DesignSystem.Model
    -> Spec.Element.Style.Style
    -> Renderer.StyleCompositor.RenderedStyles msg
renderOverrides element designSystem thisStyle =
    let
        -- DECORATION
        background =
            renderBackground thisStyle.background designSystem.colorEditor

        -- DECORATION
        typoStyle =
            case thisStyle.elementText of
                Spec.Element.Style.TypoFromDesignSystem id ->
                    case Dict.get id designSystem.typoEditor.typos of
                        Just typo ->
                            DesignSystem.Typography.typoToAttributes designSystem.colorEditor typo

                        Nothing ->
                            []

                Spec.Element.Style.CustomTypo customTypo ->
                    DesignSystem.Typography.typoToAttributes designSystem.colorEditor customTypo

                Spec.Element.Style.NoTypo ->
                    []

        borderRadius =
            case thisStyle.roundedBorders of
                Nothing ->
                    []

                Just roundedBorders ->
                    roundedBorders
                        |> Spec.Element.Style.deriveCornerDimensions
                        |> Element.Border.roundEach
                        |> List.singleton

        renderedBorder =
            case thisStyle.borderSettings of
                Just b ->
                    border designSystem b

                Nothing ->
                    []

        shadows : List (Element.Attribute msg)
        shadows =
            case thisStyle.shadow of
                Spec.Element.Style.ShadowFromDesignSystem id ->
                    case Dict.get id designSystem.shadows.shadows of
                        Just shadow ->
                            DesignSystem.Shadow.shadowToAttr designSystem.colorEditor shadow

                        Nothing ->
                            []

                Spec.Element.Style.CustomShadow shadow ->
                    DesignSystem.Shadow.shadowToAttr designSystem.colorEditor shadow

                Spec.Element.Style.NoShadow ->
                    []

        clip =
            if thisStyle.clip then
                [ Element.clip ]

            else
                []

        decoration : List (Element.Attribute msg)
        decoration =
            borderRadius
                ++ renderedBorder
                -- shadows are added here because of lazyness of rendering the shadow spec to string instead of elm-ui
                ++ shadows
                ++ clip
    in
    { other = decoration
    , typoStyles = typoStyle
    , background = background
    , imageCrop =
        renderImageCrop element thisStyle
            |> Maybe.map Element.htmlAttribute
    , shadows = []
    }


border : DesignSystem.Model -> Spec.Element.Style.BorderSettings -> List (Element.Attribute msg)
border designSystem { dimensions, color } =
    let
        colorAttrib =
            case DesignSystem.Color.Selection.getSelectedColor designSystem.colorEditor color.selection of
                Nothing ->
                    []

                Just color_ ->
                    color_
                        |> Color.Extra.toElmUi
                        |> Element.Border.color
                        |> List.singleton

        widths =
            Element.Border.widthEach
                (Spec.Element.Style.Edges.deriveEdgeDimensions dimensions)
    in
    widths :: colorAttrib



-- if the element has text spacing, the text spacing takes precedence


spacing : Model.Model.UserModel -> Spec.Element.Model.EitherElement -> Maybe Int -> List (Element.Attribute msg)
spacing userModel element spacing_ =
    let
        lineHeight =
            case getLineHeight element userModel of
                Nothing ->
                    []

                Just px ->
                    [ Ui.Style.style "line-height" ("calc(1em + " ++ String.fromInt px ++ "px)")
                    , Ui.Style.class "custom-line-height"
                    ]

        renderedSpacing =
            case ( spacing_, Spec.hasTextConnection element userModel ) of
                ( Just spacing__, False ) ->
                    [ Element.spacing spacing__ ]

                _ ->
                    []
    in
    lineHeight ++ renderedSpacing
