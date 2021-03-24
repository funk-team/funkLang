module Ui.ColorPicker.Gradient exposing
    ( BackgroundColor(..)
    , decodeBackgroundColor
    , encodeBackgroundColor
    , gradientToAttributes
    , initGradientState
    , initLinearGradientState
    , initRadialGradientState
    , removeColorPoint
    , view
    )

{-| Provide functions for rendering color pickers
-}

import Color
import Color.Convert
import Color.Extra
import DesignSystem.Color.Model
import DesignSystem.Color.Selection
import Dict
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Events.Extra
import Element.Font
import Element.Input
import Html.Attributes
import IntDict
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Ui.Boxicons
import Ui.ColorPicker.Advanced exposing (AdvancedState, SwatchUpdate, decodeAdvancedState, encodeAdvancedState)
import Ui.ColorPicker.Basic
import Ui.ColorPicker.Help
import Ui.Component
import Ui.Dropdown
import Ui.Mouse
import Ui.Style


type alias GradientState =
    { gradient : Gradient
    , pickerState : Ui.ColorPicker.Basic.State
    , visibility : Ui.ColorPicker.Advanced.Visibility
    , isCloseEnough : Bool
    , isGrabbing : Bool
    }


type alias Return =
    { designSystemUpdate : Maybe SwatchUpdate
    , state : BackgroundColor
    }


type MouseEvent
    = OnMouseDownSlider Ui.Mouse.MouseInfo
    | OnMouseMoveSlider Ui.Mouse.MouseInfo
    | OnMouseDownColorPreview ( Positioning, DesignSystem.Color.Selection.Selection ) Ui.Mouse.MouseInfo
    | OnMouseMoveColorPreview ( Positioning, DesignSystem.Color.Selection.Selection ) Ui.Mouse.MouseInfo
    | OnMouseUp



-- [generator-start]


type BackgroundColor
    = SolidBackground AdvancedState
    | GradientBackground GradientState


type alias Gradient =
    { label : String
    , colorBase : DesignSystem.Color.Selection.Selection
    , colorBasePositioning : Positioning
    , colorPoints : List ( Positioning, DesignSystem.Color.Selection.Selection )
    , gradientType : Type
    }


type Type
    = Linear Angle
    | Radial



-- between 0 and 100


type Positioning
    = Positioning Int



-- angle in deg


type Angle
    = Angle Int
    | InvAngle String


sliderWidth =
    205


sliderHeight =
    25


borderWidth =
    3


markersHeight =
    50


markersWidth =
    3


colorPreviewSize =
    25


initGradientState : GradientState
initGradientState =
    let
        default =
            defaultGradient

        pickerState =
            Color.rgb255 131 58 180
                |> defaultSwatch
                |> .pickerState

        --     |> DesignSystem.Color.Selection.Standalone
        -- default.colorBase
        --     |> DesignSystem.Color.Selection.getSelectedSwatch model
        --     |> Maybe.withDefault (defaultSwatch (Color.rgb255 131 58 180))
        --     |> .pickerState
    in
    GradientState
        default
        pickerState
        Ui.ColorPicker.Advanced.Closed
        False
        False


initLinearGradientState =
    initGradientState


initRadialGradientState =
    let
        gradient =
            initGradientState.gradient
    in
    { initGradientState
        | gradient =
            { gradient | gradientType = Radial }
    }


defaultGradient : Gradient
defaultGradient =
    { label = "Gradient"
    , colorBase =
        Color.rgb255 131 58 180
            |> DesignSystem.Color.Selection.Standalone
    , colorBasePositioning = Positioning 0
    , colorPoints =
        [ ( Positioning 53
          , Color.rgb255 253 29 29
                |> DesignSystem.Color.Selection.Standalone
          )
        , ( Positioning 100
          , Color.rgb255 252 176 69
                |> DesignSystem.Color.Selection.Standalone
          )
        ]
    , gradientType = Linear (Angle 115)
    }


defaultSwatch base =
    { label = "not interesting"
    , value = base
    , pickerState = Ui.ColorPicker.Basic.init base
    }


defaultAngleValue =
    45


gradientToAttributes : Gradient -> DesignSystem.Color.Model.Model -> List (Element.Attribute msg)
gradientToAttributes gradient model =
    let
        listColorPointStr =
            completeListColorPoints gradient
                |> List.map
                    (Tuple.mapSecond (DesignSystem.Color.Selection.getSelectedColor model))
                |> List.filterMap
                    (\( a, maybeB ) ->
                        case maybeB of
                            Just b ->
                                Just ( a, b )

                            Nothing ->
                                Nothing
                    )
                |> List.sortBy (\( Positioning p, _ ) -> p)
                |> List.map colorPointToStr
                |> List.append [ angle ]
                |> List.intersperse ", "
                |> String.concat

        colorPointToStr ( Positioning posVal, color ) =
            let
                intToPecentStr val =
                    (String.fromInt <| clamp 0 100 val) ++ "%"
            in
            Color.Convert.colorToCssRgba color
                ++ " "
                ++ intToPecentStr posVal

        angle =
            case gradient.gradientType of
                Linear (Angle a) ->
                    String.fromInt a
                        ++ "deg"

                Linear (InvAngle _) ->
                    String.fromInt defaultAngleValue
                        ++ "deg"

                Radial ->
                    "circle"

        radialOrLinear =
            case gradient.gradientType of
                Linear _ ->
                    "linear"

                Radial ->
                    "radial"

        defaultPlainColor =
            case DesignSystem.Color.Selection.getSelectedColor model gradient.colorBase of
                Nothing ->
                    []

                Just colorBase ->
                    [ Html.Attributes.style
                        "background"
                        (Color.Convert.colorToCssRgba colorBase)
                    ]
    in
    [ Html.Attributes.style
        "background"
        ("-moz-" ++ radialOrLinear ++ "-gradient(" ++ listColorPointStr ++ ")")
    , Html.Attributes.style
        "background"
        ("-webkit-" ++ radialOrLinear ++ "-gradient(" ++ listColorPointStr ++ ")")
    , Html.Attributes.style
        "background"
        (radialOrLinear ++ "-gradient(" ++ listColorPointStr ++ ")")
    ]
        |> List.append defaultPlainColor
        |> List.map Element.htmlAttribute


removeColorPoint gradientState =
    let
        gradient =
            gradientState.gradient
    in
    case gradient.colorPoints of
        ( headPos, headColor ) :: one :: queue ->
            { gradientState
                | gradient =
                    { gradient
                        | colorBase = headColor
                        , colorBasePositioning = headPos
                        , colorPoints = one :: queue
                    }
            }

        _ ->
            gradientState


view : BackgroundColor -> DesignSystem.Color.Model.Model -> Element.Element Return
view backgroundColor designSystemColors =
    let
        { selection, pickerState, visibility } =
            state

        state =
            case backgroundColor of
                SolidBackground advancedState ->
                    advancedState

                GradientBackground gradientState ->
                    { selection = gradientState.gradient.colorBase
                    , pickerState = gradientState.pickerState
                    , visibility = gradientState.visibility
                    }

        retrievedColor =
            case selection of
                DesignSystem.Color.Selection.FromSystem colorSelection ->
                    DesignSystem.Color.Model.getSelectedColor colorSelection designSystemColors
                        |> Maybe.withDefault (Color.rgba 0 0 0 0)

                DesignSystem.Color.Selection.Standalone color_ ->
                    color_

        gradientEditor =
            case backgroundColor of
                GradientBackground gradientState ->
                    Element.column []
                        [ slider gradientState designSystemColors
                        , Element.el
                            [ Element.width Element.fill
                            , Element.paddingXY 5 3
                            ]
                            (angleInput gradientState.gradient.gradientType
                                |> Element.map (updateType backgroundColor)
                            )
                        ]

                _ ->
                    Element.none

        {- The contents of the popout -}
        pickerAndSwatches =
            Element.onLeft <|
                case visibility of
                    -- TODO: apply transformations here also
                    Ui.ColorPicker.Advanced.Open position ->
                        Element.column
                            ([ Element.Background.color Ui.Style.white
                             , Element.Border.rounded 2
                             , Ui.Style.shadowMedium
                             , Element.moveLeft 2
                             , Element.moveUp 1
                             , Element.Border.width 1
                             , Element.Border.color Ui.Style.grey
                             , Element.spacing 5
                             , Element.Events.Extra.onMousedownoutside (returnvisbilityUpdate Ui.ColorPicker.Advanced.Closed)
                             ]
                                ++ Ui.ColorPicker.Advanced.renderPosition position
                            )
                            [ renderedPicker backgroundColor selection ( retrievedColor, pickerState )
                            , dividingLine
                            , backgroundColorTypeDropDown backgroundColor designSystemColors state
                                |> Element.map (Return Nothing)
                            , gradientEditor
                            , dividingLine
                            , swatches backgroundColor designSystemColors selection retrievedColor
                            ]

                    Ui.ColorPicker.Advanced.Closed ->
                        Element.none

        dividingLine =
            Element.el
                [ Element.Border.widthEach { edges | top = 1 }
                , Element.Border.color Ui.Style.grey
                , Element.width Element.fill
                ]
                Element.none

        returnvisbilityUpdate newVisbility =
            case backgroundColor of
                SolidBackground advancedState ->
                    { advancedState | visibility = newVisbility }
                        |> SolidBackground
                        |> Return Nothing

                GradientBackground gradientState ->
                    { gradientState | visibility = newVisbility }
                        |> GradientBackground
                        |> Return Nothing

        {- The little area that previews the color and shows the popout on click -}
        thumbnail =
            Element.el
                (pickerAndSwatches :: thumbnailAttribs)
            <|
                Element.el
                    [ Ui.ColorPicker.Advanced.onMouseDownOpenOrClose visibility
                        |> Element.mapAttribute returnvisbilityUpdate
                    , Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    Element.none

        thumbnailAttribs : List (Element.Attribute Return)
        thumbnailAttribs =
            Ui.ColorPicker.Help.thumbnailAttribs
                ++ (case backgroundColor of
                        GradientBackground { gradient } ->
                            gradientToAttributes gradient designSystemColors

                        SolidBackground _ ->
                            Element.Background.color
                                (Color.Extra.toElmUi retrievedColor)
                                |> List.singleton
                   )
    in
    thumbnail


renderedPicker :
    BackgroundColor
    -> DesignSystem.Color.Selection.Selection
    -> ( Color.Color, Ui.ColorPicker.Basic.State )
    -> Element.Element Return
renderedPicker backgroundColor selection ( value, state ) =
    Ui.ColorPicker.Basic.view value state
        |> Element.map
            (\msg ->
                Ui.ColorPicker.Basic.update
                    msg
                    value
                    state
                    |> (\( newState, maybeColor ) -> ( Maybe.withDefault value maybeColor, newState ))
            )
        |> Element.map (processNewPickedColor backgroundColor selection)


{-| If the color is linked to the design system, update the design system color
-}
processNewPickedColor :
    BackgroundColor
    -> DesignSystem.Color.Selection.Selection
    -> ( Color.Color, Ui.ColorPicker.Basic.State )
    -> Return
processNewPickedColor backgroundColor selection ( newColor, newPickerState ) =
    let
        processNewPickedColorSolid advancedState =
            case selection of
                DesignSystem.Color.Selection.Standalone value ->
                    Return
                        Nothing
                        ({ advancedState
                            | selection = DesignSystem.Color.Selection.Standalone newColor
                            , pickerState = newPickerState
                         }
                            |> SolidBackground
                        )

                DesignSystem.Color.Selection.FromSystem key ->
                    Return
                        (Just <| SwatchUpdate key newColor)
                        ({ advancedState
                            | pickerState = newPickerState
                         }
                            |> SolidBackground
                        )

        processNewPickedColorGrad gradientState =
            case selection of
                DesignSystem.Color.Selection.Standalone value ->
                    let
                        gradient =
                            gradientState.gradient

                        newState =
                            { gradientState
                                | gradient =
                                    { gradient | colorBase = DesignSystem.Color.Selection.Standalone newColor }
                                , pickerState = newPickerState
                            }
                    in
                    Return Nothing (GradientBackground newState)

                DesignSystem.Color.Selection.FromSystem key ->
                    let
                        newState =
                            { gradientState
                                | pickerState = newPickerState
                            }
                    in
                    Return
                        (Just <| SwatchUpdate key newColor)
                        (GradientBackground newState)
    in
    case backgroundColor of
        SolidBackground advancedState ->
            processNewPickedColorSolid advancedState

        GradientBackground gradientState ->
            processNewPickedColorGrad gradientState


backgroundColorTypeDropDown :
    BackgroundColor
    -> DesignSystem.Color.Model.Model
    -> AdvancedState
    -> Element.Element BackgroundColor
backgroundColorTypeDropDown backgroundColor designSystemColors { selection, pickerState, visibility } =
    let
        -- label
        selectedBackgroundColorTypeStr =
            case backgroundColor of
                SolidBackground _ ->
                    "Solid"

                GradientBackground gradientState ->
                    if gradientState.gradient.gradientType == Radial then
                        "Radial"

                    else
                        "Linear"

        -- content
        dropdownRows =
            [ solidRow, linearRow, radialRow ]

        solidRow =
            Ui.Dropdown.viewRow
                { isSelected =
                    selectedBackgroundColorTypeStr == "Solid"
                , label = Ui.Dropdown.Description "Solid"
                , sideNote = Ui.Dropdown.NoDetail
                , onSelect =
                    { selection = selection
                    , pickerState = pickerState
                    , visibility = visibility
                    }
                        |> SolidBackground
                , detail = Ui.Dropdown.Description "Plain color"
                , rightHandText = Nothing
                }

        linearRow =
            let
                newGradientState =
                    case backgroundColor of
                        SolidBackground _ ->
                            setIsOpen initLinearGradientState

                        GradientBackground gradientState ->
                            let
                                gradient =
                                    gradientState.gradient
                            in
                            { gradientState
                                | gradient =
                                    { gradient | gradientType = Linear (Angle 115) }
                            }
            in
            Ui.Dropdown.viewRow
                { isSelected =
                    selectedBackgroundColorTypeStr == "Linear"
                , label = Ui.Dropdown.Description "Linear"
                , sideNote = Ui.Dropdown.NoDetail
                , onSelect = GradientBackground newGradientState
                , detail = Ui.Dropdown.Description "Linear gradient"
                , rightHandText = Nothing
                }

        radialRow =
            let
                newGradientState =
                    case backgroundColor of
                        SolidBackground _ ->
                            setIsOpen initRadialGradientState

                        GradientBackground gradientState ->
                            let
                                gradient =
                                    gradientState.gradient
                            in
                            { gradientState
                                | gradient =
                                    { gradient | gradientType = Radial }
                            }
            in
            Ui.Dropdown.viewRow
                { isSelected =
                    selectedBackgroundColorTypeStr == "Radial"
                , label = Ui.Dropdown.Description "Radial"
                , sideNote = Ui.Dropdown.NoDetail
                , onSelect = GradientBackground newGradientState
                , detail = Ui.Dropdown.Description "Circular gradient"
                , rightHandText = Nothing
                }

        setIsOpen gradientState =
            { gradientState | visibility = visibility }
    in
    Ui.Dropdown.view
        [ Element.htmlAttribute (Html.Attributes.style "margin-left" "-6px") ]
        { contents = dropdownRows
        , label = selectedBackgroundColorTypeStr
        }


slider :
    GradientState
    -> DesignSystem.Color.Model.Model
    -> Element.Element Return
slider gradientState designSystemColors =
    let
        gradient =
            gradientState.gradient

        isColorBase : ( Positioning, DesignSystem.Color.Selection.Selection ) -> Bool
        isColorBase ( pos, col ) =
            (pos == gradient.colorBasePositioning)
                && (col == gradient.colorBase)

        markers =
            completeListColorPoints gradient
                |> List.map marker

        marker : ( Positioning, DesignSystem.Color.Selection.Selection ) -> Element.Attribute Return
        marker ( Positioning pos, selection_ ) =
            Element.el
                [ Element.Border.widthEach { edges | left = markersWidth }
                , Element.height (Element.px markersHeight)
                , Element.moveUp (((markersHeight - sliderHeight) / 2) - borderWidth)
                , Element.moveRight ((sliderWidth - markersWidth) * (toFloat pos / 100) + borderWidth)
                , Ui.Mouse.pointCursor
                , Element.Events.onClick
                    (Return Nothing
                        ({ gradientState | gradient = setColorBase ( Positioning pos, selection_ ) gradient }
                            |> GradientBackground
                        )
                    )
                ]
                Element.none
                |> Element.behindContent

        colorsPreviews =
            let
                tupleMapFirst fun ( a, b, c ) =
                    ( fun a, b, c )

                tupleListMapFirst =
                    List.map << tupleMapFirst

                superposed tupA tupB =
                    let
                        ( a, b ) =
                            ( Tuple.first tupA, Tuple.first tupB )
                    in
                    abs (b - a) > colorPreviewSize

                toPxPos pos =
                    let
                        unpPos =
                            unpackPositioning pos
                    in
                    (toFloat unpPos * sliderWidth / 100)
                        - (colorPreviewSize / 2)
                        |> round
                        |> clamp 0 (sliderWidth - colorPreviewSize)

                colorBaseColorPreview =
                    ( gradient.colorBasePositioning, gradient.colorBase )
                        |> (\( p, c ) -> ( p, p, c ))
                        |> tupleMapFirst toPxPos

                otherColorPreviews =
                    gradient.colorPoints
                        |> List.map (\( p, c ) -> ( p, p, c ))
                        |> tupleListMapFirst toPxPos
                        |> List.sortBy (\( first, _, _ ) -> first)
            in
            (colorBaseColorPreview :: otherColorPreviews)
                |> List.reverse
                |> List.map colorPreview

        colorPreview ( pxPos, pos, selection_ ) =
            let
                selectedBorderWidth =
                    1

                whiteBorderWidth =
                    1

                color =
                    DesignSystem.Color.Selection.getSelectedColor designSystemColors selection_
                        |> Maybe.withDefault (Color.rgb 200 200 200)
            in
            Element.el
                [ Element.width (Element.px (colorPreviewSize - 2 * selectedBorderWidth))
                , Element.height (Element.px (colorPreviewSize - 2 * selectedBorderWidth))
                , Element.Border.color (Element.rgb 1 1 1)
                , Element.Border.width whiteBorderWidth
                , Element.Background.color (Color.Extra.toElmUi color)
                ]
                Element.none
                |> Element.el
                    ([ Element.Border.width selectedBorderWidth
                     , Element.Border.color
                        (if isColorBase ( pos, selection_ ) then
                            Ui.Style.highlightColorSolid

                         else
                            Ui.Style.transparent
                        )
                     , Element.moveUp (((markersHeight - sliderHeight) / 2) - borderWidth + colorPreviewSize + 3)
                     , Element.moveRight (toFloat pxPos + borderWidth)
                     , if gradientState.isGrabbing then
                        Ui.Mouse.grabbingCursor

                       else
                        Ui.Mouse.pointCursor

                     -- , Element.Events.onClick
                     --    (Return Nothing
                     --        ({ gradientState | gradient = setColorBase ( pos, selection_ ) gradient }
                     --            |> GradientBackground
                     --        )
                     --    )
                     ]
                        ++ Ui.Style.paperShadow
                        ++ Ui.Mouse.positionAttrs
                            { onMouseDown = Just (OnMouseDownColorPreview ( pos, selection_ ))
                            , onMouseUp = Just OnMouseUp
                            , onClick = Nothing
                            , onMouseMove = Just (OnMouseMoveColorPreview ( pos, selection_ ))
                            }
                    )
                |> Element.map (\mouseEvent -> update mouseEvent gradientState designSystemColors)
                |> Element.inFront
    in
    Element.el
        ([ Element.width (Element.px (sliderWidth + 2 * borderWidth))
         , Element.height (Element.px (sliderHeight + 2 * borderWidth))
         , Element.Border.width borderWidth
         , Element.Border.color (Element.rgb 1 1 1)
         , Element.centerX
         , if gradientState.isCloseEnough then
            if gradientState.isGrabbing then
                Ui.Mouse.grabbingCursor

            else
                Ui.Mouse.pointCursor

           else
            Ui.Mouse.addElementCursor
         ]
            ++ gradientToAttributesForGradientPicker gradient designSystemColors
            ++ Ui.Mouse.positionAttrs
                { onMouseDown = Just OnMouseDownSlider
                , onMouseUp = Just OnMouseUp
                , onClick = Nothing
                , onMouseMove = Just OnMouseMoveSlider
                }
        )
        Element.none
        |> Element.map (\mouseEvent -> update mouseEvent gradientState designSystemColors)
        |> Element.el
            ([ Element.height Element.shrink
             , Element.width Element.shrink
             ]
                ++ markers
                ++ colorsPreviews
            )
        |> Element.el
            [ Element.paddingEach
                { edges
                    | top = round (((markersHeight - sliderHeight) / 2) + colorPreviewSize)
                    , bottom = round ((markersHeight - sliderHeight) / 2)
                }
            ]


gradientToAttributesForGradientPicker : Gradient -> DesignSystem.Color.Model.Model -> List (Element.Attribute msg)
gradientToAttributesForGradientPicker gradient model =
    gradientToAttributes
        { gradient
            | gradientType = Linear (Angle 90)
        }
        model


swatches :
    BackgroundColor
    -> DesignSystem.Color.Model.Model
    -> DesignSystem.Color.Selection.Selection
    -> Color.Color
    -> Element.Element Return
swatches backgroundColor designSystemColors selection retrievedColor =
    let
        allSwatches =
            [ ( DesignSystem.Color.Model.TextSwatchSelected, designSystemColors.text )
            , ( DesignSystem.Color.Model.BackgroundSwatchSelected, designSystemColors.background )
            ]
                ++ otherSwatches
                |> List.map
                    (viewSwatch
                        (case selection of
                            DesignSystem.Color.Selection.FromSystem sel ->
                                Just sel

                            _ ->
                                Nothing
                        )
                    )

        otherSwatches =
            Dict.toList designSystemColors.others
                |> List.map (Tuple.mapFirst DesignSystem.Color.Model.OtherSwatchSelected)

        swatchRows =
            Element.wrappedRow
                [ Element.spacing 10 ]
                allSwatches
                |> Element.map
                    (updateSelection << DesignSystem.Color.Selection.FromSystem)

        swatchHeader =
            Element.row [ Element.spacing 5, Element.height (Element.px 10) ]
                [ Element.text "Design System"
                , unlink
                ]

        unlink =
            case selection of
                DesignSystem.Color.Selection.FromSystem _ ->
                    let
                        newSelection =
                            DesignSystem.Color.Selection.Standalone retrievedColor
                    in
                    Element.el
                        [ Element.Events.onClick (Return Nothing (updateSelection newSelection))
                        , Element.Font.color Ui.Style.highlightColorSolid
                        , Element.pointer
                        , Element.mouseOver [ Element.Background.color Ui.Style.slightAccent ]
                        , Element.htmlAttribute (Html.Attributes.title "Unlink from design system")
                        , Element.Border.rounded 2
                        ]
                        (Element.el [ Element.scale 0.75 ] <| Ui.Component.icon Ui.Boxicons.bxUnlink)

                _ ->
                    let
                        id =
                            IntDict.nextId designSystemColors.others

                        newSelection =
                            DesignSystem.Color.Selection.FromSystem (DesignSystem.Color.Model.OtherSwatchSelected id)

                        designSystemUpdate : Maybe SwatchUpdate
                        designSystemUpdate =
                            Just (SwatchUpdate (DesignSystem.Color.Model.OtherSwatchSelected id) retrievedColor)
                    in
                    Element.el
                        [ Element.Events.onClick
                            (Return designSystemUpdate (updateSelection newSelection))
                        , Element.Font.color Ui.Style.highlightColorSolid
                        , Element.pointer
                        , Element.mouseOver [ Element.Background.color Ui.Style.slightAccent ]
                        , Element.htmlAttribute (Html.Attributes.title "Add to design system")
                        , Element.Border.rounded 2
                        ]
                        (Element.el [ Element.scale 0.75 ] <| Ui.Component.icon Ui.Boxicons.bxPlus)

        updateSelection newSelection =
            case backgroundColor of
                SolidBackground state_ ->
                    { state_ | selection = newSelection }
                        |> SolidBackground

                GradientBackground gradientState ->
                    let
                        gradient =
                            gradientState.gradient
                    in
                    { gradientState
                        | gradient = { gradient | colorBase = newSelection }
                    }
                        |> GradientBackground
    in
    Element.column [ Element.padding 10, Element.spacing 10 ]
        [ swatchHeader
        , swatchRows
            |> Element.map (Return Nothing)
        ]


viewSwatch :
    Maybe DesignSystem.Color.Model.Selection
    -> ( DesignSystem.Color.Model.Selection, DesignSystem.Color.Model.Swatch )
    -> Element.Element DesignSystem.Color.Model.Selection
viewSwatch selection ( colorReference, swatch ) =
    let
        borderColor =
            if selection == Just colorReference then
                Ui.Style.highlightColorSolid

            else
                Ui.Style.grey

        inner =
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Border.width 1
                , Element.Border.color Ui.Style.white
                , Element.Background.color (Color.Extra.toElmUi swatch.value)
                ]
                Element.none
    in
    Element.el
        [ Element.Events.onClick colorReference
        , Element.width (Element.px 25)
        , Element.height (Element.px 25)
        , Element.Border.rounded 2
        , Element.Border.width 2
        , Element.Border.color borderColor
        , Ui.Style.shadowMedium
        ]
        inner


angleInput : Type -> Element.Element Type
angleInput type_ =
    let
        inputField lab update_ angle =
            let
                angleStr =
                    case angle of
                        Angle int ->
                            String.fromInt int

                        InvAngle str ->
                            str

                isValidAngle =
                    case angle of
                        Angle _ ->
                            True

                        InvAngle _ ->
                            False
            in
            Element.Input.text
                [ Element.Border.widthEach
                    { bottom = 3, left = 0, right = 0, top = 0 }
                , Element.width (Element.px 40)
                , Element.padding 6
                , Element.alignRight
                ]
                { onChange = update_
                , text = angleStr
                , placeholder =
                    angleStr
                        |> Element.text
                        |> Element.Input.placeholder []
                        |> Just
                , label = Element.Input.labelLeft [ Element.centerY ] <| Element.text lab
                }

        updateAngle str =
            let
                newAngle =
                    case String.toInt str of
                        Just val ->
                            if (val < 360) && (val >= 0) then
                                Angle val

                            else
                                InvAngle str

                        Nothing ->
                            InvAngle str
            in
            Linear newAngle
    in
    case type_ of
        Radial ->
            Element.none

        Linear angle ->
            inputField "Rotation" updateAngle angle


updateType : BackgroundColor -> Type -> Return
updateType bg newType =
    let
        newState =
            case bg of
                GradientBackground gradientState ->
                    let
                        gradient =
                            gradientState.gradient
                    in
                    { gradientState
                        | gradient =
                            { gradient | gradientType = newType }
                    }
                        |> GradientBackground

                _ ->
                    bg
    in
    Return Nothing newState


update : MouseEvent -> GradientState -> DesignSystem.Color.Model.Model -> Return
update msg gradientState model =
    let
        gradient =
            gradientState.gradient

        allColorPoints =
            completeListColorPoints gradient

        setColorBasePositionToMouseOnSlider mouseInfo gradientState_ =
            let
                newPositioning =
                    mousePositioning mouseInfo

                gradient_ =
                    gradientState_.gradient

                maybeNewGradient =
                    let
                        ( unpNewPos, unpOldPos ) =
                            ( unpackPositioning newPositioning
                            , unpackPositioning gradient_.colorBasePositioning
                            )

                        maxDelta =
                            20
                    in
                    if abs (unpNewPos - unpOldPos) <= maxDelta then
                        Just { gradient_ | colorBasePositioning = newPositioning }

                    else
                        Nothing

                newGradientState =
                    case maybeNewGradient of
                        Just newGradient ->
                            { gradientState_ | gradient = newGradient }

                        Nothing ->
                            { gradientState_ | isCloseEnough = False }
            in
            newGradientState

        setColorBasePositionToMouseOnColorPreview mouseInfo gradientState_ =
            let
                mousePos =
                    let
                        markerPxPosition =
                            toFloat (unpackPositioning gradientState_.gradient.colorBasePositioning)
                                * (sliderWidth / 100)
                    in
                    if markerPxPosition < colorPreviewSize / 2 then
                        toFloat mouseInfo.x * (100 / sliderWidth)

                    else if markerPxPosition > sliderWidth - (colorPreviewSize / 2) then
                        toFloat (mouseInfo.x + sliderWidth - colorPreviewSize)
                            * (100 / sliderWidth)

                    else
                        (toFloat mouseInfo.x - (toFloat colorPreviewSize / 2) + markerPxPosition)
                            * (100 / sliderWidth)

                newPositioning =
                    mousePos
                        |> round
                        |> clamp 0 100
                        |> Positioning

                gradient_ =
                    gradientState_.gradient

                newGradient =
                    { gradient_ | colorBasePositioning = newPositioning }

                newGradientState =
                    { gradientState_ | gradient = newGradient }
            in
            newGradientState
    in
    case msg of
        OnMouseDownSlider mouseInfo ->
            let
                mousePos =
                    mousePositioning mouseInfo

                withNewColorPoint =
                    let
                        unpMousePos =
                            unpackPositioning mousePos

                        maybeA : Maybe ( Positioning, Color.Color )
                        maybeA =
                            allColorPoints
                                |> List.filterMap
                                    (\( Positioning p, selection ) ->
                                        let
                                            maybeColor =
                                                DesignSystem.Color.Selection.getSelectedColor model selection
                                        in
                                        case ( p <= unpMousePos, maybeColor ) of
                                            ( True, Just color ) ->
                                                ( Positioning p, color ) |> Just

                                            _ ->
                                                Nothing
                                    )
                                |> closestPositioning mousePos

                        maybeB : Maybe ( Positioning, Color.Color )
                        maybeB =
                            allColorPoints
                                |> List.filterMap
                                    (\( Positioning p, selection ) ->
                                        let
                                            maybeColor =
                                                DesignSystem.Color.Selection.getSelectedColor model selection
                                        in
                                        case ( p >= unpMousePos, maybeColor ) of
                                            ( True, Just color ) ->
                                                ( Positioning p, color ) |> Just

                                            _ ->
                                                Nothing
                                    )
                                |> closestPositioning mousePos

                        computeNewColorPoint :
                            ( Positioning, Color.Color )
                            -> ( Positioning, Color.Color )
                            -> ( Positioning, Color.Color )
                        computeNewColorPoint ( posA, colorA ) ( posB, colorB ) =
                            let
                                factor =
                                    toFloat (unpMousePos - unpackPositioning posA)
                                        / toFloat (unpackPositioning posB - unpackPositioning posA)
                            in
                            ( mousePos
                            , averageColors factor colorA colorB
                            )

                        newColorPoint =
                            case ( maybeA, maybeB ) of
                                ( Just a, Just b ) ->
                                    computeNewColorPoint a b
                                        |> Tuple.mapSecond DesignSystem.Color.Selection.Standalone

                                ( Just ( _, colorA ), Nothing ) ->
                                    ( mousePos, colorA )
                                        |> Tuple.mapSecond DesignSystem.Color.Selection.Standalone

                                ( Nothing, Just ( _, colorB ) ) ->
                                    ( mousePos, colorB )
                                        |> Tuple.mapSecond DesignSystem.Color.Selection.Standalone

                                ( Nothing, Nothing ) ->
                                    ( mousePos, gradient.colorBase )
                    in
                    setColorBase newColorPoint gradient

                closestAsBase =
                    let
                        maybeClosest =
                            List.Extra.minimumBy
                                (Tuple.first
                                    >> unpackPositioning
                                    >> (-) (unpackPositioning mousePos)
                                    >> abs
                                )
                                allColorPoints
                    in
                    case maybeClosest of
                        Just closest ->
                            setColorBase closest gradient

                        Nothing ->
                            gradient

                newGradientState =
                    let
                        isCloseEnough_ =
                            completeListColorPoints gradient
                                |> List.any (isCloseEnough mousePos << Tuple.first)
                    in
                    setColorBasePositionToMouseOnSlider
                        mouseInfo
                        { gradientState
                            | gradient =
                                if isCloseEnough_ then
                                    closestAsBase

                                else
                                    withNewColorPoint
                            , isCloseEnough = isCloseEnough_
                            , isGrabbing = mouseInfo.mousePressed
                        }
            in
            Return Nothing (GradientBackground newGradientState)

        OnMouseMoveSlider mouseInfo ->
            let
                mousePos =
                    mousePositioning mouseInfo

                newGradientState =
                    setColorBasePositionToMouseOnSlider mouseInfo gradientState

                setOtherParams gradientState_ =
                    let
                        isCloseEnough_ =
                            completeListColorPoints gradient
                                |> List.any (isCloseEnough mousePos << Tuple.first)

                        isCloseEnoughOfGrabbed =
                            isCloseEnough mousePos gradient.colorBasePositioning
                    in
                    if gradientState_.isGrabbing then
                        { gradientState_
                            | isCloseEnough = isCloseEnoughOfGrabbed
                            , isGrabbing = mouseInfo.mousePressed
                        }

                    else
                        { gradientState_
                            | isCloseEnough = isCloseEnough_
                            , isGrabbing = mouseInfo.mousePressed
                        }
            in
            if mouseInfo.mousePressed then
                Return Nothing (GradientBackground (setOtherParams newGradientState))

            else
                Return Nothing (GradientBackground (setOtherParams gradientState))

        OnMouseDownColorPreview ( pos, selection ) mouseInfo ->
            let
                colorPreviewClickedAsBase =
                    setColorBase ( pos, selection ) gradient

                newGradientState =
                    setColorBasePositionToMouseOnColorPreview
                        mouseInfo
                        { gradientState
                            | gradient = colorPreviewClickedAsBase
                            , isGrabbing = mouseInfo.mousePressed
                        }
            in
            Return Nothing (GradientBackground newGradientState)

        OnMouseMoveColorPreview ( pos, selection ) mouseInfo ->
            let
                newGradientState =
                    setColorBasePositionToMouseOnColorPreview mouseInfo gradientState

                setIsGrabbing bool gradientState_ =
                    { gradientState_
                        | isGrabbing = bool
                    }
            in
            if
                mouseInfo.mousePressed
                    && (pos == gradientState.gradient.colorBasePositioning)
                    && (selection == gradientState.gradient.colorBase)
            then
                Return Nothing (GradientBackground (setIsGrabbing mouseInfo.mousePressed newGradientState))

            else
                Return Nothing (GradientBackground (setIsGrabbing False gradientState))

        OnMouseUp ->
            let
                newGradientState =
                    { gradientState
                        | isGrabbing = False
                    }
            in
            Return Nothing (GradientBackground newGradientState)


{-| factor between 0 (= on a) and 1 (= on b)
-}
averageColors : Float -> Color.Color -> Color.Color -> Color.Color
averageColors factor a b =
    let
        rgbaA =
            Color.toRgba a

        rgbaB =
            Color.toRgba b
    in
    Color.fromRgba
        { red = ((rgbaB.red - rgbaA.red) * factor) + rgbaA.red
        , green = ((rgbaB.green - rgbaA.green) * factor) + rgbaA.green
        , blue = ((rgbaB.blue - rgbaA.blue) * factor) + rgbaA.blue
        , alpha = ((rgbaB.alpha - rgbaA.alpha) * factor) + rgbaA.alpha
        }


edges =
    { top = 0, right = 0, left = 0, bottom = 0 }


unpackPositioning pos =
    case pos of
        Positioning val ->
            val


completeListColorPoints gradient =
    ( gradient.colorBasePositioning, gradient.colorBase ) :: gradient.colorPoints


mousePositioning mouseInfo =
    (toFloat mouseInfo.x * 100 / sliderWidth)
        |> round
        |> clamp 0 100
        |> Positioning


closestPositioning mousePos colorPoints =
    List.Extra.minimumBy
        (Tuple.first
            >> unpackPositioning
            >> (-) (unpackPositioning mousePos)
            >> abs
        )
        colorPoints


isCloseEnough positioningA positioningB =
    abs (unpackPositioning positioningA - unpackPositioning positioningB) < 5


setColorBase newBase gradient =
    let
        allColorPoints =
            completeListColorPoints gradient

        others =
            List.Extra.remove
                newBase
                allColorPoints
    in
    { gradient
        | colorBase = Tuple.second newBase
        , colorBasePositioning = Tuple.first newBase
        , colorPoints = others
    }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeAngle =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeAngleHelp


decodeAngleHelp constructor =
    case constructor of
        "Angle" ->
            Decode.map
                Angle
                (Decode.field "A1" Decode.int)

        "InvAngle" ->
            Decode.map
                InvAngle
                (Decode.field "A1" Decode.string)

        other ->
            Decode.fail <| "Unknown constructor for type Angle: " ++ other


decodeBackgroundColor =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeBackgroundColorHelp


decodeBackgroundColorHelp constructor =
    case constructor of
        "SolidBackground" ->
            Decode.map
                SolidBackground
                (Decode.field "A1" decodeAdvancedState)

        "GradientBackground" ->
            Decode.map
                GradientBackground
                (Decode.field "A1" decodeGradientState)

        other ->
            Decode.fail <| "Unknown constructor for type BackgroundColor: " ++ other


decodeGradient =
    Decode.map5
        Gradient
        (Decode.field "label" Decode.string)
        (Decode.field "colorBase" DesignSystem.Color.Selection.decodeSelection)
        (Decode.field "colorBasePositioning" decodePositioning)
        (Decode.field "colorPoints" (Decode.list decodeTuple_Positioning_DesignSystem_Color_Selection_Selection_))
        (Decode.field "gradientType" decodeType)


decodePositioning =
    Decode.map Positioning Decode.int


decodeTuple_Positioning_DesignSystem_Color_Selection_Selection_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" decodePositioning)
        (Decode.field "A2" DesignSystem.Color.Selection.decodeSelection)


decodeType =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeTypeHelp


decodeTypeHelp constructor =
    case constructor of
        "Linear" ->
            Decode.map
                Linear
                (Decode.field "A1" decodeAngle)

        "Radial" ->
            Decode.succeed Radial

        other ->
            Decode.fail <| "Unknown constructor for type Type: " ++ other


encodeAngle a =
    case a of
        Angle a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Angle" )
                , ( "A1", Encode.int a1 )
                ]

        InvAngle a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "InvAngle" )
                , ( "A1", Encode.string a1 )
                ]


encodeBackgroundColor a =
    case a of
        SolidBackground a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "SolidBackground" )
                , ( "A1", encodeAdvancedState a1 )
                ]

        GradientBackground a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "GradientBackground" )
                , ( "A1", encodeGradientState a1 )
                ]


encodeGradient a =
    Encode.object
        [ ( "label", Encode.string a.label )
        , ( "colorBase", DesignSystem.Color.Selection.encodeSelection a.colorBase )
        , ( "colorBasePositioning", encodePositioning a.colorBasePositioning )
        , ( "colorPoints", Encode.list encodeTuple_Positioning_DesignSystem_Color_Selection_Selection_ a.colorPoints )
        , ( "gradientType", encodeType a.gradientType )
        ]


encodePositioning (Positioning a1) =
    Encode.int a1


encodeTuple_Positioning_DesignSystem_Color_Selection_Selection_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", encodePositioning a1 )
        , ( "A2", DesignSystem.Color.Selection.encodeSelection a2 )
        ]


encodeType a =
    case a of
        Linear a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Linear" )
                , ( "A1", encodeAngle a1 )
                ]

        Radial ->
            Encode.object
                [ ( "Constructor", Encode.string "Radial" )
                ]



-- [generator-end]


encodeGradientState : GradientState -> Encode.Value
encodeGradientState =
    .gradient >> encodeGradient


decodeGradientState : Decode.Decoder GradientState
decodeGradientState =
    decodeGradient
        |> Decode.map (\grad -> { initGradientState | gradient = grad })
