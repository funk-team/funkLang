module Canvas.AttributesPanel.Style exposing (view)

import Canvas.AttributesPanel.Content.Shared
import Canvas.AttributesPanel.Shared
import Canvas.Msg
import Canvas.Selection
import Canvas.Tool.Draw.Model
import DesignSystem
import DesignSystem.Color
import DesignSystem.Color.Model
import DesignSystem.Msg
import DesignSystem.Shadow
import DesignSystem.Typography
import Dict
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import EventsExtra
import Google.Fonts
import Html.Attributes as Attrs
import IntDict
import Interface.Data
import Interface.Scope
import Model
import Model.Model
import Renderer.Help
import Spec
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Spec.Element.Style
import Spec.Element.Style.Edges
import Spec.Mutation
import Ui.Boxicons
import Ui.ColorPicker.Advanced
import Ui.ColorPicker.Gradient
import Ui.Component
import Ui.Dropdown
import Ui.Input
import Ui.RadioRow
import Ui.Style


type ScreenOrElement
    = Element
    | Screen


view :
    Canvas.Selection.SelectionItem
    -> Model.Model.Model
    -> Spec.Element.Model.EitherElement
    -> Element.Element Canvas.Msg.Msg
view selectionItem model element =
    let
        stylesOfThisElement =
            Spec.Element.Id.getFromDict
                element.shared.id
                elementStyles

        userModel : Model.Model.UserModel
        userModel =
            Model.latest model

        { designSystem, dataConnections, elementStyles } =
            userModel

        googleFonts =
            model.googleFonts

        -- Get the default styles for different element types
        styleOverrides =
            Maybe.withDefault
                (Spec.Element.Style.default element.shared.kind)
                stylesOfThisElement

        availableSettings =
            case element.outerGeometry of
                Spec.Element.Model.ScreenGeometry _ ->
                    [ viewBackgroundOptions designSystem element.shared.id styleOverrides Screen
                    , viewTypoTabs googleFonts designSystem element styleOverrides
                    ]

                _ ->
                    elementSettings

        kinds : List (Ui.Dropdown.DropdownRowParams Canvas.Tool.Draw.Model.Mode)
        kinds =
            [ { onSelect = Canvas.Tool.Draw.Model.Box
              , isSelected =
                    case element.shared.kind of
                        Spec.Element.Model.Box ->
                            True

                        _ ->
                            False
              , label =
                    (if Spec.hasTextConnection element userModel then
                        "Text"

                     else
                        "Box"
                    )
                        |> Ui.Dropdown.Description
              , sideNote = Ui.Dropdown.NoDetail
              , detail = Ui.Dropdown.NoDetail
              , rightHandText = Nothing
              }
            , { onSelect = Canvas.Tool.Draw.Model.Button
              , isSelected =
                    case element.shared.kind of
                        Spec.Element.Model.Button ->
                            True

                        _ ->
                            False
              , label = "Button" |> Ui.Dropdown.Description
              , sideNote = Ui.Dropdown.NoDetail
              , detail = Ui.Dropdown.NoDetail
              , rightHandText = Nothing
              }
            , { onSelect = Canvas.Tool.Draw.Model.TextInput
              , isSelected =
                    case element.shared.kind of
                        Spec.Element.Model.TextInput _ ->
                            True

                        _ ->
                            False
              , label = "Text Input" |> Ui.Dropdown.Description
              , sideNote = Ui.Dropdown.NoDetail
              , detail = Ui.Dropdown.NoDetail
              , rightHandText = Nothing
              }
            ]

        kindPicker =
            let
                label =
                    case element.shared.kind of
                        Spec.Element.Model.TextInput _ ->
                            "Text Input"

                        Spec.Element.Model.Box ->
                            if Spec.hasTextConnection element userModel then
                                "Text"

                            else
                                "Box"

                        Spec.Element.Model.Button ->
                            "Button"

                dropdown =
                    Ui.Dropdown.view []
                        { contents = List.map Ui.Dropdown.viewRow kinds
                        , label = label
                        }
                        |> Element.map (Spec.Mutation.ChangeElementKind selectionItem >> Canvas.Msg.GotMutation)
            in
            Element.column
                (Canvas.AttributesPanel.Shared.sectionStyles ++ [ Element.spacing 10 ])
                [ Element.el [ Element.Font.bold ] (Element.text "Element type")
                , dropdown
                ]

        elementSettings =
            kindPicker
                :: viewMediaSpecificOptions element selectionItem userModel
                ++ [ viewBackgroundOptions designSystem element.shared.id styleOverrides Element
                   , viewTypoTabs googleFonts designSystem element styleOverrides
                   , viewShadowOptions designSystem element.shared.id styleOverrides
                   , viewRoundedCornerOptions element.shared.id styleOverrides
                   , viewBorderSettingSelector element.shared.id designSystem styleOverrides
                   , viewClip element.shared.id styleOverrides
                   ]
    in
    Element.column
        [ Element.width Element.fill ]
        availableSettings


viewMediaSpecificOptions :
    Spec.Element.Model.Element a
    -> Canvas.Selection.SelectionItem
    -> Model.Model.UserModel
    -> List (Element.Element Canvas.Msg.Msg)
viewMediaSpecificOptions element selectionItem userModel =
    let
        id =
            element.shared.id

        refinedValue =
            Renderer.Help.getContent
                id
                userModel
                (Interface.Scope.populateForElement selectionItem userModel)
                |> Maybe.map .value

        objectFitControls =
            let
                controls =
                    Canvas.AttributesPanel.Content.Shared.imageCropSelector
                        element
                        userModel.elementStyles
            in
            Element.column
                (Element.spacing 10 :: Canvas.AttributesPanel.Shared.sectionStyles)
                [ Element.el [ Element.Font.bold ] (Element.text "Image fit")
                , controls
                , case Spec.Element.hasChildren element of
                    True ->
                        Element.paragraph
                            [ Element.Font.color Ui.Style.grey, Element.paddingEach { top = 5, bottom = 0, right = 0, left = 0 } ]
                            [ Element.text "An image Element with children can only be Cover or Contain."
                            ]

                    False ->
                        Element.none
                ]

        videoSettings =
            let
                styles =
                    Spec.Element.Id.getFromDict id userModel.elementStyles
                        |> Maybe.withDefault (Spec.Element.Style.default element.shared.kind)
            in
            Element.column
                (Element.spacing 10 :: Canvas.AttributesPanel.Shared.sectionStyles)
                [ Element.el [ Element.Font.bold ] (Element.text "Video settings")
                , viewVideoStyleOptions styles.videoStyle
                ]
                |> Element.map (\videoStyles -> Canvas.Msg.GotMutation <| Spec.Mutation.SetStyle id { styles | videoStyle = videoStyles })

        maybeVideoControls =
            case refinedValue of
                Nothing ->
                    []

                Just (Interface.Data.Media { kind }) ->
                    case kind of
                        Interface.Data.Image ->
                            []

                        Interface.Data.Video ->
                            [ videoSettings ]

                        _ ->
                            []

                _ ->
                    []
    in
    (if Interface.Data.supportsObjectFit refinedValue then
        [ objectFitControls ]

     else
        []
    )
        ++ maybeVideoControls


viewClip id styles =
    case styles.clip of
        False ->
            Element.row
                Canvas.AttributesPanel.Shared.sectionStyles
                [ Element.text "Clip"
                , Canvas.AttributesPanel.Shared.viewHeaderButton
                    "+"
                    (Canvas.Msg.SetStyleOverrides
                        id
                        { styles | clip = True }
                        Nothing
                    )
                ]

        True ->
            Element.row
                Canvas.AttributesPanel.Shared.sectionStyles
                [ Element.el [ Element.Font.bold ] <| Element.text "Clip"
                , Canvas.AttributesPanel.Shared.viewHeaderButton
                    "-"
                    (Canvas.Msg.SetStyleOverrides
                        id
                        { styles | clip = False }
                        Nothing
                    )
                ]


{-| Allow the user to apply border radius
-}
viewRoundedCornerOptions : Spec.Element.Id.Id -> Spec.Element.Style.Style -> Element.Element Canvas.Msg.Msg
viewRoundedCornerOptions id styles =
    case styles.roundedBorders of
        Nothing ->
            let
                addButton =
                    Canvas.AttributesPanel.Shared.viewHeaderButton
                        "+"
                        (Canvas.Msg.SetStyleOverrides
                            id
                            { styles | roundedBorders = Just <| Spec.Element.Style.CornerDimensions Nothing Nothing Nothing Nothing }
                            Nothing
                        )
            in
            Element.row
                Canvas.AttributesPanel.Shared.sectionStyles
                [ Element.text "Rounded Corners", addButton ]

        Just roundedBorders ->
            let
                { topLeft, topRight, bottomLeft, bottomRight } =
                    roundedBorders

                derived =
                    Spec.Element.Style.deriveCornerDimensions roundedBorders

                info =
                    Element.paragraph
                        [ Element.Font.color Ui.Style.grey, Element.paddingEach { top = 5, bottom = 0, right = 0, left = 0 } ]
                        [ Element.text "If the corner settings show no effect, try using the 'clip' setting below. Children or content might still have sharp corners as the round corners only apply to this element."
                        ]

                controls =
                    Element.column
                        (Element.spacing 5 :: Canvas.AttributesPanel.Shared.sectionStyles)
                        [ header
                        , info
                        , top
                        , bottom
                        ]

                removeButton =
                    Canvas.AttributesPanel.Shared.viewHeaderButton
                        "-"
                        (Canvas.Msg.SetStyleOverrides
                            id
                            { styles | roundedBorders = Nothing }
                            Nothing
                        )

                header =
                    Element.row
                        [ Element.width Element.fill ]
                        [ Element.text "Rounded Corners", removeButton ]

                top =
                    Element.row [ Element.paddingEach { top = 10, bottom = 0, right = 0, left = 0 } ]
                        [ Ui.Input.smartInt "Top Left" Element.Input.labelLeft topLeft derived.topLeft
                            |> Element.map (\topLeftVal -> { roundedBorders | topLeft = topLeftVal })
                        , Ui.Input.smartInt "Top Right" Element.Input.labelLeft topRight derived.topRight
                            |> Element.map (\topRightVal -> { roundedBorders | topRight = topRightVal })
                        ]
                        |> Element.map (\roundedBorders_ -> Canvas.Msg.SetStyleOverrides id { styles | roundedBorders = Just roundedBorders_ } Nothing)

                bottom =
                    Element.row
                        []
                        [ Ui.Input.smartInt "Bottom Right" Element.Input.labelLeft bottomLeft derived.bottomLeft
                            |> Element.map (\bottomLeftVal -> { roundedBorders | bottomLeft = bottomLeftVal })
                        , Ui.Input.smartInt "Bottom Left" Element.Input.labelLeft bottomRight derived.bottomRight
                            |> Element.map (\bottomRightVal -> { roundedBorders | bottomRight = bottomRightVal })
                        ]
                        |> Element.map (\roundedBorders_ -> Canvas.Msg.SetStyleOverrides id { styles | roundedBorders = Just roundedBorders_ } Nothing)
            in
            controls


{-| Allow the user to change the background of an element
-}
viewBackgroundOptions : DesignSystem.Model -> Spec.Element.Id.Id -> Spec.Element.Style.Style -> ScreenOrElement -> Element.Element Canvas.Msg.Msg
viewBackgroundOptions designSystem id styles screenOrElement =
    let
        ( defaultInitColor, headerText ) =
            case screenOrElement of
                Element ->
                    ( Ui.ColorPicker.Advanced.initAdvanced, "Background Color" )

                Screen ->
                    ( Ui.ColorPicker.Advanced.initAdvancedWithCustomColor DesignSystem.Color.white, "Screen Background Color" )
    in
    case styles.background of
        Nothing ->
            let
                addButton =
                    Canvas.AttributesPanel.Shared.viewHeaderButton
                        "+"
                        (Canvas.Msg.SetStyleOverrides
                            id
                            { styles | background = Just (Ui.ColorPicker.Gradient.SolidBackground defaultInitColor) }
                            Nothing
                        )
            in
            Element.row
                Canvas.AttributesPanel.Shared.sectionStyles
                [ Element.text headerText, addButton ]

        Just bg ->
            let
                removeButton =
                    Canvas.AttributesPanel.Shared.viewHeaderButton
                        "-"
                        (Canvas.Msg.SetStyleOverrides
                            id
                            { styles | background = Nothing }
                            Nothing
                        )

                header =
                    Element.row
                        [ Element.width Element.fill ]
                        [ Element.text headerText, removeButton ]

                body : Element.Element Canvas.Msg.Msg
                body =
                    Ui.ColorPicker.Gradient.view bg designSystem.colorEditor
                        |> Element.map
                            (\{ designSystemUpdate, state } ->
                                Canvas.Msg.SetStyleOverrides
                                    id
                                    { styles
                                        | background =
                                            state |> Just
                                    }
                                    designSystemUpdate
                            )
            in
            Element.column
                (Element.spacing 10 :: Canvas.AttributesPanel.Shared.sectionStyles)
                [ header, body ]


viewTypoTabs googleFonts designSystem element styleOverrides =
    Element.column [ Element.width Element.fill ]
        [ Element.row [ Element.width Element.fill ]
            [ viewTypoOptions googleFonts designSystem element styleOverrides ]
        ]


viewTypoOptions : Google.Fonts.Fonts -> DesignSystem.Model -> Spec.Element.Model.EitherElement -> Spec.Element.Style.Style -> Element.Element Canvas.Msg.Msg
viewTypoOptions googleFonts designSystem element styles =
    let
        baseStyles =
            case styles.typoTab of
                DesignSystem.Typography.Box ->
                    DesignSystem.Typography.baseStyles

                DesignSystem.Typography.Placeholder ->
                    DesignSystem.Typography.baseStylesPlaceHolder

        styles_ =
            case styles.typoTab of
                DesignSystem.Typography.Box ->
                    styles.elementText

                DesignSystem.Typography.Placeholder ->
                    styles.placeholderText

        stylesUpdate_ msg_ =
            case styles.typoTab of
                DesignSystem.Typography.Box ->
                    { styles
                        | elementText =
                            msg_
                    }

                DesignSystem.Typography.Placeholder ->
                    { styles
                        | placeholderText =
                            msg_
                    }

        addNew_ msg_ =
            { styles
                | elementText =
                    Spec.Element.Style.CustomTypo DesignSystem.Typography.baseStyles
                , placeholderText =
                    Spec.Element.Style.CustomTypo DesignSystem.Typography.baseStylesPlaceHolder
            }

        removeAll_ msg_ =
            { styles
                | elementText =
                    msg_
                , placeholderText =
                    msg_
            }

        derivedCustomTypo =
            Spec.Element.Style.CustomTypo derivedTypo

        derivedTypo =
            case styles_ of
                Spec.Element.Style.TypoFromDesignSystem id_ ->
                    case Dict.get id_ designSystem.typoEditor.typos of
                        Nothing ->
                            baseStyles

                        Just typo ->
                            { typo | name = "Custom", isBuiltIn = False }

                Spec.Element.Style.CustomTypo typo ->
                    { typo | name = "Custom", isBuiltIn = False }

                Spec.Element.Style.NoTypo ->
                    baseStyles

        header =
            let
                addOrRemoveButton =
                    case styles_ of
                        Spec.Element.Style.TypoFromDesignSystem id_ ->
                            case Dict.get id_ designSystem.typoEditor.typos of
                                Nothing ->
                                    addButton

                                Just typo ->
                                    removeButton

                        Spec.Element.Style.CustomTypo typo ->
                            removeButton

                        Spec.Element.Style.NoTypo ->
                            addButton

                addButton =
                    Canvas.AttributesPanel.Shared.viewHeaderButtonHelp
                        "Customize typography"
                        "+"
                        (Canvas.Msg.SetStyleOverrides
                            element.shared.id
                            (addNew_ (Spec.Element.Style.CustomTypo baseStyles))
                            Nothing
                        )

                removeButton =
                    Canvas.AttributesPanel.Shared.viewHeaderButton
                        "-"
                        (Canvas.Msg.SetStyleOverrides
                            element.shared.id
                            (removeAll_ Spec.Element.Style.NoTypo)
                            Nothing
                        )

                typoElementTypePicker =
                    Ui.Dropdown.view [ Element.width (Element.px 100) ]
                        { contents =
                            [ Ui.Dropdown.viewRow
                                { isSelected =
                                    if styles.typoTab == DesignSystem.Typography.Placeholder then
                                        True

                                    else
                                        False
                                , label = Ui.Dropdown.Description "Placeholder"
                                , onSelect =
                                    Canvas.Msg.SetStyleOverrides
                                        element.shared.id
                                        { styles | typoTab = DesignSystem.Typography.Placeholder }
                                        Nothing
                                , detail = Ui.Dropdown.Description "Update the placeholder text in the Context tab"
                                , sideNote = Ui.Dropdown.NoDetail
                                , rightHandText = Nothing
                                }
                            , Ui.Dropdown.viewRow
                                { isSelected =
                                    if styles.typoTab == DesignSystem.Typography.Box then
                                        True

                                    else
                                        False
                                , label = Ui.Dropdown.Description "Input text"
                                , onSelect =
                                    Canvas.Msg.SetStyleOverrides
                                        element.shared.id
                                        { styles | typoTab = DesignSystem.Typography.Box }
                                        Nothing
                                , detail = Ui.Dropdown.Description "Check the preview to see input styles"
                                , sideNote = Ui.Dropdown.NoDetail
                                , rightHandText = Nothing
                                }
                            ]
                        , label =
                            if styles.typoTab == DesignSystem.Typography.Box then
                                "Input"

                            else
                                "Placeholder"
                        }
                        |> Element.el [ Element.alignLeft ]

                isHeaderActive =
                    case styles_ of
                        Spec.Element.Style.TypoFromDesignSystem id_ ->
                            case Dict.get id_ designSystem.typoEditor.typos of
                                Nothing ->
                                    False

                                Just typo ->
                                    True

                        Spec.Element.Style.CustomTypo typo ->
                            True

                        Spec.Element.Style.NoTypo ->
                            False
            in
            Element.column [ Element.width Element.fill ]
                [ Element.row [ Element.width Element.fill ]
                    [ Element.el
                        [ if isHeaderActive then
                            Element.Font.bold

                          else
                            Element.Font.regular
                        ]
                        (Element.text <|
                            case element.outerGeometry of
                                Spec.Element.Model.ScreenGeometry _ ->
                                    "Screen Typography"

                                _ ->
                                    "Typography"
                        )

                    --:: shadowPickerDropDown --@TODO improve dropdown so that it doesn't overflow out of the screen
                    , case ( element.shared.kind, isHeaderActive ) of
                        ( Spec.Element.Model.TextInput _, True ) ->
                            typoElementTypePicker

                        ( _, _ ) ->
                            Element.none
                    , Element.el [ Element.width Element.fill ] addOrRemoveButton
                    ]
                ]

        typoPickerDropDown =
            let
                allTypos =
                    Dict.toList designSystem.typoEditor.typos

                viewTypoPicker maybeTypoSelectedId_ typo =
                    Ui.Dropdown.view []
                        { contents =
                            List.map
                                (typoPickerDropDownRow maybeTypoSelectedId_)
                                allTypos
                                |> List.append [ typoPickerDropDownRow_custom isCustom typo ]
                        , label = typo.name
                        }
                        |> Element.el [ Element.alignLeft, Element.htmlAttribute (Attrs.style "margin-left" "-6px"), Element.width (Element.px 100) ]
            in
            case styles_ of
                Spec.Element.Style.TypoFromDesignSystem id_ ->
                    case Dict.get id_ designSystem.typoEditor.typos of
                        Nothing ->
                            Element.none

                        Just typo ->
                            viewTypoPicker (Just id_) typo

                Spec.Element.Style.CustomTypo typo ->
                    viewTypoPicker Nothing typo

                Spec.Element.Style.NoTypo ->
                    Element.none

        typoPickerDropDownRow : Maybe Int -> ( Int, DesignSystem.Typography.Typo ) -> Ui.Dropdown.Row Canvas.Msg.Msg
        typoPickerDropDownRow maybeTypoSelectedId ( id_, typo ) =
            let
                unlinkButton =
                    Element.el
                        (EventsExtra.onClickStopPropagation
                            (Spec.Mutation.SetTypoOnElement
                                element.shared.id
                                derivedCustomTypo
                                styles.typoTab
                            )
                            :: linkIconAttr
                        )
                        (Ui.Component.icon Ui.Boxicons.bxUnlink)
                        |> Element.map Canvas.Msg.GotMutation

                isSelected =
                    case maybeTypoSelectedId of
                        Just selectedId_ ->
                            id_ == selectedId_

                        Nothing ->
                            False

                editableTypoName =
                    Ui.Component.contenteditable
                        { text = typo.name
                        , placeholder = "Typo name"
                        , enabled = not typo.isBuiltIn
                        }
                        |> Element.map
                            (\newName ->
                                Spec.Mutation.UpdateTypo
                                    id_
                                    { typo | name = newName }
                            )
                        |> Element.map Canvas.Msg.GotMutation
            in
            Ui.Dropdown.viewRow
                { isSelected =
                    Maybe.map ((==) id_) maybeTypoSelectedId
                        |> Maybe.withDefault False
                , label =
                    if isSelected then
                        Ui.Dropdown.Preview editableTypoName

                    else
                        Ui.Dropdown.Description typo.name
                , onSelect =
                    Canvas.Msg.SetStyleOverrides
                        element.shared.id
                        (stylesUpdate_ (Spec.Element.Style.TypoFromDesignSystem id_))
                        Nothing
                , detail = Ui.Dropdown.NoDetail
                , sideNote =
                    if isSelected then
                        Ui.Dropdown.Preview unlinkButton

                    else
                        Ui.Dropdown.NoDetail
                , rightHandText =
                    case typo.isBuiltIn of
                        True ->
                            Just buildInDropDownText

                        False ->
                            Nothing
                }

        typoPickerDropDownRow_custom isSelected typo =
            let
                linkButton =
                    let
                        typoIndex =
                            IntDict.nextId designSystem.typoEditor.typos
                    in
                    Element.el
                        (EventsExtra.onClickStopPropagation
                            (Spec.Mutation.BatchMutation
                                [ Spec.Mutation.UpdateTypo
                                    typoIndex
                                    { typo | name = "Typo-" ++ String.fromInt typoIndex, isBuiltIn = False }
                                , Spec.Mutation.SetTypoOnElement
                                    element.shared.id
                                    (Spec.Element.Style.TypoFromDesignSystem typoIndex)
                                    styles.typoTab
                                ]
                            )
                            :: linkIconAttr
                        )
                        (Ui.Component.icon Ui.Boxicons.bxPlus)
            in
            Ui.Dropdown.viewRow
                { isSelected = isSelected
                , label = Ui.Dropdown.Description "Custom"
                , onSelect =
                    Canvas.Msg.SetStyleOverrides
                        element.shared.id
                        (stylesUpdate_ (Spec.Element.Style.CustomTypo baseStyles))
                        Nothing
                , detail = Ui.Dropdown.NoDetail
                , sideNote =
                    (if isSelected then
                        linkButton

                     else
                        Element.none
                    )
                        |> Element.map Canvas.Msg.GotMutation
                        |> Ui.Dropdown.Preview
                , rightHandText = Nothing
                }

        isCustom =
            case styles_ of
                Spec.Element.Style.CustomTypo _ ->
                    True

                _ ->
                    False

        editors : Maybe Int -> DesignSystem.Typography.Typo -> Element.Element Canvas.Msg.Msg
        editors maybeId typo =
            let
                updateTypo ( newTypo, swatchUpdate ) =
                    case maybeId of
                        Nothing ->
                            Canvas.Msg.SetStyleOverrides
                                element.shared.id
                                (stylesUpdate_ (Spec.Element.Style.CustomTypo newTypo))
                                swatchUpdate

                        Just id_ ->
                            ( DesignSystem.Typography.UpdateSelectedTypo id_ ( newTypo, swatchUpdate )
                            , case swatchUpdate of
                                Nothing ->
                                    designSystem.colorEditor

                                Just swatchUpdate_ ->
                                    DesignSystem.Color.updateSwatch swatchUpdate_ designSystem.colorEditor
                            )
                                |> DesignSystem.Msg.GotTypoMsg
                                |> Canvas.Msg.DesignSystemMsg
            in
            Element.column
                [ Element.spacing 15 ]
                [ Element.row [ Element.spacing 10, Element.htmlAttribute (Attrs.style "margin-left" "-7px") ]
                    [ DesignSystem.Typography.viewFontEditor googleFonts typo
                        |> Element.map updateTypo
                    , Element.el [ Element.htmlAttribute (Attrs.style "margin-left" "20px") ]
                        (DesignSystem.Typography.viewColor designSystem.typoEditor typo designSystem.colorEditor
                            |> Element.map updateTypo
                        )
                    ]
                , Element.row [ Element.spacing 5, Element.htmlAttribute (Attrs.style "margin-left" "5px") ]
                    [ Element.text "Varient"
                    , DesignSystem.Typography.viewVariantEditor designSystem.typoEditor typo
                        |> Element.map updateTypo
                    ]
                , Element.row [ Element.spacing 10, Element.htmlAttribute (Attrs.style "margin-left" "5px") ]
                    [ DesignSystem.Typography.viewSizeEditor designSystem.typoEditor typo
                        |> Element.map updateTypo
                    , DesignSystem.Typography.viewLineHeightEditor typo
                        |> Element.map updateTypo
                    , DesignSystem.Typography.viewLetterSpacingEditor designSystem.typoEditor typo
                        |> Element.map updateTypo
                    ]
                , Element.row [ Element.spacing 15, Element.scale 0.7, Element.htmlAttribute (Attrs.style "margin-left" "-30px") ]
                    [ DesignSystem.Typography.viewAlignmentEditor designSystem.typoEditor typo
                        |> Element.map updateTypo
                    , DesignSystem.Typography.viewFontStyles designSystem.typoEditor typo
                        |> Element.map updateTypo
                    ]
                , Element.row [ Element.htmlAttribute (Attrs.style "margin-left" "5px") ]
                    [ DesignSystem.Typography.viewShadow designSystem.typoEditor typo designSystem.colorEditor
                        |> Element.map updateTypo
                    ]

                -- We don't render the wrap attr for input fields as it has no effect in the canvas or preview as
                -- TODO Uodate the Render/Content.elm to work with wrap on text in put
                , case styles.typoTab of
                    DesignSystem.Typography.Box ->
                        DesignSystem.Typography.viewWrap derivedTypo
                            |> Element.map updateTypo

                    DesignSystem.Typography.Placeholder ->
                        Element.none
                ]

        linkIconAttr =
            [ Element.centerY
            , Element.alignRight
            , Element.Font.color (Element.rgb 1 1 1)
            , Element.Font.size 10
            , Element.paddingEach { edges | right = 5 }
            , Element.pointer
            , Element.scale 0.6
            ]

        hint =
            case element.outerGeometry of
                Spec.Element.Model.ScreenGeometry _ ->
                    Element.paragraph
                        [ Element.Font.color Ui.Style.grey, Element.paddingEach { top = 5, bottom = 0, right = 0, left = 0 } ]
                        [ Element.text "Text styles on screens will override the default text styles from the design system."
                        ]

                _ ->
                    Element.none

        hintDefaults =
            case element.outerGeometry of
                Spec.Element.Model.ScreenGeometry _ ->
                    Element.paragraph
                        [ Element.Font.color Ui.Style.grey, Element.paddingEach { top = 5, bottom = 0, right = 0, left = 0 } ]
                        [ Element.text "The default screen typography is set to 'body copy' from the design system."
                        ]

                _ ->
                    Element.none
    in
    Element.column
        ([ Element.spacing 5, Element.width Element.fill ] ++ Canvas.AttributesPanel.Shared.sectionStyles)
        (case styles_ of
            Spec.Element.Style.TypoFromDesignSystem id_ ->
                case Dict.get id_ designSystem.typoEditor.typos of
                    Nothing ->
                        [ header, hintDefaults ]

                    Just typo ->
                        [ header, typoPickerDropDown, editors (Just id_) typo, hint ]

            Spec.Element.Style.CustomTypo typo ->
                [ header, typoPickerDropDown, editors Nothing typo, hint ]

            Spec.Element.Style.NoTypo ->
                [ header, hintDefaults ]
        )


viewShadowOptions : DesignSystem.Model -> Spec.Element.Id.Id -> Spec.Element.Style.Style -> Element.Element Canvas.Msg.Msg
viewShadowOptions designSystem id styles =
    let
        derivedCustomShadow =
            Spec.Element.Style.CustomShadow derivedShadow

        derivedShadow =
            case styles.shadow of
                Spec.Element.Style.ShadowFromDesignSystem id_ ->
                    case Dict.get id_ designSystem.shadows.shadows of
                        Nothing ->
                            DesignSystem.Shadow.initShadow "Custom"

                        Just shadow ->
                            { shadow | name = "Custom", isBuiltIn = False }

                Spec.Element.Style.CustomShadow shadow ->
                    { shadow | name = "Custom", isBuiltIn = False }

                Spec.Element.Style.NoShadow ->
                    DesignSystem.Shadow.initShadow "Custom"

        header =
            let
                addOrRemoveButton =
                    case styles.shadow of
                        Spec.Element.Style.ShadowFromDesignSystem id_ ->
                            case Dict.get id_ designSystem.shadows.shadows of
                                Nothing ->
                                    [ addButton ]

                                Just shadow ->
                                    [ removeButton ]

                        Spec.Element.Style.CustomShadow shadow ->
                            [ removeButton ]

                        Spec.Element.Style.NoShadow ->
                            [ addButton ]

                addButton =
                    Canvas.AttributesPanel.Shared.viewHeaderButton
                        "+"
                        (Canvas.Msg.SetStyleOverrides
                            id
                            { styles
                                | shadow =
                                    Spec.Element.Style.CustomShadow
                                        (DesignSystem.Shadow.initShadow "Custom")
                            }
                            Nothing
                        )

                removeButton =
                    Canvas.AttributesPanel.Shared.viewHeaderButton
                        "-"
                        (Canvas.Msg.SetStyleOverrides
                            id
                            { styles | shadow = Spec.Element.Style.NoShadow }
                            Nothing
                        )
            in
            Element.row
                [ Element.width Element.fill ]
                (Element.text "Shadow    "
                    --:: shadowPickerDropDown --@TODO improve dropdown so that it doesn't overflow out of the screen
                    :: addOrRemoveButton
                )

        shadowPickerDropDown =
            let
                allShadows =
                    Dict.toList designSystem.shadows.shadows

                dropdown maybeShadowSelectedId_ shadow =
                    Ui.Dropdown.view
                        [ Element.htmlAttribute (Attrs.style "margin-left" "-6px"), Element.width (Element.px 100) ]
                        { contents =
                            List.map (shadowPickerDropDownRow maybeShadowSelectedId_) allShadows
                                |> List.append [ shadowPickerDropDownRow_custom isCustom shadow ]
                        , label = shadow.name
                        }
                        |> Element.el [ Element.alignLeft ]
            in
            case styles.shadow of
                Spec.Element.Style.ShadowFromDesignSystem id_ ->
                    case Dict.get id_ designSystem.shadows.shadows of
                        Nothing ->
                            Element.none

                        Just shadow ->
                            dropdown (Just id_) shadow

                Spec.Element.Style.CustomShadow shadow ->
                    dropdown Nothing shadow

                Spec.Element.Style.NoShadow ->
                    Element.none

        shadowPickerDropDownRow maybeShadowSelectedId ( id_, shadow ) =
            let
                unlinkButton =
                    Element.el
                        (EventsExtra.onClickStopPropagation
                            (Spec.Mutation.SetShadowOnElement
                                id
                                derivedCustomShadow
                            )
                            :: linkIconAttr
                        )
                        (Ui.Component.icon Ui.Boxicons.bxUnlink)
                        |> Element.map Canvas.Msg.GotMutation

                isSelected =
                    case maybeShadowSelectedId of
                        Just selectedId_ ->
                            id_ == selectedId_

                        Nothing ->
                            False

                editableShadowName =
                    Ui.Component.contenteditable
                        { text = shadow.name
                        , placeholder = "Typo name"
                        , enabled = not shadow.isBuiltIn
                        }
                        |> Element.map
                            (\newName ->
                                Spec.Mutation.UpdateShadows
                                    id_
                                    { shadow | name = newName }
                            )
                        |> Element.map Canvas.Msg.GotMutation
            in
            Ui.Dropdown.viewRow
                { isSelected =
                    Maybe.map ((==) id_) maybeShadowSelectedId
                        |> Maybe.withDefault False
                , label =
                    if isSelected then
                        Ui.Dropdown.Preview editableShadowName

                    else
                        Ui.Dropdown.Description shadow.name
                , onSelect =
                    Canvas.Msg.SetStyleOverrides
                        id
                        { styles | shadow = Spec.Element.Style.ShadowFromDesignSystem id_ }
                        Nothing
                , detail =
                    viewPreview designSystem.colorEditor shadow
                        |> Element.el [ Element.paddingXY 5 5, Element.width Element.fill ]
                        |> Ui.Dropdown.Preview
                , sideNote =
                    if isSelected then
                        unlinkButton |> Ui.Dropdown.Preview

                    else
                        Ui.Dropdown.NoDetail
                , rightHandText =
                    case shadow.isBuiltIn of
                        True ->
                            Just buildInDropDownText

                        False ->
                            Nothing
                }

        shadowPickerDropDownRow_custom isSelected shadow =
            let
                linkButton =
                    let
                        shadowIndex =
                            IntDict.nextId designSystem.shadows.shadows
                    in
                    Element.el
                        (EventsExtra.onClickStopPropagation
                            (Spec.Mutation.BatchMutation
                                [ Spec.Mutation.UpdateShadows
                                    shadowIndex
                                    { shadow | name = "Shadow-" ++ String.fromInt shadowIndex, isBuiltIn = False }
                                , Spec.Mutation.SetShadowOnElement
                                    id
                                    (Spec.Element.Style.ShadowFromDesignSystem shadowIndex)
                                ]
                            )
                            :: linkIconAttr
                        )
                        (Ui.Component.icon Ui.Boxicons.bxPlus)
            in
            Ui.Dropdown.viewRow
                { isSelected = isSelected
                , label = Ui.Dropdown.Description "Custom"
                , onSelect =
                    Canvas.Msg.SetStyleOverrides
                        id
                        { styles
                            | shadow =
                                Spec.Element.Style.CustomShadow { shadow | name = "Custom", isBuiltIn = False }
                        }
                        Nothing
                , detail = Ui.Dropdown.NoDetail
                , sideNote =
                    (if isSelected then
                        linkButton

                     else
                        Element.none
                    )
                        |> Element.map Canvas.Msg.GotMutation
                        |> Ui.Dropdown.Preview
                , rightHandText = Nothing
                }

        isCustom =
            case styles.shadow of
                Spec.Element.Style.CustomShadow _ ->
                    True

                _ ->
                    False

        editors maybeId shadow =
            let
                updateShadow ( newShadow, swatchUpdate ) =
                    case maybeId of
                        Nothing ->
                            -- (==) isCustom
                            Canvas.Msg.SetStyleOverrides
                                id
                                { styles | shadow = Spec.Element.Style.CustomShadow newShadow }
                                swatchUpdate

                        Just id_ ->
                            ( DesignSystem.Shadow.UpdateShadow id_ newShadow
                            , case swatchUpdate of
                                Nothing ->
                                    designSystem.colorEditor

                                Just swatchUpdate_ ->
                                    DesignSystem.Color.updateSwatch swatchUpdate_ designSystem.colorEditor
                            )
                                |> DesignSystem.Msg.GotShadowMsg
                                |> Canvas.Msg.DesignSystemMsg
            in
            Element.column
                [ Element.spacing 15 ]
                [ viewTypeEditor shadow
                    |> Element.map (\a -> updateShadow ( a, Nothing ))
                , Element.row
                    [ Element.width Element.fill
                    , Element.spaceEvenly
                    , Element.spacing 15
                    ]
                    [ viewColorEditor designSystem.colorEditor shadow
                        |> Element.map updateShadow
                    , Element.column
                        [ Element.width Element.fill ]
                        [ viewSizeEditor shadow
                            |> Element.map (\a -> updateShadow ( a, Nothing ))
                        , viewBlurEditor shadow
                            |> Element.map (\a -> updateShadow ( a, Nothing ))
                        ]
                    , viewOffsetEditor shadow
                        |> Element.map (\a -> updateShadow ( a, Nothing ))
                    ]
                , viewPreview designSystem.colorEditor shadow
                ]

        linkIconAttr =
            [ Element.centerY
            , Element.alignRight
            , Element.Font.color (Element.rgb 1 1 1)
            , Element.Font.size 10
            , Element.paddingEach { edges | right = 5 }
            , Element.pointer
            , Element.scale 0.6
            ]
    in
    Element.column
        (Element.spacing 5 :: Canvas.AttributesPanel.Shared.sectionStyles)
        (case styles.shadow of
            Spec.Element.Style.ShadowFromDesignSystem id_ ->
                case Dict.get id_ designSystem.shadows.shadows of
                    Nothing ->
                        [ header ]

                    Just shadow ->
                        [ header, shadowPickerDropDown, editors (Just id_) shadow ]

            Spec.Element.Style.CustomShadow shadow ->
                [ header, shadowPickerDropDown, editors Nothing shadow ]

            Spec.Element.Style.NoShadow ->
                [ header ]
        )


viewOffsetEditor : DesignSystem.Shadow.Shadow -> Element.Element DesignSystem.Shadow.Shadow
viewOffsetEditor shadow =
    let
        updateXInput input =
            { shadow | offset = ( input, y ) }

        updateYInput input =
            { shadow | offset = ( x, input ) }

        ( x, y ) =
            shadow.offset
    in
    Element.column
        [ Element.width Element.fill ]
        [ Element.el
            []
            (Ui.Input.smartInt "X" Element.Input.labelLeft x (Maybe.withDefault DesignSystem.Shadow.defaultOffsetX x)
                |> Element.map updateXInput
            )
        , Element.el
            []
            (Ui.Input.smartInt "Y" Element.Input.labelLeft y (Maybe.withDefault DesignSystem.Shadow.defaultOffsetY y)
                |> Element.map updateYInput
            )
        ]


viewSizeEditor : DesignSystem.Shadow.Shadow -> Element.Element DesignSystem.Shadow.Shadow
viewSizeEditor shadow =
    let
        updateSizeInput input =
            { shadow | size = input }
    in
    Element.el
        [ Element.alignTop ]
        (Ui.Input.smartInt "Size" Element.Input.labelLeft shadow.size (Maybe.withDefault DesignSystem.Shadow.defaultSize shadow.size)
            |> Element.map updateSizeInput
        )


viewBlurEditor : DesignSystem.Shadow.Shadow -> Element.Element DesignSystem.Shadow.Shadow
viewBlurEditor shadow =
    let
        updateBlurInput input =
            { shadow | blur = input }
    in
    Element.el
        [ Element.alignTop ]
        (Ui.Input.smartInt "Blur" Element.Input.labelLeft shadow.blur (Maybe.withDefault DesignSystem.Shadow.defaultBlur shadow.blur)
            |> Element.map updateBlurInput
        )


viewColorEditor : DesignSystem.Color.Model.Model -> DesignSystem.Shadow.Shadow -> Element.Element ( DesignSystem.Shadow.Shadow, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
viewColorEditor colorModel shadow =
    let
        updateColorInput { designSystemUpdate, state } =
            ( { shadow | color = state }, designSystemUpdate )
    in
    Element.el
        [ Element.alignTop
        , Element.centerY
        ]
        (Ui.ColorPicker.Advanced.viewAdvanced shadow.color colorModel
            |> Element.map updateColorInput
        )


viewTypeEditor : DesignSystem.Shadow.Shadow -> Element.Element DesignSystem.Shadow.Shadow
viewTypeEditor shadow =
    let
        updateTypeInput input =
            { shadow | type_ = input }

        radioRowParams =
            { items = [ DesignSystem.Shadow.OuterShadow, DesignSystem.Shadow.InnerShadow ]
            , toLabel =
                \a ->
                    case a of
                        DesignSystem.Shadow.OuterShadow ->
                            "OuterShadow"

                        DesignSystem.Shadow.InnerShadow ->
                            "InnerShadow"
            , selected = (==) shadow.type_
            }
    in
    Element.el
        [ Element.alignTop
        ]
        (Ui.RadioRow.view radioRowParams
            |> Element.map updateTypeInput
        )


viewPreview colorModel shadow =
    Element.el
        ([ Element.width (Element.px 150)
         , Element.height (Element.px 40)
         , Element.Border.rounded 3
         , Element.centerX
         , Element.Background.color (Element.rgb 1 1 1)
         ]
            ++ DesignSystem.Shadow.shadowToAttr colorModel shadow
        )
        Element.none
        |> Element.el
            [ Element.width Element.fill
            , Element.paddingXY 0 20
            , Element.clip
            , Element.Background.color (Element.rgb 1 1 1)
            ]


{-| View border width and color control
-}
viewBorderSettingSelector :
    Spec.Element.Id.Id
    -> DesignSystem.Model
    -> Spec.Element.Style.Style
    -> Element.Element Canvas.Msg.Msg
viewBorderSettingSelector id designSystem styles =
    case styles.borderSettings of
        Nothing ->
            let
                addButton =
                    Canvas.AttributesPanel.Shared.viewHeaderButton
                        "+"
                        (Canvas.Msg.SetStyleOverrides
                            id
                            { styles
                                | borderSettings =
                                    Just
                                        { color = Ui.ColorPicker.Advanced.initAdvanced
                                        , dimensions = Spec.Element.Style.Edges.init
                                        }
                            }
                            Nothing
                        )
            in
            Element.row
                Canvas.AttributesPanel.Shared.sectionStyles
                [ Element.text "Border", addButton ]

        Just settings ->
            let
                borderSizes =
                    Canvas.AttributesPanel.Shared.edgesEditor
                        settings.dimensions
                        (Spec.Element.Style.Edges.deriveEdgeDimensions settings.dimensions)

                removeButton =
                    Canvas.AttributesPanel.Shared.viewHeaderButton
                        "-"
                        (Canvas.Msg.SetStyleOverrides
                            id
                            { styles | borderSettings = Nothing }
                            Nothing
                        )

                header =
                    Element.row
                        [ Element.width Element.fill ]
                        [ Element.text "Border", removeButton ]
            in
            Element.column
                Canvas.AttributesPanel.Shared.sectionStyles
                [ header
                , Ui.ColorPicker.Advanced.viewAdvanced settings.color designSystem.colorEditor
                    |> Element.map (\{ designSystemUpdate, state } -> Canvas.Msg.SetStyleOverrides id { styles | borderSettings = Just { settings | color = state } } designSystemUpdate)
                , borderSizes
                    |> Element.map (\dimensions -> Canvas.Msg.SetStyleOverrides id { styles | borderSettings = Just { settings | dimensions = dimensions } } Nothing)
                ]


viewVideoStyleOptions styles =
    let
        loopControls =
            Ui.RadioRow.view
                { items = [ False, True ]
                , selected = (==) styles.loop
                , toLabel =
                    \loop ->
                        case loop of
                            True ->
                                "Loop"

                            False ->
                                "Once"
                }
                |> Element.map (\loop -> { styles | loop = loop })

        autoplayControls =
            Ui.RadioRow.view
                { items = [ False, True ]
                , selected = (==) styles.autoplay
                , toLabel =
                    \autoplay ->
                        case autoplay of
                            True ->
                                "AutoPlay"

                            False ->
                                "Paused"
                }
                |> Element.map (\autoplay -> { styles | autoplay = autoplay })

        controlsControls =
            Ui.RadioRow.view
                { items = [ False, True ]
                , selected = (==) styles.controls
                , toLabel =
                    \controls ->
                        case controls of
                            True ->
                                "Show controls"

                            False ->
                                "Immersive"
                }
                |> Element.map (\controls -> { styles | controls = controls })
    in
    Element.column [ Element.spacing 5, Element.width Element.fill ]
        [ Element.row [ Element.width Element.fill ] [ Element.el [] <| Element.text "Player", Element.el [ Element.alignRight ] controlsControls ]
        , Element.row [ Element.width Element.fill ] [ Element.el [] <| Element.text "On load", Element.el [ Element.alignRight ] autoplayControls ]
        , Element.row [ Element.width Element.fill ] [ Element.el [] <| Element.text "Repeat", Element.el [ Element.alignRight ] loopControls ]
        ]


buildInDropDownText : String
buildInDropDownText =
    "(Built-in) "


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }
