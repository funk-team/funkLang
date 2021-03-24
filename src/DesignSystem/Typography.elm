module DesignSystem.Typography exposing (..)

{-| Use this module to render the typo editor menu and to work with typos in general.
-}

import Cmd.Extra
import Color
import Color.Extra
import DesignSystem.Color
import DesignSystem.Color.Model
import DesignSystem.Color.Selection
import Dict
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Lazy
import Google.Fonts
import Help
import Html.Attributes
import IntDict
import Json.Decode as Decode
import Json.Decode.Extra as Extra
import Json.Encode as Encode
import RemoteData
import Ui.Boxicons
import Ui.ColorPicker.Advanced
import Ui.Component
import Ui.DesignSystem
import Ui.Dropdown
import Ui.Input
import Ui.Style
import ZipList


type Msg
    = UpdateSelectedTypo Int ( Typo, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
    | SelectTypo Int
    | RemoveTypo Int Bool
    | OpenAddTypoInput
    | UpdateAddTypoInput String
    | SubmitAddTypoInput
    | CancelAddTypoInput
    | AddTypo
    | UpdateName Int Bool String


type alias TypoTab =
    TypoTabV2


decodeTypoTab =
    Decode.oneOf
        [ decodeTypoTabV2
        , Decode.succeed Box
        ]


encodeTypoTab =
    encodeTypoTabV2



-- [generator-start]


type TypoTabV2
    = Box
    | Placeholder


type alias Model =
    { -- typos
      typos : IntDict.Dict Typo

    -- select and edit typos
    , selection : Int
    , addTypoInput : Maybe String
    }


type alias Typo =
    { name : String
    , font : Google.Fonts.Font
    , size : Maybe Int
    , lineHeight : Maybe Int
    , letterSpacing : Maybe Int
    , alignment : FontAlignment
    , styles : Maybe (List FontStyles)
    , color : Ui.ColorPicker.Advanced.AdvancedState
    , shadow : Maybe ShadowSettings
    , weight : Weight
    , shouldWrap : Bool
    , isBuiltIn : Bool
    }


defaultSize =
    16


defaultLineHeight =
    4


defaultLetterSpacing =
    0


defaulAlignment =
    AlignLeft


defaultColorState =
    Ui.ColorPicker.Advanced.initAdvanced


defaultShadow =
    Nothing


defaultWeight =
    Regular


defaultNameElementStyles =
    "Custom"


defaultNameNothingSelected =
    "None"


defaultPlaceholderStyles =
    [ Element.Font.color Ui.Style.grey ]


baseStyles : Typo
baseStyles =
    { name = "Custom"
    , font = Google.Fonts.defaultFont
    , size = Just 16
    , lineHeight = Just defaultLineHeight
    , letterSpacing = Nothing
    , alignment = AlignLeft
    , styles = Nothing
    , color = Ui.ColorPicker.Advanced.initAdvancedWithCustomColor Color.black

    -- , color = defaultColorState
    , shadow = Nothing
    , weight = defaultWeight
    , shouldWrap = True
    , isBuiltIn = False
    }


baseStylesPlaceHolder : Typo
baseStylesPlaceHolder =
    { baseStyles | color = Ui.ColorPicker.Advanced.initAdvancedWithCustomColor Color.grey }


type FontAlignment
    = AlignLeft
    | AlignRight
    | Center


type FontStyles
    = UnderLine
    | StrikeThrough
    | Italic


type alias ShadowSettings =
    { offsetX : Maybe Int
    , offsetY : Maybe Int
    , blur : Maybe Int
    , color : Ui.ColorPicker.Advanced.AdvancedState
    }


type Weight
    = Heavy
    | ExtraBold
    | Bold
    | SemiBold
    | Medium
    | Regular
    | Light
    | ExtraLight
    | HairLine


weightToElmUi : Weight -> Element.Attribute msg
weightToElmUi weight =
    case weight of
        Heavy ->
            Element.Font.heavy

        ExtraBold ->
            Element.Font.extraBold

        Bold ->
            Element.Font.bold

        SemiBold ->
            Element.Font.semiBold

        Medium ->
            Element.Font.medium

        Regular ->
            Element.Font.regular

        Light ->
            Element.Font.light

        ExtraLight ->
            Element.Font.extraLight

        HairLine ->
            Element.Font.hairline


alignmentxToElmUi : FontAlignment -> Element.Attribute msg
alignmentxToElmUi alignment_ =
    case alignment_ of
        AlignLeft ->
            Element.Font.alignLeft

        AlignRight ->
            Element.Font.alignRight

        Center ->
            Element.Font.center


styleToElmUi : FontStyles -> Element.Attribute msg
styleToElmUi style_ =
    case style_ of
        UnderLine ->
            Element.Font.underline

        StrikeThrough ->
            Element.Font.strike

        Italic ->
            Element.Font.italic


sizeToElmUi : Maybe Int -> Element.Attribute msg
sizeToElmUi size_ =
    Element.Font.size (size_ |> Maybe.withDefault defaultSize)


letterSpacingToElmUi : Maybe Int -> Element.Attribute msg
letterSpacingToElmUi letterSpacing_ =
    Element.Font.letterSpacing
        (letterSpacing_
            |> Maybe.withDefault defaultLetterSpacing
            |> toFloat
        )


colorToElmUi : Ui.ColorPicker.Advanced.AdvancedState -> DesignSystem.Color.Model.Model -> List (Element.Attr decorative msg)
colorToElmUi fontColor_ colorDesignSystem =
    let
        textColor =
            fontColor_
                |> (.selection >> DesignSystem.Color.Selection.getSelectedColor colorDesignSystem)
                |> Maybe.map (Color.Extra.toElmUi >> Element.Font.color >> List.singleton)
                |> Maybe.withDefault []
    in
    textColor


shadowToElmUi : Maybe ShadowSettings -> DesignSystem.Color.Model.Model -> List (Element.Attribute msg)
shadowToElmUi shadow_ colorDesignSystem =
    case shadow_ of
        Nothing ->
            [ Ui.Style.style "text-shadow" "none" ]

        Just shadowValues ->
            let
                offset =
                    ( shadowValues.offsetX |> Maybe.withDefault 0 |> toFloat
                    , shadowValues.offsetY |> Maybe.withDefault 0 |> toFloat
                    )

                shadowSettings =
                    { offset = offset
                    , blur = shadowValues.blur |> Maybe.withDefault 0 |> toFloat
                    , color = shadowColor
                    }

                shadowColor =
                    shadowValues.color
                        |> (.selection >> DesignSystem.Color.Selection.getSelectedColor colorDesignSystem)
                        |> Maybe.map Color.Extra.toElmUi
                        |> Maybe.withDefault (Element.rgba 0 0 0 1)
            in
            [ Element.Font.shadow shadowSettings
            ]



-- interesting code


init : ( Model, Cmd Msg )
init =
    ( { typos =
            IntDict.empty
                |> IntDict.insertNew
                    { baseStyles | name = "h1", size = Just 60, isBuiltIn = True }
                |> IntDict.insertNew
                    { baseStyles | name = "h2", size = Just 40, isBuiltIn = True }
                |> IntDict.insertNew
                    { baseStyles | name = "h3", size = Just 20, isBuiltIn = True }
                |> IntDict.insertNew
                    { baseStyles | name = "Body Copy", isBuiltIn = True }
      , selection = 1
      , addTypoInput = Nothing
      }
    , Cmd.none
    )


typoToAttributes : DesignSystem.Color.Model.Model -> Typo -> List (Element.Attribute msg)
typoToAttributes colorDesignSystem typo =
    let
        size =
            typo.size
                |> Maybe.withDefault defaultSize
                |> Element.Font.size

        lineHeight =
            typo.lineHeight
                |> Maybe.withDefault defaultLineHeight
                |> (\lh -> [ Ui.Style.style "line-height" (String.fromInt lh ++ "px") ])

        wrap =
            case typo.shouldWrap of
                True ->
                    []

                False ->
                    [ Ui.Style.class "white-space-nowrap" ]
    in
    size
        :: Google.Fonts.fontToAttributes typo.font
        ++ [ alignmentxToElmUi typo.alignment
           , letterSpacingToElmUi typo.letterSpacing
           ]
        ++ colorToElmUi typo.color colorDesignSystem
        ++ List.map styleToElmUi (typo.styles |> Maybe.withDefault [])
        ++ shadowToElmUi typo.shadow colorDesignSystem
        ++ wrap



-- ++ lineHeight


selectedTypographyStyle : Int -> Model -> Maybe Typo
selectedTypographyStyle selection model =
    Dict.get selection model.typos


modelToUpdate : Int -> Model -> Typo -> Model
modelToUpdate selection model newStyles =
    { model | typos = Dict.insert selection newStyles model.typos }


allDisplayedFonts : Model -> List Google.Fonts.Font
allDisplayedFonts model =
    (Dict.toList model.typos
        |> List.map Tuple.second
    )
        |> List.map .font


view : Google.Fonts.Fonts -> Model -> DesignSystem.Color.Model.Model -> Element.Element Msg
view googleFonts model colorSystem =
    Ui.DesignSystem.view
        { sidebar = viewSidebar model colorSystem
        , mainBody =
            Dict.get model.selection model.typos
                |> Maybe.map (viewEditor googleFonts model colorSystem)
                |> Maybe.withDefault Element.none
        }



--  view sidebar


previewTypo : String -> Typo -> DesignSystem.Color.Model.Model -> Element.Element msg
previewTypo text typo colorDesignSystem =
    Element.paragraph
        (typoToAttributes colorDesignSystem typo
            ++ [ Element.width Element.fill
               , Element.height Element.fill
               ]
        )
        [ Element.text text ]


previewTypoForSidebar : String -> Typo -> DesignSystem.Color.Model.Model -> Element.Element msg
previewTypoForSidebar text typo colorDesignSystem =
    Element.el
        (typoToAttributes colorDesignSystem typo
            ++ [ Element.width Element.fill
               , Element.height Element.fill
               ]
        )
        (Element.text text)


viewSidebar : Model -> DesignSystem.Color.Model.Model -> Element.Element Msg
viewSidebar model colorDesignSystem =
    let
        rowValues : List ( Int, Typo )
        rowValues =
            Dict.toList model.typos

        rowValToSidebarRow : ( Int, Typo ) -> Element.Element Msg
        rowValToSidebarRow ( selection, typo ) =
            let
                elementReference =
                    selectionToString selection

                mayebEditableName : Element.Element Msg
                mayebEditableName =
                    case typo.isBuiltIn of
                        False ->
                            Element.el (typoToAttributes colorDesignSystem typo)
                                (Ui.Component.contenteditable
                                    { text = typo.name
                                    , placeholder = "Name your typography"
                                    , enabled = model.selection == selection
                                    }
                                    |> Element.map (UpdateName selection typo.isBuiltIn)
                                )

                        True ->
                            Element.el
                                (typoToAttributes colorDesignSystem typo
                                    ++ [ Element.width Element.fill
                                       , Element.height Element.fill
                                       ]
                                )
                                (Element.text typo.name)
            in
            Ui.DesignSystem.viewSidebarRowSimple
                { isSelected = model.selection == selection
                , label = mayebEditableName
                , msg = SelectTypo selection
                , onRemove =
                    case typo.isBuiltIn of
                        False ->
                            Just (RemoveTypo selection typo.isBuiltIn)

                        True ->
                            Nothing
                , attribs = [ Element.clip, Ui.Style.style "white-space" "nowrap" ]
                }

        addButton =
            Ui.DesignSystem.viewAddButton
                { openFieldMsg = OpenAddTypoInput
                , updateFieldMsg = UpdateAddTypoInput
                , submitMsg = SubmitAddTypoInput
                , cancelMsg = CancelAddTypoInput
                , maybeInput = model.addTypoInput
                , thingToAdd = "Typography"
                }
    in
    Ui.DesignSystem.viewSidebar
        { rows = List.map rowValToSidebarRow rowValues
        , addButton = addButton |> Just
        }



-- view Editor


viewEditor : Google.Fonts.Fonts -> Model -> DesignSystem.Color.Model.Model -> Typo -> Element.Element Msg
viewEditor googleFonts model colorEditor typo =
    Element.column
        [ Element.spacing 32
        , Element.height Element.fill
        , Element.width Element.fill
        , Element.padding 20
        ]
        [ fullWidthControls googleFonts model colorEditor typo
        , previewTypo "Lorem ipsum dolor sit amet." typo colorEditor
        ]


fullWidthControls : Google.Fonts.Fonts -> Model -> DesignSystem.Color.Model.Model -> Typo -> Element.Element Msg
fullWidthControls googleFonts model colorEditor typo =
    Element.wrappedRow
        [ Element.spacing 32
        , Element.width Element.fill
        ]
        [ viewFontEditor googleFonts typo |> layoutEditor "Font" |> Element.map (UpdateSelectedTypo model.selection)
        , viewColor model typo colorEditor |> layoutEditor "Text Color" |> Element.map (UpdateSelectedTypo model.selection)
        , viewSizeEditor model typo |> Element.map (UpdateSelectedTypo model.selection)
        , viewLineHeightEditor typo |> Element.map (UpdateSelectedTypo model.selection)
        , viewLetterSpacingEditor model typo |> Element.map (UpdateSelectedTypo model.selection)
        , viewVariantEditor model typo |> layoutEditor "Variant" |> Element.map (UpdateSelectedTypo model.selection)
        , viewAlignmentEditor model typo |> layoutEditor "Alignment" |> Element.map (UpdateSelectedTypo model.selection)
        , viewFontStyles model typo |> layoutEditor "Typo" |> Element.map (UpdateSelectedTypo model.selection)
        , viewWrap typo |> Element.map (UpdateSelectedTypo model.selection)
        , viewShadow model typo colorEditor
            |> Element.map (UpdateSelectedTypo model.selection)
        ]


viewColor : Model -> Typo -> DesignSystem.Color.Model.Model -> Element.Element ( Typo, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
viewColor model typo colorDesignSystem =
    let
        pickr =
            Ui.ColorPicker.Advanced.viewAdvanced typo.color colorDesignSystem
                |> Element.map (\{ designSystemUpdate, state } -> ( { typo | color = state }, designSystemUpdate ))
    in
    Element.row [ Element.htmlAttribute (Html.Attributes.style "z-index" "201") ] [ pickr ]


viewWrap : Typo -> Element.Element ( Typo, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
viewWrap typoStyles =
    Element.Input.checkbox [ Element.width Element.shrink ]
        { label = Element.Input.labelRight [] (Element.text "Wrap text")
        , checked = typoStyles.shouldWrap
        , onChange = identity
        , icon = Element.Input.defaultCheckbox
        }
        |> Element.map (\shouldWrap -> ( { typoStyles | shouldWrap = shouldWrap }, Nothing ))


viewShadow : Model -> Typo -> DesignSystem.Color.Model.Model -> Element.Element ( Typo, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
viewShadow model typo colorDesignSystem =
    let
        newShadow =
            { offsetX = Nothing
            , offsetY = Nothing
            , blur = Nothing
            , color = Ui.ColorPicker.Advanced.initAdvanced
            }

        typoShadow : ShadowSettings
        typoShadow =
            typo.shadow
                |> Maybe.withDefault newShadow

        inputField lab update_ value_ =
            Element.Input.text
                [ Element.Border.widthEach
                    { bottom = 3, left = 0, right = 0, top = 0 }
                , Element.width (Element.px 40)
                , Element.padding 6
                , Element.alignRight
                ]
                { onChange = update_
                , text = String.fromFloat value_
                , placeholder =
                    String.fromFloat value_
                        |> Element.text
                        |> Element.Input.placeholder []
                        |> Just
                , label = Element.Input.labelLeft [ Element.centerY ] <| Element.text lab
                }

        colorInput =
            Ui.ColorPicker.Advanced.viewAdvanced typoShadow.color colorDesignSystem
                |> Element.map (\{ designSystemUpdate, state } -> ( { typo | shadow = Just { typoShadow | color = state } }, designSystemUpdate ))

        blurInput =
            Ui.Input.smartInt "Blur" Element.Input.labelLeft typoShadow.blur (Maybe.withDefault 0 typoShadow.blur)
                |> Element.map (\blur -> ( { typo | shadow = Just { typoShadow | blur = blur } }, Nothing ))

        offsetXInput =
            Ui.Input.smartInt "X" Element.Input.labelLeft typoShadow.offsetX (Maybe.withDefault 0 typoShadow.offsetX)
                |> Element.map (\offsetX -> ( { typo | shadow = Just { typoShadow | offsetX = offsetX } }, Nothing ))

        offsetYInput =
            Ui.Input.smartInt "Y" Element.Input.labelLeft typoShadow.offsetY (Maybe.withDefault 0 typoShadow.offsetY)
                |> Element.map (\offsetY -> ( { typo | shadow = Just { typoShadow | offsetY = offsetY } }, Nothing ))

        ( offsetOptions, blurColorOptions ) =
            let
                shadowOptionsOffset =
                    [ offsetXInput, offsetYInput ]

                shadowOptionsBlurColor =
                    [ colorInput, blurInput ]
            in
            ( shadowOptionsOffset, shadowOptionsBlurColor )

        resetOption =
            case typo.shadow of
                Nothing ->
                    Element.none

                Just t ->
                    Ui.Component.iconOnClick ( { typo | shadow = Nothing }, Nothing ) Ui.Boxicons.bxReset False

        header =
            Element.row
                [ Element.height (Element.px 20) ]
                [ Element.text "Shadow"
                , Element.el [ Element.scale 0.6 ]
                    resetOption
                ]
    in
    Element.column
        [ Element.spacing 5
        , Element.Background.color Ui.Style.lightGrey
        , Element.Border.rounded 3
        , Element.padding 5
        , Element.width (Element.px 150)
        , Element.Font.size 12
        , Element.alignLeft
        ]
        [ header
        , Element.row [ Element.spacing 10 ]
            [ Element.el [ Element.alignTop ] colorInput
            , Element.el [ Element.alignTop, Element.width (Element.px 50) ] blurInput
            , Element.column [ Element.width Element.fill ]
                [ offsetXInput
                , offsetYInput
                ]
            ]
        ]


viewFontStyles : Model -> Typo -> Element.Element ( Typo, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
viewFontStyles model typo =
    let
        listOfStyles =
            case typo.styles of
                Just styleState ->
                    styleState

                _ ->
                    []

        styleButtons =
            List.map toButton [ UnderLine, StrikeThrough, Italic ]

        toIcon sty =
            case sty of
                UnderLine ->
                    Ui.Boxicons.bxUnderline

                StrikeThrough ->
                    Ui.Boxicons.bxStrikethrough

                Italic ->
                    Ui.Boxicons.bxItalic

        toButton : FontStyles -> Element.Element ( Typo, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
        toButton style_ =
            let
                isActive =
                    case listOfStyles of
                        [] ->
                            False

                        _ ->
                            if List.member style_ listOfStyles then
                                True

                            else
                                False

                update_ =
                    ( { typo | styles = Just (Help.toggleInList style_ listOfStyles) }, Nothing )
            in
            Ui.Component.iconOnClick update_ (toIcon style_) isActive
    in
    Element.row [ Element.spacingXY 5 0 ] styleButtons


viewAlignmentEditor : Model -> Typo -> Element.Element ( Typo, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
viewAlignmentEditor model typo =
    let
        alignments =
            List.map toButton [ AlignLeft, Center, AlignRight ]

        toIcon al =
            case al of
                AlignLeft ->
                    Ui.Boxicons.bxAlignLeft

                Center ->
                    Ui.Boxicons.bxAlignMiddle

                AlignRight ->
                    Ui.Boxicons.bxAlignRight

        toButton : FontAlignment -> Element.Element ( Typo, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
        toButton alignment_ =
            let
                isActive =
                    if typo.alignment == alignment_ then
                        True

                    else
                        False

                update_ =
                    ( { typo | alignment = alignment_ }, Nothing )
            in
            Ui.Component.iconOnClick update_ (toIcon alignment_) isActive
    in
    Element.row [ Element.spacingXY 5 0 ] alignments


viewWeightEditor : Model -> Typo -> Element.Element ( Typo, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
viewWeightEditor model typo =
    let
        weightToString wt =
            case wt of
                Heavy ->
                    "heavy"

                ExtraBold ->
                    "extraBold"

                Bold ->
                    "bold"

                SemiBold ->
                    "semiBold"

                Medium ->
                    "medium"

                Regular ->
                    "regular"

                Light ->
                    "light"

                ExtraLight ->
                    "extraLight"

                HairLine ->
                    "hairLine"

        rows =
            List.map toRow [ Heavy, ExtraBold, Bold, SemiBold, Medium, Regular, Light, ExtraLight, HairLine ]

        toRow weight_ =
            Ui.Dropdown.viewRow
                { isSelected =
                    if typo.weight == weight_ then
                        True

                    else
                        False
                , label = weightToString weight_ |> Ui.Dropdown.Description
                , onSelect = ( { typo | weight = weight_ }, Nothing )
                , detail = Ui.Dropdown.NoDetail
                , sideNote = Ui.Dropdown.NoDetail
                , rightHandText = Nothing
                }

        dropdown =
            Ui.Dropdown.view
                [ Ui.Style.style "margin-left" "-6px", Element.width (Element.px 100) ]
                { contents = rows
                , label = weightToString typo.weight
                }
    in
    dropdown


{-| Lazy version because Element.layout is very costly and lots of font make the rendering slow
-}
viewFontEditor =
    Element.Lazy.lazy2 viewFontEditorHelp


viewFontEditorHelp : Google.Fonts.Fonts -> Typo -> Element.Element ( Typo, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
viewFontEditorHelp googleFonts typo =
    let
        toDropDownRow font =
            Ui.Dropdown.viewRow
                { isSelected = font.family == typo.font.family
                , label = Ui.Dropdown.Description font.family
                , detail =
                    Ui.Dropdown.Description
                        (font.variants
                            |> ZipList.length
                            |> String.fromInt
                            |> (\variantAmount ->
                                    variantAmount ++ " variants"
                               )
                        )
                , onSelect =
                    ( { typo | font = font }, Nothing )
                , sideNote = Ui.Dropdown.NoDetail
                , rightHandText = Nothing
                }
    in
    case googleFonts of
        RemoteData.Failure f ->
            Element.text "could not load fonts"

        RemoteData.Loading ->
            Element.text "Loading fonts"

        RemoteData.NotAsked ->
            Element.text "Loading fonts"

        RemoteData.Success fonts ->
            -- to preview the font Google.Fonts.regularFontToAttributes typo.font
            Ui.Dropdown.view []
                { label = typo.font.family
                , contents =
                    fonts
                        -- perf improvement because google fonts returns >1000 different fonts and that slows things down
                        |> List.take 100
                        |> List.map toDropDownRow
                }


viewSizeEditor : Model -> Typo -> Element.Element ( Typo, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
viewSizeEditor model typo =
    let
        updateInput input =
            ( { typo | size = input }, Nothing )
    in
    Ui.Input.smartInt "Size" Element.Input.labelAbove typo.size (Maybe.withDefault defaultSize typo.size)
        |> Element.map updateInput
        |> Element.el [ Element.alignTop ]


viewLineHeightEditor : Typo -> Element.Element ( Typo, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
viewLineHeightEditor typo =
    let
        updateInput input =
            ( { typo | lineHeight = input }, Nothing )
    in
    Ui.Input.smartInt "Line height" Element.Input.labelAbove typo.lineHeight (Maybe.withDefault defaultLineHeight typo.lineHeight)
        |> Element.map updateInput
        |> Element.el [ Element.alignTop ]


viewLetterSpacingEditor : Model -> Typo -> Element.Element ( Typo, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
viewLetterSpacingEditor model typo =
    let
        updateInput input =
            ( { typo | letterSpacing = input }, Nothing )
    in
    Ui.Input.smartInt "Letter spacing" Element.Input.labelAbove typo.letterSpacing (Maybe.withDefault defaultLetterSpacing typo.letterSpacing)
        |> Element.map updateInput
        |> Element.el [ Element.alignTop ]


viewVariantEditor : Model -> Typo -> Element.Element ( Typo, Maybe Ui.ColorPicker.Advanced.SwatchUpdate )
viewVariantEditor model typo =
    Ui.Dropdown.view []
        { label =
            typo.font.variants
                |> ZipList.current
                |> Google.Fonts.variantToString
        , contents =
            typo.font.variants
                |> ZipList.selectedMap
                    (\isSelected variant ->
                        Ui.Dropdown.viewRow
                            { isSelected = isSelected
                            , label =
                                Google.Fonts.variantToString variant
                                    |> Ui.Dropdown.Description
                            , detail = Ui.Dropdown.NoDetail
                            , sideNote = Ui.Dropdown.NoDetail
                            , onSelect =
                                ( { typo
                                    | font =
                                        let
                                            font =
                                                typo.font
                                        in
                                        { font
                                            | variants =
                                                font.variants
                                                    |> ZipList.goToFirst
                                                        ((==) variant)
                                                    |> Maybe.withDefault font.variants
                                        }
                                  }
                                , Nothing
                                )
                            , rightHandText = Nothing
                            }
                    )
                |> ZipList.toList
        }


layoutEditor title editor =
    Element.column
        [ Element.alignTop, Element.spacing 10 ]
        [ Element.text title
        , editor
        ]



-- Update


update : Msg -> Model -> DesignSystem.Color.Model.Model -> ( ( Model, DesignSystem.Color.Model.Model ), Cmd Msg )
update msg model colorEditor =
    case msg of
        UpdateSelectedTypo selection ( newTypo, swatchUpdate ) ->
            let
                colorEditor_ =
                    case swatchUpdate of
                        Nothing ->
                            colorEditor

                        Just swatchUpdate_ ->
                            DesignSystem.Color.updateSwatch swatchUpdate_ colorEditor
            in
            ( { model | typos = Dict.insert selection newTypo model.typos }, colorEditor_ )
                |> Cmd.Extra.withNoCmd

        SelectTypo newSelection ->
            ( { model
                | selection = newSelection
              }
            , colorEditor
            )
                |> Cmd.Extra.withNoCmd

        RemoveTypo index isBuiltIn ->
            case isBuiltIn of
                False ->
                    ( { model | typos = Dict.remove index model.typos }, colorEditor )
                        |> Cmd.Extra.withNoCmd

                True ->
                    ( model, colorEditor ) |> Cmd.Extra.withNoCmd

        OpenAddTypoInput ->
            ( { model | addTypoInput = Just "" }, colorEditor )
                |> Cmd.Extra.withNoCmd

        UpdateAddTypoInput input ->
            ( { model | addTypoInput = Just input }, colorEditor )
                |> Cmd.Extra.withNoCmd

        CancelAddTypoInput ->
            ( { model | addTypoInput = Nothing }, colorEditor )
                |> Cmd.Extra.withNoCmd

        SubmitAddTypoInput ->
            case model.addTypoInput of
                Just addTypoInput_ ->
                    ( { model
                        | addTypoInput = Nothing
                        , typos =
                            Just addTypoInput_
                                |> Maybe.map
                                    (\newTypoName ->
                                        IntDict.insertNew
                                            { baseStyles
                                                | name = newTypoName
                                            }
                                            model.typos
                                    )
                                |> Maybe.withDefault model.typos
                        , selection = IntDict.nextId model.typos
                      }
                    , colorEditor
                    )
                        |> Cmd.Extra.withNoCmd

                _ ->
                    ( ( model, colorEditor ), Cmd.none )

        AddTypo ->
            ( { model
                | typos =
                    IntDict.insertNew
                        { baseStyles
                            | name =
                                "Typography-"
                                    ++ String.fromInt (IntDict.nextId model.typos)
                        }
                        model.typos
              }
            , colorEditor
            )
                |> Cmd.Extra.withNoCmd

        UpdateName id isBuiltIn name ->
            case isBuiltIn of
                False ->
                    ( { model
                        | typos =
                            Dict.update
                                id
                                (Maybe.map (\customTypo -> { customTypo | name = name }))
                                model.typos
                      }
                    , colorEditor
                    )
                        |> Cmd.Extra.withNoCmd

                True ->
                    ( model, colorEditor ) |> Cmd.Extra.withNoCmd



-- utils


selectionToString selection =
    "typo-" ++ String.fromInt selection



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeFontAlignment =
    let
        recover x =
            case x of
                "AlignLeft" ->
                    Decode.succeed AlignLeft

                "AlignRight" ->
                    Decode.succeed AlignRight

                "Center" ->
                    Decode.succeed Center

                other ->
                    Decode.fail <| "Unknown constructor for type FontAlignment: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeFontStyles =
    let
        recover x =
            case x of
                "UnderLine" ->
                    Decode.succeed UnderLine

                "StrikeThrough" ->
                    Decode.succeed StrikeThrough

                "Italic" ->
                    Decode.succeed Italic

                other ->
                    Decode.fail <| "Unknown constructor for type FontStyles: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeModel =
    Decode.map3
        Model
        (Decode.field "typos" (IntDict.decodeDict decodeTypo))
        (Decode.field "selection" Decode.int)
        (Decode.field "addTypoInput" (Decode.maybe Decode.string))


decodeShadowSettings =
    Decode.map4
        ShadowSettings
        (Decode.field "offsetX" (Decode.maybe Decode.int))
        (Decode.field "offsetY" (Decode.maybe Decode.int))
        (Decode.field "blur" (Decode.maybe Decode.int))
        (Decode.field "color" Ui.ColorPicker.Advanced.decodeAdvancedState)


decodeTypo =
    Decode.succeed
        Typo
        |> Extra.andMap (Decode.field "name" Decode.string)
        |> Extra.andMap (Decode.field "font" Google.Fonts.decodeFont)
        |> Extra.andMap (Decode.field "size" (Decode.maybe Decode.int))
        |> Extra.andMap (Decode.field "lineHeight" (Decode.maybe Decode.int))
        |> Extra.andMap (Decode.field "letterSpacing" (Decode.maybe Decode.int))
        |> Extra.andMap (Decode.field "alignment" decodeFontAlignment)
        |> Extra.andMap (Decode.field "styles" (Decode.maybe (Decode.list decodeFontStyles)))
        |> Extra.andMap (Decode.field "color" Ui.ColorPicker.Advanced.decodeAdvancedState)
        |> Extra.andMap (Decode.field "shadow" (Decode.maybe decodeShadowSettings))
        |> Extra.andMap (Decode.field "weight" decodeWeight)
        |> Extra.andMap (Decode.field "shouldWrap" Decode.bool)
        |> Extra.andMap (Decode.field "isBuiltIn" Decode.bool)


decodeTypoTabV2 =
    let
        recover x =
            case x of
                "Box" ->
                    Decode.succeed Box

                "Placeholder" ->
                    Decode.succeed Placeholder

                other ->
                    Decode.fail <| "Unknown constructor for type TypoTabV2: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeWeight =
    let
        recover x =
            case x of
                "Heavy" ->
                    Decode.succeed Heavy

                "ExtraBold" ->
                    Decode.succeed ExtraBold

                "Bold" ->
                    Decode.succeed Bold

                "SemiBold" ->
                    Decode.succeed SemiBold

                "Medium" ->
                    Decode.succeed Medium

                "Regular" ->
                    Decode.succeed Regular

                "Light" ->
                    Decode.succeed Light

                "ExtraLight" ->
                    Decode.succeed ExtraLight

                "HairLine" ->
                    Decode.succeed HairLine

                other ->
                    Decode.fail <| "Unknown constructor for type Weight: " ++ other
    in
    Decode.string |> Decode.andThen recover


encodeFontAlignment a =
    case a of
        AlignLeft ->
            Encode.string "AlignLeft"

        AlignRight ->
            Encode.string "AlignRight"

        Center ->
            Encode.string "Center"


encodeFontStyles a =
    case a of
        UnderLine ->
            Encode.string "UnderLine"

        StrikeThrough ->
            Encode.string "StrikeThrough"

        Italic ->
            Encode.string "Italic"


encodeMaybeInt a =
    case a of
        Just b ->
            Encode.int b

        Nothing ->
            Encode.null


encodeMaybeShadowSettings a =
    case a of
        Just b ->
            encodeShadowSettings b

        Nothing ->
            Encode.null


encodeMaybeString a =
    case a of
        Just b ->
            Encode.string b

        Nothing ->
            Encode.null


encodeMaybe_ListFontStyles_ a =
    case a of
        Just b ->
            Encode.list encodeFontStyles b

        Nothing ->
            Encode.null


encodeModel a =
    Encode.object
        [ ( "typos", IntDict.encodeDict encodeTypo a.typos )
        , ( "selection", Encode.int a.selection )
        , ( "addTypoInput", encodeMaybeString a.addTypoInput )
        ]


encodeShadowSettings a =
    Encode.object
        [ ( "offsetX", encodeMaybeInt a.offsetX )
        , ( "offsetY", encodeMaybeInt a.offsetY )
        , ( "blur", encodeMaybeInt a.blur )
        , ( "color", Ui.ColorPicker.Advanced.encodeAdvancedState a.color )
        ]


encodeTypo a =
    Encode.object
        [ ( "name", Encode.string a.name )
        , ( "font", Google.Fonts.encodeFont a.font )
        , ( "size", encodeMaybeInt a.size )
        , ( "lineHeight", encodeMaybeInt a.lineHeight )
        , ( "letterSpacing", encodeMaybeInt a.letterSpacing )
        , ( "alignment", encodeFontAlignment a.alignment )
        , ( "styles", encodeMaybe_ListFontStyles_ a.styles )
        , ( "color", Ui.ColorPicker.Advanced.encodeAdvancedState a.color )
        , ( "shadow", encodeMaybeShadowSettings a.shadow )
        , ( "weight", encodeWeight a.weight )
        , ( "shouldWrap", Encode.bool a.shouldWrap )
        , ( "isBuiltIn", Encode.bool a.isBuiltIn )
        ]


encodeTypoTabV2 a =
    case a of
        Box ->
            Encode.string "Box"

        Placeholder ->
            Encode.string "Placeholder"


encodeWeight a =
    case a of
        Heavy ->
            Encode.string "Heavy"

        ExtraBold ->
            Encode.string "ExtraBold"

        Bold ->
            Encode.string "Bold"

        SemiBold ->
            Encode.string "SemiBold"

        Medium ->
            Encode.string "Medium"

        Regular ->
            Encode.string "Regular"

        Light ->
            Encode.string "Light"

        ExtraLight ->
            Encode.string "ExtraLight"

        HairLine ->
            Encode.string "HairLine"



-- [generator-end]
