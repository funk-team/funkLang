module DesignSystem.Shadow exposing (..)

import Cmd.Extra
import Color
import Color.Extra
import DesignSystem.Color
import DesignSystem.Color.Model
import DesignSystem.Color.Selection
import Dict
import Element
import Element.Border
import Element.Input
import Hover
import IntDict
import Json.Decode as Decode
import Json.Encode as Encode
import Ui.ColorPicker.Advanced
import Ui.Component
import Ui.DesignSystem
import Ui.Input
import Ui.RadioRow


type alias HoveredElements =
    Hover.HoveredElements



-- [generator-start]


type alias Model =
    { shadows : IntDict.Dict Shadow
    , selection : Int
    , addShadowInput : Maybe String
    , hoveredElements : HoveredElements
    }


type alias Shadow =
    { offset : ( Maybe Int, Maybe Int )
    , size : Maybe Int
    , blur : Maybe Int
    , color : Ui.ColorPicker.Advanced.AdvancedState
    , type_ : Type
    , name : String
    , isBuiltIn : Bool
    }


type Type
    = OuterShadow
    | InnerShadow


init : Model
init =
    { shadows =
        IntDict.empty
            |> IntDict.insertNew
                { offset = ( Just 0, Just 0 )
                , size = Just 2
                , blur = Just 5
                , color =
                    Ui.ColorPicker.Advanced.initAdvancedWithCustomColor
                        (Color.rgba 0 0 0 0.1)
                , type_ = OuterShadow
                , name = "Outer"
                , isBuiltIn = True
                }
            |> IntDict.insertNew
                { offset = ( Just 0, Just 0 )
                , size = Just 2
                , blur = Just 5
                , color =
                    Ui.ColorPicker.Advanced.initAdvancedWithCustomColor
                        (Color.rgba 0 0 0 0.06)
                , type_ = InnerShadow
                , name = "Inner"
                , isBuiltIn = True
                }
    , selection = 1
    , addShadowInput = Nothing
    , hoveredElements = []
    }


shadowToAttr : DesignSystem.Color.Model.Model -> Shadow -> List (Element.Attribute msg)
shadowToAttr colorModel { offset, size, blur, color, type_ } =
    let
        function =
            case type_ of
                OuterShadow ->
                    Element.Border.shadow

                InnerShadow ->
                    Element.Border.innerShadow

        maybeColor =
            DesignSystem.Color.Selection.getSelectedColor
                colorModel
                color.selection
                |> Maybe.map Color.Extra.toElmUi

        config =
            let
                ( x, y ) =
                    offset
            in
            { offset = ( Maybe.withDefault defaultOffsetX x |> toFloat, Maybe.withDefault defaultOffsetY y |> toFloat )
            , size = Maybe.withDefault defaultSize size |> toFloat
            , blur = Maybe.withDefault defaultBlur blur |> toFloat
            , color = Element.rgb 0 0 0
            }
    in
    case maybeColor of
        Just color_ ->
            [ function { config | color = color_ } ]

        Nothing ->
            []


initShadow name =
    { offset = ( Nothing, Nothing )
    , size = Nothing
    , blur = Nothing
    , color = Ui.ColorPicker.Advanced.initAdvanced
    , type_ = OuterShadow
    , name = name
    , isBuiltIn = False
    }


defaultOffsetX =
    0


defaultOffsetY =
    0


defaultSize =
    0


defaultBlur =
    5



-- view


view : Model -> DesignSystem.Color.Model.Model -> Element.Element ( Msg, DesignSystem.Color.Model.Model )
view model colorsModel =
    Ui.DesignSystem.view
        { sidebar =
            viewSidebar model colorsModel
                |> Element.map (\a -> ( a, colorsModel ))
        , mainBody =
            mainContent model colorsModel
        }


viewSidebar : Model -> DesignSystem.Color.Model.Model -> Element.Element Msg
viewSidebar model colorModel =
    let
        preview shadow =
            Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 80)
                ]
                (Element.el
                    ([ Element.centerX
                     , Element.centerY
                     , Element.width (Element.px 170)
                     , Element.height (Element.px 40)
                     ]
                        ++ shadowToAttr colorModel shadow
                    )
                    Element.none
                )

        viewSidebarRow ( id, shadow ) =
            let
                elementReference =
                    "shadow.viewSidebar:id-"
                        ++ String.fromInt id

                mayebEditableName =
                    Ui.Component.contenteditable
                        { text = shadow.name
                        , placeholder = "Name your Shadow"
                        , enabled = model.selection == id && not shadow.isBuiltIn
                        }
                        |> Element.map (UpdateName id)
            in
            Ui.DesignSystem.viewSidebarRow
                { isSelected = model.selection == id
                , isHovered =
                    Hover.isHovered
                        model.hoveredElements
                        elementReference
                , title = mayebEditableName
                , preview = preview shadow
                , onSelect = UpdateSelection id
                , onRemove =
                    case shadow.isBuiltIn of
                        True ->
                            Nothing

                        False ->
                            Just (RemoveShadow id)
                , elementRef = elementReference
                , onHover = HoverMsg
                , customAttribs = []
                }
    in
    Ui.DesignSystem.viewSidebar
        { rows =
            model.shadows
                |> Dict.toList
                |> List.map viewSidebarRow
        , addButton =
            Ui.DesignSystem.viewAddButton
                { openFieldMsg = OpenAddShadowField
                , updateFieldMsg = UpdateAddShadowInput
                , submitMsg = RequestNewShadow
                , cancelMsg = CancelNewShadow
                , maybeInput = model.addShadowInput
                , thingToAdd = "Shadow"
                }
                |> Just
        }


mainContent : Model -> DesignSystem.Color.Model.Model -> Element.Element ( Msg, DesignSystem.Color.Model.Model )
mainContent model colorModel =
    case Dict.get model.selection model.shadows of
        Nothing ->
            Element.none

        Just shadow ->
            Element.column
                [ Element.padding 20
                , Element.spacing 50
                ]
                [ Element.wrappedRow
                    [ Element.spacing 30 ]
                    [ viewOffsetEditor shadow
                        |> Element.map (\a -> ( UpdateSelectedShadow a, colorModel ))
                    , viewSizeEditor shadow
                        |> Element.map (\a -> ( UpdateSelectedShadow a, colorModel ))
                    , viewBlurEditor shadow
                        |> Element.map (\a -> ( UpdateSelectedShadow a, colorModel ))
                    , viewColorEditor colorModel shadow
                        |> Element.map (Tuple.mapFirst UpdateSelectedShadow)
                    , viewTypeEditor shadow
                        |> Element.map (\a -> ( UpdateSelectedShadow a, colorModel ))
                    ]
                , viewPreview colorModel shadow
                ]


viewOffsetEditor : Shadow -> Element.Element Shadow
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
        [ Element.spacing 10
        , Element.alignTop
        ]
        [ Element.el
            []
            (Element.text "Offset")
        , Element.row
            [ Element.spacing 15 ]
            [ Element.el
                [ Element.width (Element.minimum 40 Element.shrink) ]
                (Ui.Input.smartInt "X" Element.Input.labelLeft x (Maybe.withDefault defaultOffsetX x)
                    |> Element.map updateXInput
                )
            , Element.el
                [ Element.width (Element.minimum 40 Element.shrink) ]
                (Ui.Input.smartInt "Y" Element.Input.labelLeft y (Maybe.withDefault defaultOffsetY y)
                    |> Element.map updateYInput
                )
            ]
        ]


viewSizeEditor : Shadow -> Element.Element Shadow
viewSizeEditor shadow =
    let
        updateSizeInput input =
            { shadow | size = input }
    in
    Element.el
        [ Element.alignTop ]
        (Ui.Input.smartInt "Size" Element.Input.labelAbove shadow.size (Maybe.withDefault defaultSize shadow.size)
            |> Element.map updateSizeInput
        )


viewBlurEditor : Shadow -> Element.Element Shadow
viewBlurEditor shadow =
    let
        updateBlurInput input =
            { shadow | blur = input }
    in
    Element.el
        [ Element.alignTop ]
        (Ui.Input.smartInt "Blur" Element.Input.labelAbove shadow.blur (Maybe.withDefault defaultBlur shadow.blur)
            |> Element.map updateBlurInput
        )


viewColorEditor : DesignSystem.Color.Model.Model -> Shadow -> Element.Element ( Shadow, DesignSystem.Color.Model.Model )
viewColorEditor colorModel shadow =
    let
        updateColorInput { designSystemUpdate, state } =
            let
                updatedColorModel =
                    case designSystemUpdate of
                        Nothing ->
                            colorModel

                        Just swatchUpdate ->
                            DesignSystem.Color.updateSwatch swatchUpdate colorModel
            in
            ( { shadow | color = state }, updatedColorModel )
    in
    Element.column
        [ Element.spacing 10
        , Element.alignTop
        ]
        [ Element.el
            []
            (Element.text "Color")
        , Ui.ColorPicker.Advanced.viewAdvanced shadow.color colorModel
            |> Element.map updateColorInput
        ]


viewTypeEditor : Shadow -> Element.Element Shadow
viewTypeEditor shadow =
    let
        updateTypeInput input =
            { shadow | type_ = input }

        radioRowParams =
            { items = [ OuterShadow, InnerShadow ]
            , toLabel =
                \a ->
                    case a of
                        OuterShadow ->
                            "OuterShadow"

                        InnerShadow ->
                            "InnerShadow"
            , selected = (==) shadow.type_
            }
    in
    Element.column
        [ Element.spacing 8
        , Element.alignTop
        ]
        [ Element.el
            []
            (Element.text "Type")
        , Ui.RadioRow.view radioRowParams
            |> Element.map updateTypeInput
        ]


viewPreview colorModel shadow =
    Element.el
        ([ Element.width (Element.px 300)
         , Element.height (Element.px 60)
         ]
            ++ shadowToAttr colorModel shadow
        )
        Element.none


isBuiltInShadow : Int -> IntDict.Dict Shadow -> Bool
isBuiltInShadow id shadows =
    case Dict.get id shadows of
        Nothing ->
            False

        Just shadow ->
            shadow.isBuiltIn



-- update


type Msg
    = UpdateSelection Int
    | OpenAddShadowField
    | UpdateAddShadowInput String
    | RequestNewShadow
    | CancelNewShadow
    | UpdateSelectedShadow Shadow
    | RemoveShadow Int
    | HoverMsg Hover.Msg
    | UpdateShadow Int Shadow
    | UpdateName Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSelection newId ->
            ( { model | selection = newId }, Cmd.none )

        RemoveShadow idToRm ->
            ( { model | shadows = Dict.remove idToRm model.shadows }, Cmd.none )

        OpenAddShadowField ->
            ( { model | addShadowInput = Just "" }, Cmd.none )

        UpdateAddShadowInput inp ->
            ( { model | addShadowInput = Just inp }, Cmd.none )

        CancelNewShadow ->
            ( { model | addShadowInput = Nothing }, Cmd.none )

        RequestNewShadow ->
            case model.addShadowInput of
                Just addShadowInput_ ->
                    ( { model
                        | addShadowInput = Nothing
                        , shadows =
                            case model.addShadowInput of
                                Just name ->
                                    IntDict.insertNew (initShadow name) model.shadows

                                Nothing ->
                                    model.shadows
                        , selection = IntDict.nextId model.shadows
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        UpdateSelectedShadow newShadow ->
            ( { model
                | shadows =
                    Dict.update
                        model.selection
                        (always (Just newShadow))
                        model.shadows
              }
            , Cmd.none
            )

        UpdateShadow id newShadow ->
            ( { model
                | shadows =
                    Dict.update
                        id
                        (always (Just newShadow))
                        model.shadows
              }
            , Cmd.none
            )

        HoverMsg hoverMsg ->
            let
                ( newHoveredElements, cmd ) =
                    Hover.update hoverMsg model.hoveredElements
            in
            { model | hoveredElements = newHoveredElements }
                |> Cmd.Extra.withCmd (Cmd.map HoverMsg cmd)

        UpdateName id name ->
            ( { model
                | shadows =
                    Dict.update
                        id
                        (Maybe.map (\customShadow -> { customShadow | name = name }))
                        model.shadows
              }
            , Cmd.none
            )



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeModel =
    Decode.map4
        Model
        (Decode.field "shadows" (IntDict.decodeDict decodeShadow))
        (Decode.field "selection" Decode.int)
        (Decode.field "addShadowInput" (Decode.maybe Decode.string))
        (Decode.field "hoveredElements" decodeHoveredElements)


decodeShadow =
    Decode.map7
        Shadow
        (Decode.field "offset" decodeTuple_MaybeInt_MaybeInt_)
        (Decode.field "size" (Decode.maybe Decode.int))
        (Decode.field "blur" (Decode.maybe Decode.int))
        (Decode.field "color" Ui.ColorPicker.Advanced.decodeAdvancedState)
        (Decode.field "type_" decodeType)
        (Decode.field "name" Decode.string)
        (Decode.field "isBuiltIn" Decode.bool)


decodeTuple_MaybeInt_MaybeInt_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" (Decode.maybe Decode.int))
        (Decode.field "A2" (Decode.maybe Decode.int))


decodeType =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeTypeHelp


decodeTypeHelp constructor =
    case constructor of
        "OuterShadow" ->
            Decode.succeed OuterShadow

        "InnerShadow" ->
            Decode.succeed InnerShadow

        other ->
            Decode.fail <| "Unknown constructor for type Type: " ++ other


encodeMaybeInt a =
    case a of
        Just b ->
            Encode.int b

        Nothing ->
            Encode.null


encodeMaybeString a =
    case a of
        Just b ->
            Encode.string b

        Nothing ->
            Encode.null


encodeModel a =
    Encode.object
        [ ( "shadows", IntDict.encodeDict encodeShadow a.shadows )
        , ( "selection", Encode.int a.selection )
        , ( "addShadowInput", encodeMaybeString a.addShadowInput )
        , ( "hoveredElements", encodeHoveredElements a.hoveredElements )
        ]


encodeShadow a =
    Encode.object
        [ ( "offset", encodeTuple_MaybeInt_MaybeInt_ a.offset )
        , ( "size", encodeMaybeInt a.size )
        , ( "blur", encodeMaybeInt a.blur )
        , ( "color", Ui.ColorPicker.Advanced.encodeAdvancedState a.color )
        , ( "type_", encodeType a.type_ )
        , ( "name", Encode.string a.name )
        , ( "isBuiltIn", Encode.bool a.isBuiltIn )
        ]


encodeTuple_MaybeInt_MaybeInt_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", encodeMaybeInt a1 )
        , ( "A2", encodeMaybeInt a2 )
        ]


encodeType a =
    case a of
        OuterShadow ->
            Encode.object
                [ ( "Constructor", Encode.string "OuterShadow" )
                ]

        InnerShadow ->
            Encode.object
                [ ( "Constructor", Encode.string "InnerShadow" )
                ]



-- [generator-end]


encodeHoveredElements _ =
    Encode.null


decodeHoveredElements =
    Decode.succeed []
