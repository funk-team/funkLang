module DesignSystem.Color exposing (..)

{-| Use this module to render the color editor menu and to wor with colors in general.
-}

import Cmd.Extra
import Color
import Color.Extra
import DesignSystem.Color.Model
import Dict
import Element
import Element.Background
import Element.Border
import Hover
import IntDict
import Json.Decode as Decode
import Json.Encode as Encode
import Ui.ColorPicker.Advanced
import Ui.ColorPicker.Basic
import Ui.Component
import Ui.DesignSystem
import Ui.Style



-- interesting code


init : DesignSystem.Color.Model.Model
init =
    { others = IntDict.empty
    , background =
        { label = "Background"
        , value = white
        , pickerState = Ui.ColorPicker.Basic.init white
        }
    , text =
        { label = "Text"
        , value = offblack
        , pickerState = Ui.ColorPicker.Basic.init offblack
        }
    , selection = DesignSystem.Color.Model.TextSwatchSelected
    , hoveredElements = []
    , addSwatchInput = Nothing
    }


offblack =
    Color.rgb 0.2 0.2 0.2



-- update


type Msg
    = Update DesignSystem.Color.Model.Model
      -- | AddColor
    | RemoveSwatch Int
    | SelectSwatch DesignSystem.Color.Model.Selection
    | HoverMsg Hover.Msg
    | OpenAddSwatchInput
    | UpdateAddSwatchInput String
    | SubmitAddSwatchInput
    | CancelAddSwatchInput
    | UpdateName Int String


update : Msg -> DesignSystem.Color.Model.Model -> ( DesignSystem.Color.Model.Model, Cmd Msg )
update msg model =
    case msg of
        Update m ->
            ( m, Cmd.none )

        -- AddColor ->
        -- let
        --     new =
        --         { label = "Some Color :)"
        --         , value = Color.rgb 0.8 0.8 0.8
        --         , deletable = True
        --         , pickerState = Ui.ColorPicker.Basic.empty
        --         }
        -- in
        -- ( { model | others = model.others ++ [ new ] }, Cmd.none )
        RemoveSwatch index ->
            { model | others = Dict.remove index model.others }
                |> Cmd.Extra.withNoCmd

        SelectSwatch selection ->
            { model | selection = selection }
                |> Cmd.Extra.withNoCmd

        HoverMsg hoverMsg ->
            let
                ( newHoveredElements, cmd ) =
                    Hover.update hoverMsg model.hoveredElements
            in
            { model | hoveredElements = newHoveredElements }
                |> Cmd.Extra.withCmd (Cmd.map HoverMsg cmd)

        OpenAddSwatchInput ->
            { model | addSwatchInput = Just "" }
                |> Cmd.Extra.withNoCmd

        UpdateAddSwatchInput inp ->
            { model | addSwatchInput = Just inp }
                |> Cmd.Extra.withNoCmd

        CancelAddSwatchInput ->
            { model | addSwatchInput = Nothing }
                |> Cmd.Extra.withNoCmd

        UpdateName id name ->
            { model
                | others =
                    Dict.update
                        id
                        (Maybe.map (\customColor -> { customColor | label = name }))
                        model.others
            }
                |> Cmd.Extra.withNoCmd

        SubmitAddSwatchInput ->
            case model.addSwatchInput of
                Just addSwatchInput_ ->
                    let
                        new =
                            { label = addSwatchInput_
                            , value = offwhite
                            , pickerState = Ui.ColorPicker.Basic.init offwhite
                            }

                        updatedOthers =
                            IntDict.insertNew new model.others
                    in
                    { model
                        | others = updatedOthers
                        , addSwatchInput = Nothing
                        , selection =
                            updatedOthers
                                |> Dict.toList
                                |> List.map Tuple.first
                                |> List.maximum
                                |> Maybe.map DesignSystem.Color.Model.OtherSwatchSelected
                                |> Maybe.withDefault
                                    model.selection
                    }
                        |> Cmd.Extra.withNoCmd

                _ ->
                    model |> Cmd.Extra.withNoCmd


offwhite =
    Color.rgb 0.8 0.8 0.8


updateSwatch : Ui.ColorPicker.Advanced.SwatchUpdate -> DesignSystem.Color.Model.Model -> DesignSystem.Color.Model.Model
updateSwatch swatchUpdate model =
    case swatchUpdate.reference of
        DesignSystem.Color.Model.TextSwatchSelected ->
            { model | text = setValue swatchUpdate.value model.text }

        DesignSystem.Color.Model.BackgroundSwatchSelected ->
            { model | background = setValue swatchUpdate.value model.background }

        DesignSystem.Color.Model.OtherSwatchSelected key ->
            { model
                | others =
                    Dict.update
                        key
                        (\swatch ->
                            case swatch of
                                Just swatch_ ->
                                    Just <| setValue swatchUpdate.value swatch_

                                Nothing ->
                                    Just
                                        { label = "Color " ++ String.fromInt key
                                        , value = swatchUpdate.value
                                        , pickerState = Ui.ColorPicker.Basic.init swatchUpdate.value
                                        }
                        )
                        model.others
            }


setValue : Color.Color -> DesignSystem.Color.Model.Swatch -> DesignSystem.Color.Model.Swatch
setValue value swatch =
    { swatch | value = value }



---- VIEW ----


view : DesignSystem.Color.Model.Model -> Element.Element Msg
view model =
    Ui.DesignSystem.view
        { sidebar = viewSidebar model
        , mainBody = viewMainContent model
        }


preview swatch =
    Element.el
        [ swatch.value
            |> Color.Extra.toElmUi
            |> Element.Background.color
        , Element.height (Element.px 50)
        , Element.width Element.fill
        , Element.Border.rounded 5
        , Ui.Style.shadowMedium
        ]
        Element.none


picker swatch =
    Ui.ColorPicker.Advanced.view ( swatch.value, swatch.pickerState )
        |> Element.map
            (\( value, pickerState ) ->
                { swatch
                    | pickerState = pickerState
                    , value = value
                }
            )


viewSidebar : DesignSystem.Color.Model.Model -> Element.Element Msg
viewSidebar model =
    Ui.DesignSystem.viewSidebar
        { rows =
            model.others
                |> Dict.toList
                |> List.map (Tuple.mapFirst DesignSystem.Color.Model.OtherSwatchSelected)
                |> (::) ( DesignSystem.Color.Model.BackgroundSwatchSelected, model.background )
                |> (::) ( DesignSystem.Color.Model.TextSwatchSelected, model.text )
                |> List.map
                    (\( selection, swatch ) ->
                        let
                            elementReference =
                                "Color.viewSidebar:"
                                    ++ selectionToString selection

                            mayebEditableName =
                                case selection of
                                    DesignSystem.Color.Model.OtherSwatchSelected index ->
                                        Ui.Component.contenteditable
                                            { text = swatch.label
                                            , placeholder = "Name your color "
                                            , enabled = model.selection == selection
                                            }
                                            |> Element.map (UpdateName index)

                                    _ ->
                                        Element.el
                                            [ Element.width Element.fill
                                            , Element.height Element.fill
                                            ]
                                            (Element.text swatch.label)
                        in
                        Ui.DesignSystem.viewSidebarRow
                            { isSelected = model.selection == selection
                            , isHovered =
                                Hover.isHovered
                                    model.hoveredElements
                                    elementReference
                            , title = mayebEditableName
                            , preview = preview swatch
                            , onSelect = SelectSwatch selection
                            , onRemove =
                                case selection of
                                    DesignSystem.Color.Model.OtherSwatchSelected index ->
                                        Just (RemoveSwatch index)

                                    _ ->
                                        Nothing
                            , elementRef = elementReference
                            , onHover = HoverMsg
                            , customAttribs = []
                            }
                    )
        , addButton =
            Ui.DesignSystem.viewAddButton
                { openFieldMsg = OpenAddSwatchInput
                , updateFieldMsg = UpdateAddSwatchInput
                , submitMsg = SubmitAddSwatchInput
                , cancelMsg = CancelAddSwatchInput
                , maybeInput = model.addSwatchInput
                , thingToAdd = "color"
                }
                |> Just
        }


viewMainContent : DesignSystem.Color.Model.Model -> Element.Element Msg
viewMainContent model =
    Element.el [ Element.padding 20 ] <|
        case model.selection of
            DesignSystem.Color.Model.TextSwatchSelected ->
                picker model.text
                    |> Element.map
                        (\newSwatch ->
                            { model | text = newSwatch }
                                |> Update
                        )

            DesignSystem.Color.Model.BackgroundSwatchSelected ->
                picker model.background
                    |> Element.map
                        (\newSwatch ->
                            { model | background = newSwatch }
                                |> Update
                        )

            DesignSystem.Color.Model.OtherSwatchSelected index ->
                model.others
                    |> Dict.get index
                    |> Maybe.map picker
                    |> Maybe.withDefault Element.none
                    |> Element.map
                        (\newSwatch ->
                            { model
                                | others =
                                    model.others
                                        |> Dict.insert index newSwatch
                            }
                                |> Update
                        )



-- encoders & decoders


encodePickerState _ =
    Encode.null


selectionToString selection =
    case selection of
        DesignSystem.Color.Model.TextSwatchSelected ->
            "TextSwatchSelected"

        DesignSystem.Color.Model.BackgroundSwatchSelected ->
            "BackgroundSwatchSelected"

        DesignSystem.Color.Model.OtherSwatchSelected index ->
            "OtherSwatchSelected-" ++ String.fromInt index


encodeSelection _ =
    Encode.null


decodeSelection =
    Decode.succeed DesignSystem.Color.Model.TextSwatchSelected


decodeModel =
    Decode.succeed init


decodeSwatch =
    Decode.succeed
        { label = "tmpDecode"
        , value = white
        , pickerState = Ui.ColorPicker.Basic.init white
        }


white =
    Color.rgb 1 1 1


encodeModel a =
    Encode.null


encodeSwatch a =
    Encode.null
