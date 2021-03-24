module Ui.ColorPicker.Advanced exposing
    ( AdvancedState
    , Return
    , SwatchUpdate
    , Visibility(..)
    , decodeAdvancedState
    , decodeSwatchUpdate
    , encodeAdvancedState
    , encodeSwatchUpdate
    , initAdvanced
    , initAdvancedWithCustomColor
    , onMouseDownOpenOrClose
    , renderPosition
    , view
    , viewAdvanced
    )

{-| Provide functions for rendering color pickers
-}

import Color
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
import Html.Attributes
import IntDict
import Json.Decode as Decode
import Json.Encode as Encode
import Ui.Boxicons
import Ui.ColorPicker.Basic
import Ui.ColorPicker.Help
import Ui.Component
import Ui.Style



---- DATA STRUCTURES ----


type alias AdvancedState =
    { selection : DesignSystem.Color.Selection.Selection
    , pickerState : Ui.ColorPicker.Basic.State
    , visibility : Visibility
    }


type Visibility
    = Closed
    | Open { top : Float, right : Float }


type alias Return =
    { designSystemUpdate : Maybe SwatchUpdate
    , state : AdvancedState
    }


type alias SwatchUpdate =
    { reference : DesignSystem.Color.Model.Selection
    , value : Color.Color
    }



-- INIT


black =
    Color.rgba 0 0 0 1


initAdvanced : AdvancedState
initAdvanced =
    AdvancedState
        (DesignSystem.Color.Selection.Standalone black)
        (Ui.ColorPicker.Basic.init black)
        Closed


initAdvancedWithCustomColor : Color.Color -> AdvancedState
initAdvancedWithCustomColor color =
    AdvancedState
        (DesignSystem.Color.Selection.Standalone color)
        (Ui.ColorPicker.Basic.init color)
        Closed



---- VIEW ----


{-| Render a simple color picker
-}
view :
    ( Color.Color, Ui.ColorPicker.Basic.State )
    -> Element.Element ( Color.Color, Ui.ColorPicker.Basic.State )
view ( value, state ) =
    Ui.ColorPicker.Basic.view value state
        |> Element.map
            (\msg ->
                Ui.ColorPicker.Basic.update
                    msg
                    value
                    state
                    |> (\( newState, maybeColor ) -> ( Maybe.withDefault value maybeColor, newState ))
            )


viewAdvanced : AdvancedState -> DesignSystem.Color.Model.Model -> Element.Element Return
viewAdvanced ({ selection, pickerState, visibility } as state) designSystemColors =
    let
        retrievedColor =
            case selection of
                DesignSystem.Color.Selection.FromSystem colorSelection ->
                    DesignSystem.Color.Model.getSelectedColor colorSelection designSystemColors
                        |> Maybe.withDefault (Color.rgba 0 0 0 0)

                DesignSystem.Color.Selection.Standalone color_ ->
                    color_

        swatches : Element.Element Return
        swatches =
            let
                otherSwatches =
                    Dict.toList designSystemColors.others
                        |> List.map (Tuple.mapFirst DesignSystem.Color.Model.OtherSwatchSelected)

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

                swatchRows =
                    Element.wrappedRow
                        [ Element.spacing 10 ]
                        allSwatches
                        |> Element.map
                            (\sel ->
                                { state
                                    | selection = DesignSystem.Color.Selection.FromSystem sel
                                }
                            )

                swatchHeader =
                    Element.row [ Element.spacing 5, Element.height (Element.px 10) ]
                        [ Element.text "Design System"
                        , unlink
                        ]

                unlink =
                    case selection of
                        DesignSystem.Color.Selection.FromSystem _ ->
                            Element.el
                                [ Element.Events.onClick (Return Nothing { state | selection = DesignSystem.Color.Selection.Standalone retrievedColor })
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

                                newState =
                                    { state | selection = DesignSystem.Color.Selection.FromSystem (DesignSystem.Color.Model.OtherSwatchSelected id) }

                                designSystemUpdate : Maybe SwatchUpdate
                                designSystemUpdate =
                                    Just (SwatchUpdate (DesignSystem.Color.Model.OtherSwatchSelected id) retrievedColor)
                            in
                            Element.el
                                [ Element.Events.onClick
                                    (Return designSystemUpdate newState)
                                , Element.Font.color Ui.Style.highlightColorSolid
                                , Element.pointer
                                , Element.mouseOver [ Element.Background.color Ui.Style.slightAccent ]
                                , Element.htmlAttribute (Html.Attributes.title "Add to design system")
                                , Element.Border.rounded 2
                                ]
                                (Element.el [ Element.scale 0.75 ] <| Ui.Component.icon Ui.Boxicons.bxPlus)
            in
            Element.column [ Element.padding 10, Element.spacing 10 ]
                [ swatchHeader
                , swatchRows
                    |> Element.map (Return Nothing)
                ]

        renderedPicker : Element.Element Return
        renderedPicker =
            view ( retrievedColor, pickerState )
                |> Element.map processNewPickedColor

        {- If the color is linked to the design system, update the design system color -}
        processNewPickedColor : ( Color.Color, Ui.ColorPicker.Basic.State ) -> Return
        processNewPickedColor ( newColor, newPickerState ) =
            case selection of
                DesignSystem.Color.Selection.Standalone value ->
                    Return Nothing
                        { state
                            | selection = DesignSystem.Color.Selection.Standalone newColor
                            , pickerState = newPickerState
                        }

                DesignSystem.Color.Selection.FromSystem key ->
                    Return
                        (Just <| SwatchUpdate key newColor)
                        { state
                            | pickerState = newPickerState
                        }

        {- The contents of the popout -}
        pickerAndSwatches =
            Element.onLeft <|
                case visibility of
                    Open position ->
                        Element.column
                            ([ Element.Background.color Ui.Style.white
                             , Element.Border.rounded 2
                             , Ui.Style.shadowMedium
                             , Element.moveLeft 2
                             , Element.moveUp 1
                             , Element.Border.width 1
                             , Element.Border.color Ui.Style.grey
                             , onMouseDownOutsideClose
                                |> Element.mapAttribute (\newVisbility -> Return Nothing { state | visibility = newVisbility })
                             ]
                                ++ renderPosition position
                            )
                            [ renderedPicker
                            , swatches
                            ]

                    Closed ->
                        Element.none

        {- The little area that previews the color and shows the popout on click -}
        thumbnail =
            Element.el
                (pickerAndSwatches :: thumbnailAttribs)
            <|
                Element.el
                    [ onMouseDownOpenOrClose state.visibility
                        |> Element.mapAttribute (\newVisbility -> Return Nothing { state | visibility = newVisbility })
                    , Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    Element.none

        thumbnailAttribs : List (Element.Attribute Return)
        thumbnailAttribs =
            Element.Background.color (Color.Extra.toElmUi retrievedColor)
                :: Ui.ColorPicker.Help.thumbnailAttribs
    in
    thumbnail


renderPosition { top, right } =
    [ Ui.Style.style "position" "fixed"
    , Ui.Style.style "top" (String.fromFloat top ++ "px")
    , Ui.Style.style "right" (String.fromFloat right ++ "px")
    ]


onMouseDownOutsideClose =
    Element.Events.Extra.onMousedownoutside Closed



-- (Return Nothing { state | visibility = Closed })


{-| When the visibility is open, just close id

But when it is closed, find the size of the viewport and the position of the swatch that was clicked.

Then make sure that the panel is usable

  - not overlapping screen dimensions
  - sticks to the swatch if the window is resized

-}
onMouseDownOpenOrClose visibility =
    let
        newStateDecoder =
            case visibility of
                Open _ ->
                    Decode.succeed Closed

                Closed ->
                    let
                        -- this probably should not be hard-coded here but meh
                        pickerHeight =
                            400

                        -- just a larg value
                        calculateOffsets swatch window =
                            { top = min swatch.y (window.height - pickerHeight)
                            , right = window.width - swatch.x
                            }
                                |> Open

                        swatchPositionDecoder =
                            Decode.map2
                                (\x y -> { x = x, y = y })
                                (Decode.at [ "target", "boundingClientRect", "x" ] Decode.float)
                                (Decode.at [ "target", "boundingClientRect", "y" ] Decode.float)

                        windowSizeDecoder =
                            Decode.map2
                                (\width height -> { width = width, height = height })
                                (Decode.at [ "target", "funk_window", "innerWidth" ] Decode.float)
                                (Decode.at [ "target", "funk_window", "innerHeight" ] Decode.float)
                    in
                    Decode.map2
                        calculateOffsets
                        swatchPositionDecoder
                        windowSizeDecoder
    in
    Element.Events.Extra.onMouseDown newStateDecoder


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



-- CODECS


encodeSwatchUpdate b =
    Encode.null


decodeSwatchUpdate =
    Decode.fail <| "Unknown constructor"


encodeAdvancedState : AdvancedState -> Encode.Value
encodeAdvancedState =
    .selection >> DesignSystem.Color.Selection.encodeSelection


decodeAdvancedState : Decode.Decoder AdvancedState
decodeAdvancedState =
    DesignSystem.Color.Selection.decodeSelection
        |> Decode.map (\sel -> { initAdvanced | selection = sel })
