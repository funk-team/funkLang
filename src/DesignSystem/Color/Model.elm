module DesignSystem.Color.Model exposing (..)

{-| Allow the user to design a color system

@@TODOs

  - contrast checks
  - color transformation logic

-}

import Color
import Color.Extra
import Dict
import Hover
import IntDict
import Json.Decode as Decode
import Json.Encode as Encode
import Ui.ColorPicker.Basic


type alias PickerState =
    Ui.ColorPicker.Basic.State


type alias Swatch =
    { label : String
    , value : Color.Extra.Color
    , pickerState : PickerState
    }



-- [generator-start]


type Selection
    = TextSwatchSelected
    | BackgroundSwatchSelected
    | OtherSwatchSelected Int


type alias Model =
    { -- swatches
      others : IntDict.Dict Swatch
    , background : Swatch
    , text : Swatch

    -- select and edit swatches
    , selection : Selection
    , hoveredElements : Hover.HoveredElements
    , addSwatchInput : Maybe String
    }


getSelectedColor : Selection -> Model -> Maybe Color.Color
getSelectedColor ref model =
    Maybe.map .value <| getSelectedSwatch ref model


getSelectedSwatch : Selection -> Model -> Maybe Swatch
getSelectedSwatch ref model =
    case ref of
        BackgroundSwatchSelected ->
            model.background |> Just

        TextSwatchSelected ->
            model.text |> Just

        OtherSwatchSelected key ->
            Dict.get key model.others


getSelectionList : Model -> List Selection
getSelectionList model =
    let
        otherSelections =
            Dict.toList model.others
                |> List.map (Tuple.first >> OtherSwatchSelected)
    in
    [ BackgroundSwatchSelected
    , TextSwatchSelected
    ]
        ++ otherSelections



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeModel =
    Decode.map6
        Model
        (Decode.field "others" (IntDict.decodeDict decodeSwatch))
        (Decode.field "background" decodeSwatch)
        (Decode.field "text" decodeSwatch)
        (Decode.field "selection" decodeSelection)
        (Decode.field "hoveredElements" Hover.decodeHoveredElements)
        (Decode.field "addSwatchInput" (Decode.maybe Decode.string))


decodeSelection =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeSelectionHelp


decodeSelectionHelp constructor =
    case constructor of
        "TextSwatchSelected" ->
            Decode.succeed TextSwatchSelected

        "BackgroundSwatchSelected" ->
            Decode.succeed BackgroundSwatchSelected

        "OtherSwatchSelected" ->
            Decode.map
                OtherSwatchSelected
                (Decode.field "A1" Decode.int)

        other ->
            Decode.fail <| "Unknown constructor for type Selection: " ++ other


encodeMaybeString a =
    case a of
        Just b ->
            Encode.string b

        Nothing ->
            Encode.null


encodeModel a =
    Encode.object
        [ ( "others", IntDict.encodeDict encodeSwatch a.others )
        , ( "background", encodeSwatch a.background )
        , ( "text", encodeSwatch a.text )
        , ( "selection", encodeSelection a.selection )
        , ( "hoveredElements", Hover.encodeHoveredElements a.hoveredElements )
        , ( "addSwatchInput", encodeMaybeString a.addSwatchInput )
        ]


encodeSelection a =
    case a of
        TextSwatchSelected ->
            Encode.object
                [ ( "Constructor", Encode.string "TextSwatchSelected" )
                ]

        BackgroundSwatchSelected ->
            Encode.object
                [ ( "Constructor", Encode.string "BackgroundSwatchSelected" )
                ]

        OtherSwatchSelected a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "OtherSwatchSelected" )
                , ( "A1", Encode.int a1 )
                ]



-- [generator-end]


decodeSwatch : Decode.Decoder Swatch
decodeSwatch =
    Decode.map2
        (\label color -> Swatch label color (Ui.ColorPicker.Basic.init color))
        (Decode.field "label" Decode.string)
        (Decode.field "value" Color.Extra.decodeColor)


encodeSwatch a =
    Encode.object
        [ ( "label", Encode.string a.label )
        , ( "value", Color.Extra.encodeColor a.value )
        , ( "pickerState", encodePickerState a.pickerState )
        ]


encodePickerState _ =
    Encode.null


decodePickerState =
    Decode.succeed Ui.ColorPicker.Basic.init
