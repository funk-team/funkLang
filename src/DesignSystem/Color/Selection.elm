module DesignSystem.Color.Selection exposing (..)

{-| An advanced color picker can either define a color directly or refer to a color
-}

import Color
import Color.Extra
import DesignSystem.Color.Model
import Json.Decode as Decode
import Json.Encode as Encode


getSelectedColor : DesignSystem.Color.Model.Model -> Selection -> Maybe Color.Color
getSelectedColor designSystemColors sel =
    case sel of
        FromSystem ref ->
            DesignSystem.Color.Model.getSelectedColor ref designSystemColors

        Standalone color ->
            Just color



-- getSelectedSwatch : DesignSystem.Color.Model.Model -> Selection -> Maybe DesignSystem.Color.Model.Swatch
--     case sel of
--         FromSystem ref ->
--             DesignSystem.Color.Model.getSelectedColor ref designSystemColors
--
--         Standalone color ->
--             Just color
-- [generator-start]


type Selection
    = FromSystem DesignSystem.Color.Model.Selection
    | Standalone Color.Extra.Color



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeSelection =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeSelectionHelp


decodeSelectionHelp constructor =
    case constructor of
        "FromSystem" ->
            Decode.map
                FromSystem
                (Decode.field "A1" DesignSystem.Color.Model.decodeSelection)

        "Standalone" ->
            Decode.map
                Standalone
                (Decode.field "A1" Color.Extra.decodeColor)

        other ->
            Decode.fail <| "Unknown constructor for type Selection: " ++ other


encodeSelection a =
    case a of
        FromSystem a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "FromSystem" )
                , ( "A1", DesignSystem.Color.Model.encodeSelection a1 )
                ]

        Standalone a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Standalone" )
                , ( "A1", Color.Extra.encodeColor a1 )
                ]



-- [generator-end]
