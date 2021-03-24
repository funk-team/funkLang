module Canvas.Tool.Model exposing (..)

import Canvas.Selection
import Canvas.Tool.Cut
import Canvas.Tool.Debug
import Canvas.Tool.Draw
import Canvas.Tool.Draw.Model
import Canvas.Tool.Msg
import Canvas.Tool.Transform.Model
import Element
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Spec.Element
import Spec.Element.Model
import Spec.Mutation


init : Tool
init =
    Transform Canvas.Tool.Transform.Model.NotTransforming


type alias ActiveState =
    { simpleElement : Bool
    , simpleTextInput : Bool
    , simpleButton : Bool
    , simpleText : Bool
    , transform : Bool
    , cut : Bool
    , debug : Bool
    , evolve : Bool
    }


type alias Return =
    { tool : Tool
    , mutation : Maybe Spec.Mutation.Mutation
    , seed : Random.Seed
    , selection : Canvas.Selection.Selection
    }


{-| Things to decorate an element with.
Layout is overriding an elements previous layout settings.
For example, if we resize, we ant to replace the current layout of an element
with the layout that the user is expressing using the resize tool.
If both the resize tool and the element would create layout-relevant attributes we would get conflicts.

This is only relevant for elements (atm) because the tool can not change unwrap attributes of the root element,
it can only _add_ to them.

-}
type alias OverridesOrDecoration =
    { other : List (Element.Attribute Canvas.Tool.Msg.Msg)
    , layout : List (Element.Attribute Canvas.Tool.Msg.Msg)
    , element : Spec.Element.Model.EitherElement
    }


type alias DrawingDropDownPrams =
    { dropDownOpen : Bool
    , selectedTool : Tool
    , previousTool : Tool
    }



-- [generator-start]


type Tool
    = Draw Canvas.Tool.Draw.Model.State
    | Cut Canvas.Tool.Cut.State
    | Transform Canvas.Tool.Transform.Model.State
    | Debug Canvas.Tool.Debug.State



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeTool =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeToolHelp


decodeToolHelp constructor =
    case constructor of
        "Draw" ->
            Decode.map
                Draw
                (Decode.field "A1" Canvas.Tool.Draw.Model.decodeState)

        "Cut" ->
            Decode.map
                Cut
                (Decode.field "A1" Canvas.Tool.Cut.decodeState)

        "Transform" ->
            Decode.map
                Transform
                (Decode.field "A1" Canvas.Tool.Transform.Model.decodeState)

        "Debug" ->
            Decode.map
                Debug
                (Decode.field "A1" Canvas.Tool.Debug.decodeState)

        other ->
            Decode.fail <| "Unknown constructor for type Tool: " ++ other


encodeTool a =
    case a of
        Draw a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Draw" )
                , ( "A1", Canvas.Tool.Draw.Model.encodeState a1 )
                ]

        Cut a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Cut" )
                , ( "A1", Canvas.Tool.Cut.encodeState a1 )
                ]

        Transform a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Transform" )
                , ( "A1", Canvas.Tool.Transform.Model.encodeState a1 )
                ]

        Debug a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Debug" )
                , ( "A1", Canvas.Tool.Debug.encodeState a1 )
                ]



-- [generator-end]
