module Hover exposing (..)

import Cmd.Extra exposing (..)
import Delay
import Element
import Element.Events
import Json.Decode as Decode
import Json.Encode as Encode



-- [generator-start]


type Msg
    = On String
    | Off String
    | DelayedOn Float String
    | DelayedOff Float String


type alias HoveredElements =
    List String


update : Msg -> HoveredElements -> ( HoveredElements, Cmd Msg )
update msg model =
    case msg of
        On key ->
            on key model
                |> withNoCmd

        Off key ->
            off key model
                |> withNoCmd

        DelayedOn time key ->
            ( model
            , Delay.after
                time
                Delay.Second
                (On key)
            )

        DelayedOff time key ->
            ( model
            , Delay.after
                time
                Delay.Second
                (Off key)
            )


on key model =
    key :: model


off key model =
    List.filter ((/=) key) model


attributes : String -> List (Element.Attribute Msg)
attributes key =
    [ Element.Events.onMouseEnter (On key)
    , Element.Events.onMouseLeave (Off key)
    ]


attributesWithDelay : ( Float, Float ) -> String -> List (Element.Attribute Msg)
attributesWithDelay ( delayOn, delayOff ) key =
    [ Element.Events.onMouseEnter (DelayedOn delayOn key)
    , Element.Events.onMouseLeave (DelayedOff delayOff key)
    ]


isHovered : HoveredElements -> String -> Bool
isHovered model key =
    model
        |> List.member key


isNotHovered : HoveredElements -> String -> Bool
isNotHovered model key =
    model
        |> List.member key
        |> not



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeHoveredElements =
    Decode.list Decode.string


decodeMsg =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeMsgHelp


decodeMsgHelp constructor =
    case constructor of
        "On" ->
            Decode.map
                On
                (Decode.field "A1" Decode.string)

        "Off" ->
            Decode.map
                Off
                (Decode.field "A1" Decode.string)

        "DelayedOn" ->
            Decode.map2
                DelayedOn
                (Decode.field "A1" Decode.float)
                (Decode.field "A2" Decode.string)

        "DelayedOff" ->
            Decode.map2
                DelayedOff
                (Decode.field "A1" Decode.float)
                (Decode.field "A2" Decode.string)

        other ->
            Decode.fail <| "Unknown constructor for type Msg: " ++ other


encodeHoveredElements a =
    Encode.list Encode.string a


encodeMsg a =
    case a of
        On a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "On" )
                , ( "A1", Encode.string a1 )
                ]

        Off a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Off" )
                , ( "A1", Encode.string a1 )
                ]

        DelayedOn a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "DelayedOn" )
                , ( "A1", Encode.float a1 )
                , ( "A2", Encode.string a2 )
                ]

        DelayedOff a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "DelayedOff" )
                , ( "A1", Encode.float a1 )
                , ( "A2", Encode.string a2 )
                ]



-- [generator-end]
