module Color.Extra exposing (..)

{-| Small additions to the avh4/color library
-}

import Color
import Element
import Json.Decode as Decode
import Json.Encode as Encode


type alias Color =
    Color.Color


{-| String versions of the RGBA channels
-}
type alias Rgba255Strings =
    { red : String, green : String, blue : String, alpha : String }


toElmUi : Color.Color -> Element.Color
toElmUi color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    Element.rgba red green blue alpha


{-| Round a `Float` to a given number of decimal places.
-}
roundTo : Int -> Float -> Float
roundTo places value =
    let
        factor =
            toFloat (10 ^ places)
    in
    ((value * factor)
        |> round
        |> toFloat
    )
        / factor


{-| Convert RGBA input back to a Color.Color

    import Color

    parseRgbaStrings
        { red = "-1", green = "100", blue = "1000", alpha = "20" }
    --> Just (Color.rgba 0 0.3922 1 1)

    parseRgbaStrings { red = "areiston", green = "100", blue = "1000", alpha = "20" } --> Nothing

-}
parseRgbaStrings : Rgba255Strings -> Maybe Color.Color
parseRgbaStrings { red, green, blue, alpha } =
    case ( ( String.toInt red, String.toInt green ), ( String.toInt blue, String.toFloat alpha ) ) of
        ( ( Just r, Just g ), ( Just b, Just a ) ) ->
            Just
                (Color.rgba
                    (r |> toFloat |> (\channel -> clamp 0 255 channel / 255 |> roundTo 4))
                    (g |> toFloat |> (\channel -> clamp 0 255 channel / 255 |> roundTo 4))
                    (b |> toFloat |> (\channel -> clamp 0 255 channel / 255 |> roundTo 4))
                    (clamp 0 1 a)
                )

        _ ->
            Nothing


{-| Convert a color to string RGBA channels

    import Color

    toRgba255Strings (Color.rgba 0 1 0.5 0.8)
        --> {red = "0", green = "255", blue = "128", alpha = "0.8"}

-}
toRgba255Strings : Color -> Rgba255Strings
toRgba255Strings color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    { red = red * 255 |> round |> String.fromInt
    , green = green * 255 |> round |> String.fromInt
    , blue = blue * 255 |> round |> String.fromInt
    , alpha = alpha |> String.fromFloat
    }



-- [generator-start]


type alias Hsla =
    { hue : Float
    , saturation : Float
    , lightness : Float
    , alpha : Float
    }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeHsla =
    Decode.map4
        Hsla
        (Decode.field "hue" Decode.float)
        (Decode.field "saturation" Decode.float)
        (Decode.field "lightness" Decode.float)
        (Decode.field "alpha" Decode.float)


encodeHsla a =
    Encode.object
        [ ( "hue", Encode.float a.hue )
        , ( "saturation", Encode.float a.saturation )
        , ( "lightness", Encode.float a.lightness )
        , ( "alpha", Encode.float a.alpha )
        ]



-- [generator-end]


encodeColor =
    Color.toHsla
        >> encodeHsla


decodeColor =
    decodeHsla |> Decode.map Color.fromHsla


transparent =
    ColorRgba 1 1 1 0


toColorRgba255 { red, green, blue, alpha } =
    { red = (red * 255) |> round
    , green = (green * 255) |> round
    , blue = (blue * 255) |> round
    , alpha = alpha
    }


fromColorRgba255 { red, green, blue, alpha } =
    { red = (red |> toFloat) / 255
    , green = (green |> toFloat) / 255
    , blue = (blue |> toFloat) / 255
    , alpha = alpha
    }



-- [decgen-start]


type alias ColorRgba =
    { red : Float, green : Float, blue : Float, alpha : Float }


type alias ColorRgba255 =
    { red : Int, green : Int, blue : Int, alpha : Float }



-- [decgen-generated-start] -- DO NOT MODIFY or remove this line


decodeColorRgba =
    Decode.map4
        ColorRgba
        (Decode.field "red" Decode.float)
        (Decode.field "green" Decode.float)
        (Decode.field "blue" Decode.float)
        (Decode.field "alpha" Decode.float)


decodeColorRgba255 =
    Decode.map4
        ColorRgba255
        (Decode.field "red" Decode.int)
        (Decode.field "green" Decode.int)
        (Decode.field "blue" Decode.int)
        (Decode.field "alpha" Decode.float)


encodeColorRgba a =
    Encode.object
        [ ( "red", Encode.float a.red )
        , ( "green", Encode.float a.green )
        , ( "blue", Encode.float a.blue )
        , ( "alpha", Encode.float a.alpha )
        ]


encodeColorRgba255 a =
    Encode.object
        [ ( "red", Encode.int a.red )
        , ( "green", Encode.int a.green )
        , ( "blue", Encode.int a.blue )
        , ( "alpha", Encode.float a.alpha )
        ]



-- [decgen-end]


toCssString : Color.Color -> String
toCssString color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color

        vals =
            [ red, green, blue ]
                |> List.map ((*) 255)
                |> List.map (round >> String.fromInt)
                |> String.join ", "
    in
    "rgba("
        ++ vals
        ++ ", "
        ++ String.fromFloat alpha
        ++ ")"
