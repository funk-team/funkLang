module Ui.ColorPicker.Basic exposing
    ( State, Msg, update, view
    , init
    )

{-| The funk color picker.

It is a fork of simonh1000/elm-colorpicker.

@docs State, Msg, empty, update, view

The main picker is for saturation and lightness, while the sliders below are for hue and opacity respectively.

-}

import Color exposing (Color)
import Color.Convert
import Color.Extra
import Element
import Element.Background
import Element.Border
import Html exposing (div, Html)
import Html.Attributes as Attrs
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (..)
import Svg.Attributes as SvgAttrs exposing (offset, stopColor, stopOpacity, x, x1, x2, y, y1, y2)
import Svg.Events as SvgEvents
import Ui.Dropdown
import Ui.Input
import Ui.Style



{-| The model stores the hue because dark/light colours are otherwise indistinguihsable.
But the user could easily change their hex between updates so we need to check that this has _probably_
not happened.
-}
type alias Model =
    { mouseTarget : MouseTarget
    , hue : Maybe Float -- 0.0 .. 1.0
    , stringInput : StringInput
    }


{-| User can use text input either as HEX or RGBA255 with alpha 0-1
-}
type StringInput
    = Hex String
    | Rgba Color.Extra.Rgba255Strings


type MouseTarget
    = Unpressed
    | SatLight Float -- hue, main area
    | HueSlider -- 1st slider
    | OpacitySlider Float -- hue, 2nd slider


{-| Opaque type. Needs to be added to your model. You will also need to store a `Color` in your model

    type alias Model =
        { myColour : Color
        , colorPicker : Ui.ColorPicker.Basic.Model
        }

-}
type State
    = State Model



-- /*
--  ██████  ██████  ███    ██ ███████ ████████
-- ██      ██    ██ ████   ██ ██         ██
-- ██      ██    ██ ██ ██  ██ ███████    ██
-- ██      ██    ██ ██  ██ ██      ██    ██
--  ██████  ██████  ██   ████ ███████    ██
-- */
--


widgetWidth =
    200


widgetPadding =
    6


widgetHeight =
    150



-- /*
-- ██    ██ ███████ ███████ ███████ ██    ██ ██
-- ██    ██ ██      ██      ██      ██    ██ ██
-- ██    ██ ███████ █████   █████   ██    ██ ██
-- ██    ██      ██ ██      ██      ██    ██ ██
--  ██████  ███████ ███████ ██       ██████  ███████
-- */
--


{-| Initial ColorPicker state

    init =
        { myColour = Color.red
        , colorPicker = Ui.ColorPicker.Basic.empty
        }

-}
init : Color -> State
init =
    blankModel >> State


blankModel : Color -> Model
blankModel color =
    { mouseTarget = Unpressed
    , hue = Nothing
    , stringInput = Hex (Color.Convert.colorToHex color)
    }



-- /*
-- ██    ██ ██████  ██████   █████  ████████ ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██
--  ██████  ██      ██████  ██   ██    ██    ███████
-- */
--


{-| Opaque type. These messages are handled by `Ui.ColorPicker.Basic.update`
-}
type Msg
    = OnMouseDown MouseTarget MouseInfo
    | OnMouseMove MouseTarget MouseInfo
    | OnClick MouseTarget MouseInfo
    | OnMouseUp
    | StringInputUpdated StringInput
    | StringInputTypeUpdated StringInput
    | NoOp -- used to prevent bubbling of messages


{-| On each update, ColorPicker returns its model and (where appropriate) the new selected colo(u)r.

    ColorPickermsg msg ->
        let
            (cp, col) =
                Ui.ColorPicker.Basic.update msg model.colorPicker
        in
            { model | colorPicker = cp, myColor = Maybe.withDefault model.myColor col }

-}
update : Msg -> Color -> State -> ( State, Maybe Color )
update message col (State model) =
    update_ message col model
        |> Tuple.mapFirst State


update_ : Msg -> Color -> Model -> ( Model, Maybe Color )
update_ message col model =
    let
        handleMouseMove mouseTarget mouseInfo =
            if mouseInfo.mousePressed && model.mouseTarget == mouseTarget then
                let
                    newColor =
                        calcNewColour mouseTarget mouseInfo
                in
                ( model
                    |> setHue mouseTarget mouseInfo
                    |> setStringInput newColor
                , newColor
                )

            else if not mouseInfo.mousePressed && model.mouseTarget == mouseTarget then
                -- Mouse wss unpressed whie not over a target
                ( setMouseTarget Unpressed model, Nothing )

            else
                ( model, Nothing )

        calcNewColour mouseTarget =
            case mouseTarget of
                SatLight hue ->
                    Just << calcSatLight col (Maybe.withDefault hue model.hue)

                HueSlider ->
                    Just << calcHue col

                OpacitySlider hue ->
                    -- \mouseInfo -> model.hue |> Maybe.map (\h -> calcOpacity col h mouseInfo)
                    Just << calcOpacity col (Maybe.withDefault hue model.hue)

                Unpressed ->
                    \_ -> Nothing
    in
    case message of
        StringInputTypeUpdated stringInput ->
            ( { model | stringInput = stringInput }
            , Nothing
            )

        StringInputUpdated stringInput ->
            let
                -- @@TODO: update model hue etc
                newColor =
                    case stringInput of
                        Hex hexString ->
                            case Color.Convert.hexToColor hexString of
                                Ok color ->
                                    Just color

                                Err _ ->
                                    Nothing

                        Rgba rgbaStrings ->
                            Color.Extra.parseRgbaStrings rgbaStrings
            in
            ( { model | stringInput = stringInput }
            , newColor
            )

        OnMouseDown mouseTarget mouseInfo ->
            let
                newColor =
                    calcNewColour mouseTarget mouseInfo
            in
            ( model
                |> setStringInput newColor
                |> setMouseTarget mouseTarget
                |> setHue mouseTarget mouseInfo
            , newColor
            )

        OnMouseMove mouseTarget mouseInfo ->
            handleMouseMove mouseTarget mouseInfo

        OnClick mouseTarget mouseInfo ->
            ( setHue mouseTarget mouseInfo model, calcNewColour mouseTarget mouseInfo )

        OnMouseUp ->
            ( setMouseTarget Unpressed model, Nothing )

        NoOp ->
            ( model, Nothing )


setMouseTarget : MouseTarget -> Model -> Model
setMouseTarget mouseTarget model =
    { model | mouseTarget = mouseTarget }


setStringInput : Maybe Color -> Model -> Model
setStringInput color model =
    case color of
        Nothing ->
            model

        Just c ->
            { model
                | stringInput =
                    case model.stringInput of
                        Hex hexString ->
                            Color.Convert.colorToHex c |> Hex

                        Rgba hexString ->
                            Color.Extra.toRgba255Strings c |> Rgba
            }


setHue : MouseTarget -> MouseInfo -> Model -> Model
setHue mouseTarget mouseInfo model =
    case mouseTarget of
        SatLight hue ->
            { model | hue = model.hue |> Maybe.withDefault hue |> Just }

        HueSlider ->
            { model | hue = Just <| toFloat mouseInfo.x / widgetWidth }

        OpacitySlider hue ->
            { model | hue = model.hue |> Maybe.withDefault hue |> Just }

        _ ->
            model


calcSatLight : Color -> Float -> MouseInfo -> Color
calcSatLight col currHue { x, y, mousePressed } =
    let
        hsla =
            Color.toHsla col
    in
    { hsla
        | hue = currHue
        , saturation = toFloat x / widgetWidth
        , lightness = 1 - toFloat y / widgetHeight
    }
        |> Color.fromHsla


calcHue : Color -> MouseInfo -> Color
calcHue col { x, mousePressed } =
    let
        ({ saturation, lightness, alpha } as hsla) =
            Color.toHsla col

        hue =
            toFloat x / widgetWidth

        newCol =
            -- Enable 'escape from black'
            if saturation == 0 && lightness < 0.02 then
                { hsla | hue = hue, saturation = 0.5, lightness = 0.5 }

            else
                { hsla | hue = hue }
    in
    newCol |> Color.fromHsla


calcOpacity : Color -> Float -> MouseInfo -> Color
calcOpacity col _ { x, mousePressed } =
    let
        hsla =
            Color.toHsla col
    in
    { hsla | alpha = toFloat x / widgetWidth }
        |> Color.fromHsla



-- /*
-- ██    ██ ██ ███████ ██     ██
-- ██    ██ ██ ██      ██     ██
-- ██    ██ ██ █████   ██  █  ██
--  ██  ██  ██ ██      ██ ███ ██
--   ████   ██ ███████  ███ ███
-- */
--


{-| Renders the color picker on screen
-}
view : Color -> State -> Element.Element Msg
view col (State model) =
    let
        hsla =
            Color.toHsla col

        hue =
            Maybe.withDefault hsla.hue model.hue

        colCss =
            Color.hsl hue 1 0.5
                |> Color.toCssString
    in
    Element.column
        [ Element.Background.color Ui.Style.white
        , Element.padding widgetPadding
        , Element.width <| Element.px (widgetWidth + 2 * widgetPadding)
        , Element.Border.rounded 2
        , Element.spacing 10
        ]
        [ Element.el [] <|
            Element.column []
                [ satLightPalette hue colCss model.mouseTarget
                , Element.html <| pickerIndicator col
                ]
        , Element.el [] <|
            Element.html <|
                div (pickerStyles ++ sliderContainerStyles "hue")
                    [ huePalette model.mouseTarget
                    , hueMarker hue
                    ]
        , Element.el [] <|
            Element.html <|
                div (checkedBkgStyles ++ pickerStyles ++ sliderContainerStyles "opacity")
                    [ opacityPalette hsla model
                    , alphaMarker hsla.alpha
                    ]
        , viewStringInput col model
        ]


viewStringInput : Color.Color -> Model -> Element.Element Msg
viewStringInput color model =
    let
        rows =
            [ Ui.Dropdown.viewRow
                { isSelected =
                    case model.stringInput of
                        Hex _ ->
                            True

                        Rgba _ ->
                            False
                , label = Ui.Dropdown.Description "HEX"
                , sideNote = Ui.Dropdown.NoDetail
                , onSelect = Hex (Color.Convert.colorToHex color)
                , detail = Ui.Dropdown.Description "The all-time classic"
                , rightHandText = Nothing
                }
            , Ui.Dropdown.viewRow
                { isSelected =
                    case model.stringInput of
                        Rgba _ ->
                            True

                        Hex _ ->
                            False
                , label = Ui.Dropdown.Description "RGBA"
                , sideNote = Ui.Dropdown.NoDetail
                , onSelect =
                    Rgba (Color.Extra.toRgba255Strings color)
                , detail = Ui.Dropdown.Description "In case you need some alpha"
                , rightHandText = Nothing
                }
            ]

        dropdown =
            Ui.Dropdown.view
                [ Element.htmlAttribute (Attrs.style "margin-left" "-6px") ]
                { contents = rows
                , label =
                    case model.stringInput of
                        Hex _ ->
                            "HEX"

                        Rgba _ ->
                            "RGBA"
                }

        inputs =
            case model.stringInput of
                Hex stringValue ->
                    Ui.Input.string "" (Color.Convert.colorToHex color) stringValue
                        |> Element.map Hex

                Rgba rgba ->
                    let
                        { red, green, blue, alpha } =
                            Color.toRgba color

                        redPlaceholder =
                            red * 255 |> round |> String.fromInt

                        greenPlaceholder =
                            green * 255 |> round |> String.fromInt

                        bluePlaceholder =
                            blue * 255 |> round |> String.fromInt

                        alphaPlaceholder =
                            alpha |> String.fromFloat

                        rgbaInputs =
                            Element.row [ Element.width Element.fill, Element.spacing 5 ]
                                [ Ui.Input.string "" redPlaceholder rgba.red
                                    |> Element.map (\red_ -> { rgba | red = red_ })
                                , Ui.Input.string "" greenPlaceholder rgba.green
                                    |> Element.map (\green_ -> { rgba | green = green_ })
                                , Ui.Input.string "" bluePlaceholder rgba.blue
                                    |> Element.map (\blue_ -> { rgba | blue = blue_ })
                                , Ui.Input.string "" alphaPlaceholder rgba.alpha
                                    |> Element.map (\alpha_ -> { rgba | alpha = alpha_ })
                                ]
                    in
                    rgbaInputs
                        |> Element.map Rgba
    in
    Element.row
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ dropdown
            |> Element.map StringInputTypeUpdated
        , inputs
            |> Element.map StringInputUpdated
        ]


satLightPalette : Float -> String -> MouseTarget -> Element.Element Msg
satLightPalette hue colCss mouseTarget =
    let
        palette =
            svg
                [ SvgAttrs.width (String.fromInt widgetWidth)
                , SvgAttrs.height (String.fromInt widgetHeight)
                , SvgAttrs.class "main-picker"
                , Attrs.style "pointer-events" "none"
                , SvgAttrs.display "block"
                ]
                [ defs
                    []
                    [ linearGradient
                        [ SvgAttrs.id "pickerSaturation" ]
                        [ stop [ offset "0", stopColor "#808080", stopOpacity "1" ] []
                        , stop [ offset "1", stopColor "#808080", stopOpacity "0" ] []
                        ]
                    , linearGradient
                        [ SvgAttrs.id "pickerBrightness", x1 "0", y1 "0", x2 "0", y2 "1" ]
                        [ stop [ offset "0", stopColor "#fff", stopOpacity "1" ] []
                        , stop [ offset "0.499", stopColor "#fff", stopOpacity "0" ] []
                        , stop [ offset "0.5", stopColor "#000", stopOpacity "0" ] []
                        , stop [ offset "1", stopColor "#000", stopOpacity "1" ] []
                        ]
                    ]
                , rect [ SvgAttrs.width (String.fromInt widgetWidth), SvgAttrs.height (String.fromInt widgetHeight), SvgAttrs.fill colCss, SvgAttrs.id "picker" ] []
                , rect [ SvgAttrs.width (String.fromInt widgetWidth), SvgAttrs.height (String.fromInt widgetHeight), SvgAttrs.fill "url(#pickerSaturation)" ] []
                , rect
                    [ SvgAttrs.width (String.fromInt widgetWidth)
                    , SvgAttrs.height (String.fromInt widgetHeight)
                    , SvgAttrs.fill "url(#pickerBrightness)"
                    ]
                    []
                ]

        eventHandlers =
            svgDragAttrs mouseTarget (SatLight hue) (OnMouseMove <| SatLight hue)
                |> List.map Element.htmlAttribute

        eventCapturer =
            Element.el
                ([ Element.width Element.fill, Element.height Element.fill ] ++ eventHandlers)
                Element.none
                |> Element.inFront
    in
    Element.el [ eventCapturer ] (Element.html palette)


{-| pick saturation & lightness
-}
pickerIndicator : Color -> Html Msg
pickerIndicator col =
    let
        adjustment =
            4

        { saturation, lightness } =
            Color.toHsla col

        borderColor =
            if lightness > 0.95 then
                "#cccccc"

            else
                "#ffffff"

        cx_ =
            saturation * widgetWidth - adjustment |> round |> String.fromInt

        cy_ =
            widgetHeight - lightness * widgetHeight - adjustment |> round |> String.fromInt
    in
    div
        [ Attrs.style "position" "absolute"
        , Attrs.style "top" (cy_ ++ "px")
        , Attrs.style "left" (cx_ ++ "px")
        , Attrs.style "border-radius" "100%"
        , Attrs.style "border" ("2px solid " ++ borderColor)
        , Attrs.style "width" "6px"
        , Attrs.style "height" "6px"
        , Attrs.style "pointer-events" "none"
        ]
        []



-- --------------------------
-- HUE
-- --------------------------


huePalette : MouseTarget -> Svg Msg
huePalette mouseTarget =
    let
        mkStop ( os, sc ) =
            stop [ offset os, stopColor sc, stopOpacity "1" ] []

        stops : List ( String, String )
        stops =
            [ ( "0%", "#FF0000" )
            , ( "17%", "#FF00FF" )
            , ( "33%", "#0000FF" )
            , ( "50%", "#00FFFF" )
            , ( "66%", "#00FF00" )
            , ( "83%", "#FFFF00" )
            , ( "100%", "#FF0000" )
            ]
    in
    svg
        (SvgAttrs.class "hue-picker" :: sliderStyles)
        [ defs []
            [ linearGradient
                [ SvgAttrs.id "gradient-hsv", x1 "100%", y1 "0%", x2 "0%", y2 "0%" ]
                (stops |> List.map mkStop)
            ]
        , rect
            ([ x "0"
             , y "0"
             , SvgAttrs.width (String.fromInt widgetWidth)
             , SvgAttrs.height "100%"
             , SvgAttrs.fill "url(#gradient-hsv)"
             ]
                ++ svgDragAttrs mouseTarget HueSlider (OnMouseMove HueSlider)
            )
            []
        ]



-- --------------------------
-- OPACITY
-- --------------------------


opacityPalette : { a | hue : Float, saturation : Float, lightness : Float } -> Model -> Html Msg
opacityPalette hsla model =
    let
        mkCol op =
            Color.hsla hsla.hue hsla.saturation hsla.lightness op
                |> Color.toCssString

        grad =
            "linear-gradient(0.25turn, " ++ mkCol 0 ++ ", " ++ mkCol 1 ++ ")"

        overlay =
            [ Attrs.style "background" grad
            , Attrs.style "height" "100%"
            , Attrs.style "width" "100%"
            ]

        mouseTarget =
            OpacitySlider hsla.hue
    in
    div (overlay ++ htmlDragAttrs model.mouseTarget mouseTarget (OnMouseMove mouseTarget)) []


{-| Select the hue
-}
hueMarker : Float -> Html Msg
hueMarker lastHue =
    let
        correction =
            4

        xVal =
            -- shift by 4px to center on selected color
            (lastHue * widgetWidth - correction) |> round |> String.fromInt
    in
    div (Attrs.style "left" (xVal ++ "px") :: markerAttrs) []


alphaMarker : Float -> Html Msg
alphaMarker alpha =
    let
        correction =
            4

        xVal =
            -- shift by 4px to center on selected color
            (alpha * widgetWidth - correction) |> round |> String.fromInt
    in
    div (Attrs.style "left" (xVal ++ "px") :: markerAttrs) []



-- /*
-- ███████ ██    ██ ███████ ███    ██ ████████     ██   ██  █████  ███    ██ ██████  ██      ███████ ██████  ███████
-- ██      ██    ██ ██      ████   ██    ██        ██   ██ ██   ██ ████   ██ ██   ██ ██      ██      ██   ██ ██
-- █████   ██    ██ █████   ██ ██  ██    ██        ███████ ███████ ██ ██  ██ ██   ██ ██      █████   ██████  ███████
-- ██       ██  ██  ██      ██  ██ ██    ██        ██   ██ ██   ██ ██  ██ ██ ██   ██ ██      ██      ██   ██      ██
-- ███████   ████   ███████ ██   ████    ██        ██   ██ ██   ██ ██   ████ ██████  ███████ ███████ ██   ██ ███████
-- */
--


htmlDragAttrs : MouseTarget -> MouseTarget -> (MouseInfo -> Msg) -> List (Svg.Attribute Msg)
htmlDragAttrs currMouseTgt thisTgt onMoveMsg =
    let
        common =
            [ Html.Events.on "mousedown" <| Decode.map (OnMouseDown thisTgt) decodeMouseInfo
            , Html.Events.onMouseUp OnMouseUp
            , onClickHtml <| OnClick thisTgt
            ]
    in
    if currMouseTgt == thisTgt then
        -- for the current target, we also want to track moves
        Html.Events.on "mousemove" (Decode.map onMoveMsg decodeMouseInfo) :: common

    else
        common


onClickHtml : (MouseInfo -> Msg) -> Html.Attribute Msg
onClickHtml msgCreator =
    Html.Events.on "click" (Decode.map msgCreator decodeMouseInfo)


svgDragAttrs : MouseTarget -> MouseTarget -> (MouseInfo -> Msg) -> List (Svg.Attribute Msg)
svgDragAttrs currMouseTgt thisTgt onMoveMsg =
    let
        common =
            [ onMouseDownSvg <| OnMouseDown thisTgt
            , SvgEvents.onMouseUp OnMouseUp
            , onClickSvg <| OnClick thisTgt
            ]
    in
    if currMouseTgt == thisTgt then
        -- for the current target, we also want to track moves
        onMouseMoveSvg onMoveMsg :: common

    else
        common


onMouseDownSvg : (MouseInfo -> Msg) -> Svg.Attribute Msg
onMouseDownSvg msgCreator =
    SvgEvents.on "mousedown" (Decode.map msgCreator decodeMouseInfo)


onMouseMoveSvg : (MouseInfo -> Msg) -> Svg.Attribute Msg
onMouseMoveSvg msgCreator =
    SvgEvents.on "mousemove" (Decode.map msgCreator decodeMouseInfo)


onClickSvg : (MouseInfo -> Msg) -> Svg.Attribute Msg
onClickSvg msgCreator =
    SvgEvents.on "click" (Decode.map msgCreator decodeMouseInfo)


{-| Hack to prevent SVG click events bubble through to rest of app. SVG does not have an onWithOptions
-}
bubblePreventer : Html.Attribute Msg
bubblePreventer =
    Html.Events.stopPropagationOn "click" <| Decode.succeed ( NoOp, True )



-- MouseInfo`


type alias MouseInfo =
    { x : Int
    , y : Int
    , mousePressed : Bool
    }


decodeMouseInfo : Decoder MouseInfo
decodeMouseInfo =
    Decode.map3 MouseInfo
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)
        (Decode.field "buttons" Decode.int |> Decode.map ((/=) 0))



-- Html Attributes


sliderContainerStyles : String -> List (Html.Attribute msg)
sliderContainerStyles name =
    [ Attrs.style "width" (String.fromInt widgetWidth ++ "px")
    , Attrs.style "height" "12px"
    , Attrs.class <| "color-picker-slider " ++ name
    ]


checkedBkgStyles : List (Html.Attribute msg)
checkedBkgStyles =
    [ Attrs.style "background-size" "12px 12px"
    , Attrs.style "background-position" "0 0, 0 6px, 6px -6px, -6px 0px"
    , Attrs.style "background-image" "linear-gradient(45deg, #808080 25%, transparent 25%), linear-gradient(-45deg, #808080 25%, transparent 25%), linear-gradient(45deg, transparent 75%, #808080 75%), linear-gradient(-45deg, transparent 75%, #808080 75%)"
    ]


pickerStyles : List (Html.Attribute msg)
pickerStyles =
    [ Attrs.style "cursor" "crosshair"
    , Attrs.style "position" "relative"
    ]


markerAttrs : List (Html.Attribute msg)
markerAttrs =
    [ Attrs.style "position" "absolute"
    , Attrs.style "top" "1px"
    , Attrs.style "bottom" "1px"
    , Attrs.style "border" "1px solid #ddd"
    , Attrs.style "background-color" "#ffffff"
    , Attrs.style "width" "6px"
    , -- this is essential to enable dragging
      Attrs.style "pointer-events" "none"
    ]



-- SVG Attributes


sliderStyles : List (Svg.Attribute msg)
sliderStyles =
    [ SvgAttrs.width (String.fromInt widgetWidth)
    , SvgAttrs.height "100%"
    , SvgAttrs.display "block"
    ]
