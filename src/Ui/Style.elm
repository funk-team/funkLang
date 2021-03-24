module Ui.Style exposing (..)

{- Common constants and CSS helpers

   Suggestion: In the future we could maybe create a single type that represents
   Funk's styling, as a style palette (`UiPalette`), and have the `view` functions take
   in that style palette as argument.
   That way creating new styling for funk would only be a matter of creating a new
   `UiPalette` type and feeding that to the `view` functions, instead of the old styling.
   Potential use case: dark mode vs light mode styling
-}

import Color
import Color.Extra
import Color.Manipulate
import Element
import Element.Background
import Element.Font
import Html
import Html.Attributes


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }


headerHeight =
    40


monospace =
    Element.Font.family
        [ Element.Font.typeface "JetBrains Mono"
        , Element.Font.monospace
        ]


headerStyles =
    [ Element.height (Element.px headerHeight)
    , Element.Background.color black
    , Element.Font.color white
    , Element.width Element.fill
    ]


baseFontSize =
    16


base =
    [ Element.Font.size baseFontSize
    , interFont
    , exposeCssVariables |> Element.html |> Element.inFront
    ]



-- INTERFACE WITH CSS --


class =
    Element.htmlAttribute << Html.Attributes.class


exposeCssVariables =
    let
        vars =
            [ expose "highlight-solid" highlightColorSolidRaw
            , expose "highlight-solid-important" highlightColorSolidImportantRaw
            , expose "slight-accent" slightAccentRaw
            , expose "slighter-accent" slighterAccentRaw
            , expose "highlight-transparent-important" highlightColorTransparentImportantRaw
            ]
                |> String.join ""
    in
    ":root {\n"
        ++ vars
        ++ "\n}"
        |> styleTag


expose name value =
    "--" ++ name ++ ":" ++ Color.Extra.toCssString value ++ ";"


setHighlightToSecondary =
    class "highlight-secondary"



-- DEFINITIONS --


lightGrey =
    Element.rgba 0 0 0 0.05


sidebarWidth =
    250


rightSidebarWidth =
    350


transparent =
    Element.rgba 1 1 1 0


slightOutline =
    style


highlightColor =
    let
        { red, green, blue } =
            highlightColorSolidRaw
                |> Color.toRgba
    in
    Element.rgba red green blue 0.35


highlightColorMid =
    let
        { red, green, blue } =
            highlightColorSolidRaw
                |> Color.toRgba
    in
    Element.rgba red green blue 0.5


highlightColorLow =
    let
        { red, green, blue } =
            highlightColorSolidRaw
                |> Color.toRgba
    in
    Element.rgba red green blue 0.4


highlightColorSolidRaw =
    Color.rgb255 66 84 244


highlightColorSolidImportantRaw =
    Color.rgb255 231 57 57


highlightColorTransparentImportantRaw =
    Color.Manipulate.fadeOut 0.5 highlightColorSolidImportantRaw


highlightColorSolid =
    let
        { red, green, blue } =
            highlightColorSolidRaw
                |> Color.toRgba
    in
    Element.rgba red green blue 1


highlightColorSolidImportant =
    let
        { red, green, blue } =
            highlightColorSolidImportantRaw
                |> Color.toRgba
    in
    Element.rgba red green blue 1


highlightColorGradientVertical =
    Element.Background.gradient
        { angle = 90
        , steps = [ transparent, highlightColorSolid, transparent ]
        }


highlightColorGradientHorizontal =
    Element.Background.gradient
        { angle = 90
        , steps = [ transparent, highlightColorSolid, transparent ]
        }


importantHighlightColor =
    let
        { red, green, blue } =
            highlightColorSolidImportantRaw
                |> Color.toRgba
    in
    Element.rgba red green blue 0.25


importantHighlightColorSolid =
    let
        { red, green, blue } =
            highlightColorSolidImportantRaw
                |> Color.toRgba
    in
    Element.rgba red green blue 1


black =
    Element.rgb 0 0 0


greyRaw =
    Color.rgba 0 0 0 0.3


grey =
    Element.rgba 0 0 0 0.5


greyText =
    Element.rgba 218 223 225 0.5


white =
    Element.rgb 1 1 1


lightRed =
    Element.rgba 245 0 0 0.7


shadow =
    String.join ", "
        >> style "box-shadow"


shadowMedium =
    style "box-shadow" "0 10px 15px -3px rgba(0, 0, 0, 0.05), 0 4px 6px -2px rgba(0, 0, 0, 0.05), 0 2px 16px -2px rgba(0, 0, 0, 0.055)"


font name =
    Element.Font.family [ Element.Font.typeface name, Element.Font.sansSerif ]


interFont =
    Element.Font.family [ Element.Font.typeface "Inter", Element.Font.sansSerif ]


style : String -> String -> Element.Attribute msg
style prop val =
    Element.htmlAttribute (Html.Attributes.style prop val)


styleTag content =
    Html.node "style" [] [ Html.text content ]


pageBackground =
    let
        { red, green, blue } =
            greyRaw
                |> Color.toRgba
    in
    Element.Background.color (Element.rgba255 (round red) (round green) (round blue) 0.25)


transparentBlack =
    Element.rgba 0 0 0 0.5


slightAccent =
    let
        { red, green, blue, alpha } =
            slightAccentRaw
                |> Color.toRgba
    in
    Element.rgba255 (round red) (round green) (round blue) alpha


slighterAccent =
    let
        { red, green, blue, alpha } =
            slighterAccentRaw
                |> Color.toRgba
    in
    Element.rgba255 (round red) (round green) (round blue) alpha


slightAccentRaw =
    Color.Manipulate.fadeOut 0.1 greyRaw


slighterAccentRaw =
    slightAccentRaw |> Color.Manipulate.fadeOut 0.1


transition =
    style "transition" "0.1s ease-in-out"


styles =
    List.map (\( prop, val ) -> style prop val)


swiftOut =
    "cubic-bezier(0.55, 0, 0.1, 1)"


buttonRow =
    Element.row [ Element.width Element.fill ]


panelBackground =
    Element.Background.color white


{-| Add some neat shadows and a light border
-}
paperShadow : List (Element.Attribute msg)
paperShadow =
    let
        sheetShadow =
            style "box-shadow" "0 -1px 2px 0 rgba(0,0,0,0.04), 0 1px 2px 0 rgba(0,0,0,0.13), 0 17px 34px -15px rgba(118,118,118,0.28)"

        roundCorners =
            style "border-radius" "2px"

        whiteBackground =
            Element.Background.color white
    in
    [ whiteBackground
    , sheetShadow
    , roundCorners
    ]


monoFont =
    Element.Font.family [ Element.Font.typeface "JetBrains Mono", Element.Font.monospace ]


error : Element.Color
error =
    Element.rgb255 211 47 47
