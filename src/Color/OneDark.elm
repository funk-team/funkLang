module Color.OneDark exposing
    ( black, white
    , lightRed, darkRed
    , lightYellow, darkYellow
    , blue, cyan, green, magenta
    , gutterGrey, commentGrey
    )

{-| This module provides onedark color scheme using elm-ui.


# ColorSheme

@docs black, white
@docs lightRed, darkRed
@docs lightYellow, darkYellow
@docs blue, cyan, green, magenta
@docs gutterGrey, commentGrey

-}

import Color exposing (Color)


{-| rgb(40, 44, 52)
#282c34
-}
black : Color
black =
    Color.rgb255 40 44 52


{-| rgb(171, 178, 191)
#abb2bf
-}
white : Color
white =
    Color.rgb255 171 178 191


{-| rgb(224, 108, 117)
#e06c75
-}
lightRed : Color
lightRed =
    Color.rgb255 224 108 117


{-| rgb(190, 80, 70)
#be5046
-}
darkRed : Color
darkRed =
    Color.rgb255 190 80 70


{-| rgb(152, 195, 121)
#98c379
-}
green : Color
green =
    Color.rgb255 152 195 121


{-| rgb(229, 192, 123)
#e5c07b
-}
lightYellow : Color
lightYellow =
    Color.rgb255 229 192 123


{-| rgb(209, 154, 102)
#d19a66
-}
darkYellow : Color
darkYellow =
    Color.rgb255 209 154 102


{-| rgb(97, 175, 239)
#61afef
-}
blue : Color
blue =
    Color.rgb255 97 175 239


{-| rgb(198, 120, 221)
#c678dd
-}
magenta : Color
magenta =
    Color.rgb255 198 120 221


{-| rgb(86, 182, 194)
#56b6c2
-}
cyan : Color
cyan =
    Color.rgb255 86 182 194


{-| rgb(76, 82, 99)
#4b5263
-}
gutterGrey : Color
gutterGrey =
    Color.rgb255 76 82 99


{-| rgb(92, 99, 112)
#5c6370
-}
commentGrey : Color
commentGrey =
    Color.rgb255 92 99 112
