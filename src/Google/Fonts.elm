module Google.Fonts exposing (..)

{-| Use this module to work with google fonts.
A font is a collection of characters with a similar design. These characters include lowercase and uppercase letters, numbers, punctuation marks, and symbols. Changing the font can alter the look and feel of a block of text.
A font is not the typographic system for which they are used.
-}

import Element
import Html.Attributes
import Json.Decode
import Json.Decode.Field
import Json.Encode
import RemoteData
import RemoteData.Http
import Url.Builder
import ZipList



-- model
-- [generator-start]


type alias Family =
    String


type alias FileReference =
    String


type alias Font =
    { family : Family
    , variants : ZipList.ZipList Variant
    , subsets : List Subset
    , category : String
    }


type alias Fonts =
    RemoteData.WebData (List Font)


decodeFonts =
    Json.Decode.succeed initFonts


encodeFonts _ =
    Json.Encode.null


initFonts =
    RemoteData.Loading


defaultFont : Font
defaultFont =
    { category = "sans-serif"
    , family = "Roboto"
    , subsets = [ Cyrillic, CyrillicExt, Greek, GreekExt, Latin, LatinExt, Vietnamese ]
    , variants = ZipList.Zipper [ W300italic, W300, W100italic, W100 ] Regular [ Italic, W500, W500italic, W700, W700italic, W900, W900italic ]
    }


fontToAttributes : Font -> List (Element.Attribute msg)
fontToAttributes font =
    (::)
        (font.family
            |> Html.Attributes.style "font-family"
            |> Element.htmlAttribute
        )
        (font.variants
            |> ZipList.current
            |> variantToStyles
        )


{-| like fontToAttributes but with the regular variant of the font

note: has the same behaviour as fontToAttributes if no regular variant is found

-}
regularFontToAttributes : Font -> List (Element.Attribute msg)
regularFontToAttributes font =
    toRegularVariant font
        |> fontToAttributes


{-| Gives the same font but with the regular variant selected.

note: if no regular variant is found gives the font unchanged

-}
toRegularVariant : Font -> Font
toRegularVariant font =
    let
        isRegular variant =
            variant == Regular

        updateVariant : Font -> ZipList.ZipList Variant -> Font
        updateVariant oldFont newVariant =
            { oldFont | variants = newVariant }
    in
    font.variants
        |> ZipList.goToFirst isRegular
        |> Maybe.map (updateVariant font)
        |> Maybe.withDefault font



-- request fonts from google


googleApiBase =
    "https://www.googleapis.com"


googleApi_key =
    "AIzaSyA8mev5haACftvGQTiipfiaRf40nXhzYvk"


request : Cmd Fonts
request =
    let
        apiParam =
            Url.Builder.string "key" googleApi_key

        sortParam =
            Url.Builder.string "sort" "popularity"

        url =
            Url.Builder.crossOrigin
                googleApiBase
                [ "webfonts", "v1", "webfonts" ]
                [ sortParam, apiParam ]
    in
    RemoteData.Http.get
        url
        identity
        decodeFontsFromGoogleApi



-- font decoders from google api


decodeFontsFromGoogleApi =
    Json.Decode.list decodeFontFromGoogleApi
        |> Json.Decode.field "items"


decodeFontFromGoogleApi =
    Json.Decode.Field.require "family" Json.Decode.string <|
        \family ->
            Json.Decode.Field.require "variants" (Json.Decode.list decodeVariant) <|
                \variantList ->
                    Json.Decode.Field.require "subsets" (Json.Decode.list decodeSubset) <|
                        \subsets ->
                            Json.Decode.Field.require "files" (Json.Decode.dict Json.Decode.string) <|
                                \files ->
                                    Json.Decode.Field.require "category" Json.Decode.string <|
                                        \category ->
                                            let
                                                maybeVariants =
                                                    let
                                                        maybeVariantZipList =
                                                            variantList
                                                                |> ZipList.fromList
                                                    in
                                                    maybeVariantZipList
                                                        |> Maybe.andThen
                                                            (ZipList.goToFirst
                                                                ((==) Regular)
                                                            )
                                                        |> Maybe.map Just
                                                        |> Maybe.withDefault maybeVariantZipList
                                            in
                                            maybeVariants
                                                |> Maybe.map
                                                    (\variants ->
                                                        Json.Decode.succeed
                                                            { family = family
                                                            , variants = variants
                                                            , subsets = subsets
                                                            , category = category
                                                            }
                                                    )
                                                |> Maybe.withDefault
                                                    (Json.Decode.fail "Font variant list empty.")



-- variant


type Variant
    = Regular
    | Italic
    | W100
    | W100italic
    | W200
    | W200italic
    | W300
    | W300italic
    | W400
    | W400italic
    | W500
    | W500italic
    | W600
    | W600italic
    | W700
    | W700italic
    | W800
    | W800italic
    | W900
    | W900italic


variantToString v =
    case v of
        Regular ->
            "regular"

        Italic ->
            "italic"

        W100 ->
            "100"

        W100italic ->
            "100 italic"

        W200 ->
            "200"

        W200italic ->
            "200 italic"

        W300 ->
            "300"

        W300italic ->
            "300 italic"

        W400 ->
            "400"

        W400italic ->
            "400 italic"

        W500 ->
            "500"

        W500italic ->
            "500 italic"

        W600 ->
            "600"

        W600italic ->
            "600 italic"

        W700 ->
            "700"

        W700italic ->
            "700 italic"

        W800 ->
            "800"

        W800italic ->
            "800 italic"

        W900 ->
            "900"

        W900italic ->
            "900 italic"


weight =
    Html.Attributes.style "font-weight" >> Element.htmlAttribute


style =
    Html.Attributes.style "font-style" >> Element.htmlAttribute


variantToStyles v =
    case v of
        Regular ->
            [ weight "normal", style "normal" ]

        Italic ->
            [ style "italic" ]

        W100 ->
            [ weight "100" ]

        W100italic ->
            [ weight "100", style "italic" ]

        W200 ->
            [ weight "200" ]

        W200italic ->
            [ weight "200", style "italic" ]

        W300 ->
            [ weight "300" ]

        W300italic ->
            [ weight "300", style "italic" ]

        W400 ->
            [ weight "400" ]

        W400italic ->
            [ weight "400", style "italic" ]

        W500 ->
            [ weight "500" ]

        W500italic ->
            [ weight "500", style "italic" ]

        W600 ->
            [ weight "600" ]

        W600italic ->
            [ weight "600", style "italic" ]

        W700 ->
            [ weight "700" ]

        W700italic ->
            [ weight "700", style "italic" ]

        W800 ->
            [ weight "800" ]

        W800italic ->
            [ weight "800", style "italic" ]

        W900 ->
            [ weight "900" ]

        W900italic ->
            [ weight "900", style "italic" ]


encodeVariant =
    let
        toStr =
            \v ->
                case v of
                    Regular ->
                        "regular"

                    Italic ->
                        "italic"

                    W100 ->
                        "100"

                    W100italic ->
                        "100italic"

                    W200 ->
                        "200"

                    W200italic ->
                        "200italic"

                    W300 ->
                        "300"

                    W300italic ->
                        "300italic"

                    W400 ->
                        "400"

                    W400italic ->
                        "400italic"

                    W500 ->
                        "500"

                    W500italic ->
                        "500italic"

                    W600 ->
                        "600"

                    W600italic ->
                        "600italic"

                    W700 ->
                        "700"

                    W700italic ->
                        "700italic"

                    W800 ->
                        "800"

                    W800italic ->
                        "800italic"

                    W900 ->
                        "900"

                    W900italic ->
                        "900italic"
    in
    toStr >> Json.Encode.string


decodeVariant =
    let
        recover x =
            case x of
                "regular" ->
                    Json.Decode.succeed Regular

                "italic" ->
                    Json.Decode.succeed Italic

                "100" ->
                    Json.Decode.succeed W100

                "100italic" ->
                    Json.Decode.succeed W100italic

                "200" ->
                    Json.Decode.succeed W200

                "200italic" ->
                    Json.Decode.succeed W200italic

                "300" ->
                    Json.Decode.succeed W300

                "300italic" ->
                    Json.Decode.succeed W300italic

                "400" ->
                    Json.Decode.succeed W400

                "400italic" ->
                    Json.Decode.succeed W400italic

                "500" ->
                    Json.Decode.succeed W500

                "500italic" ->
                    Json.Decode.succeed W500italic

                "600" ->
                    Json.Decode.succeed W600

                "600italic" ->
                    Json.Decode.succeed W600italic

                "700" ->
                    Json.Decode.succeed W700

                "700italic" ->
                    Json.Decode.succeed W700italic

                "800" ->
                    Json.Decode.succeed W800

                "800italic" ->
                    Json.Decode.succeed W800italic

                "900" ->
                    Json.Decode.succeed W900

                "900italic" ->
                    Json.Decode.succeed W900italic

                other ->
                    Json.Decode.fail <|
                        "Unknown constructor for type Variant: "
                            ++ other
    in
    Json.Decode.string
        |> Json.Decode.andThen recover



-- subset


type Subset
    = Arabic
    | Bengali
    | ChineseHongkong
    | ChineseSimplified
    | ChineseTraditional
    | Cyrillic
    | CyrillicExt
    | Devanagari
    | Greek
    | GreekExt
    | Gujarati
    | Gurmukhi
    | Hebrew
    | Japanese
    | Kannada
    | Khmer
    | Korean
    | Latin
    | LatinExt
    | Malayalam
    | Myanmar
    | Oriya
    | Sinhala
    | Tamil
    | Telugu
    | Thai
    | Tibetan
    | Vietnamese


subsetToString subset =
    case subset of
        Arabic ->
            "arabic"

        Bengali ->
            "bengali"

        ChineseHongkong ->
            "chinese-hongkong"

        ChineseSimplified ->
            "chinese-simplified"

        ChineseTraditional ->
            "chinese-traditional"

        Cyrillic ->
            "cyrillic"

        CyrillicExt ->
            "cyrillic-ext"

        Devanagari ->
            "devanagari"

        Greek ->
            "greek"

        GreekExt ->
            "greek-ext"

        Gujarati ->
            "gujarati"

        Gurmukhi ->
            "gurmukhi"

        Hebrew ->
            "hebrew"

        Japanese ->
            "japanese"

        Kannada ->
            "kannada"

        Khmer ->
            "khmer"

        Korean ->
            "korean"

        Latin ->
            "latin"

        LatinExt ->
            "latin-ext"

        Malayalam ->
            "malayalam"

        Myanmar ->
            "myanmar"

        Oriya ->
            "oriya"

        Sinhala ->
            "sinhala"

        Tamil ->
            "tamil"

        Telugu ->
            "telugu"

        Thai ->
            "thai"

        Tibetan ->
            "tibetan"

        Vietnamese ->
            "vietnamese"


encodeSubset =
    subsetToString >> Json.Encode.string


decodeSubset =
    let
        recover x =
            case x of
                "arabic" ->
                    Json.Decode.succeed Arabic

                "bengali" ->
                    Json.Decode.succeed Bengali

                "chinese-hongkong" ->
                    Json.Decode.succeed ChineseHongkong

                "chinese-simplified" ->
                    Json.Decode.succeed ChineseSimplified

                "chinese-traditional" ->
                    Json.Decode.succeed ChineseTraditional

                "cyrillic" ->
                    Json.Decode.succeed Cyrillic

                "cyrillic-ext" ->
                    Json.Decode.succeed CyrillicExt

                "devanagari" ->
                    Json.Decode.succeed Devanagari

                "greek" ->
                    Json.Decode.succeed Greek

                "greek-ext" ->
                    Json.Decode.succeed GreekExt

                "gujarati" ->
                    Json.Decode.succeed Gujarati

                "gurmukhi" ->
                    Json.Decode.succeed Gurmukhi

                "hebrew" ->
                    Json.Decode.succeed Hebrew

                "japanese" ->
                    Json.Decode.succeed Japanese

                "kannada" ->
                    Json.Decode.succeed Kannada

                "khmer" ->
                    Json.Decode.succeed Khmer

                "korean" ->
                    Json.Decode.succeed Korean

                "latin" ->
                    Json.Decode.succeed Latin

                "latin-ext" ->
                    Json.Decode.succeed LatinExt

                "malayalam" ->
                    Json.Decode.succeed Malayalam

                "myanmar" ->
                    Json.Decode.succeed Myanmar

                "oriya" ->
                    Json.Decode.succeed Oriya

                "sinhala" ->
                    Json.Decode.succeed Sinhala

                "tamil" ->
                    Json.Decode.succeed Tamil

                "telugu" ->
                    Json.Decode.succeed Telugu

                "thai" ->
                    Json.Decode.succeed Thai

                "tibetan" ->
                    Json.Decode.succeed Tibetan

                "vietnamese" ->
                    Json.Decode.succeed Vietnamese

                other ->
                    Json.Decode.fail <| "unknown constructor for type -sub-set: " ++ other
    in
    Json.Decode.string |> Json.Decode.andThen recover



-- other decoders / encoders


decodeFont =
    Json.Decode.map4
        Font
        (Json.Decode.field "family" Json.Decode.string)
        (Json.Decode.field "variants" (ZipList.decode decodeVariant))
        (Json.Decode.field "subsets" (Json.Decode.list decodeSubset))
        (Json.Decode.field "category" Json.Decode.string)


encodeFont a =
    Json.Encode.object
        [ ( "family", Json.Encode.string a.family )
        , ( "variants", ZipList.encode encodeVariant a.variants )
        , ( "subsets", Json.Encode.list encodeSubset a.subsets )
        , ( "category", Json.Encode.string a.category )
        ]
