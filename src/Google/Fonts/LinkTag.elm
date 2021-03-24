module Google.Fonts.LinkTag exposing (displayedFontLinkTags)

{-| This module is used to generate the link tags to the fonts used in the app

note: We spell out special characters _comma_ in this module because
Url.Builder.Absolute escapes them but the googlefonts API URLs require them to
work.

-}

import DesignSystem.Typography
import Google.Fonts exposing (Font)
import Html
import Html.Attributes
import List.Extra
import Model.Model
import Spec.Element.Id
import Spec.Element.Style
import Url.Builder
import ZipList


type alias SimpleFont =
    { family : Google.Fonts.Family
    , variants : List Google.Fonts.Variant

    -- , subsets : List Google.Fonts.Subset
    }


aggregateFamily : ( Font, List Font ) -> SimpleFont
aggregateFamily ( first, rest ) =
    { family = first.family
    , variants =
        ZipList.current first.variants
            :: List.map (\font -> ZipList.current font.variants) rest
            |> List.Extra.uniqueBy Google.Fonts.variantToString
    }


{-| This combines fonts with different selected weights and subsets into a superset.
It functions like a union operator.

    aggregate
        [ {family = "Fam", variants = [W700italic], subset = Languages [LatinExt]}
        , {family = "Fam", variants = [W600], subset = Languages [Latin]}
        , {family = "Fam2", variants = [W700italic], subset = Text "Another Family"}
        ]
    --> [{ family = "Fam", subset = Languages [Latin,LatinExt], variants = [W700italic,W600] },{ family = "Fam2", subset = Text "Another Family", variants = [W700italic] }]

-}
aggregate : List Font -> List SimpleFont
aggregate =
    List.sortBy .family
        >> List.Extra.groupWhile (\a b -> a.family == b.family)
        >> List.map aggregateFamily


{-| Make the url that you need to give to the link element.

    makeFamilyUrl {family = "Another Family", variants = [W700italic], subset = Text "Another Family"}
    --> "https://fonts.googleapis.com/css2?family=Another+Family:ital,wght@1,700&text=Another%20Family"

    yep. google fonts API encodes spaces in font names with '+'

-}
makeFamilyUrl : SimpleFont -> String
makeFamilyUrl family =
    let
        baseUrl =
            "https://fonts.googleapis.com"

        familyParam =
            renderOneFamily family
                |> Url.Builder.string "family"
    in
    Url.Builder.crossOrigin baseUrl [ "css2" ] [ familyParam ]
        |> String.replace "_equals_" "="
        |> String.replace "_ampersand_" "&"
        |> String.replace "_pipe_" "|"
        |> String.replace "_plus_" "+"
        |> String.replace "_colon_" ":"
        |> String.replace "_comma_" ","
        |> String.replace "_semicolon_" ";"
        |> String.replace "_at_" "@"


spaceToPlus : String -> String
spaceToPlus =
    String.split " "
        >> String.join "_plus_"


{-| Render the string telling the google fonts API to retrieve the right stylesheet r a given font
-}
renderOneFamily : SimpleFont -> String
renderOneFamily { family, variants } =
    let
        encodedVariants =
            case variants of
                [] ->
                    ""

                _ ->
                    "_colon_ital_comma_wght_at_"
                        ++ String.join "_semicolon_" (variants |> List.map variantToMatrix |> List.sort)
    in
    spaceToPlus family ++ encodedVariants


render : List Font -> Html.Html msg
render families =
    let
        oneTag family =
            Html.node
                "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.attribute
                    "href"
                    (makeFamilyUrl family)
                ]
                []
    in
    families
        |> aggregate
        |> List.map oneTag
        |> Html.div []


extractCustomStyles : Spec.Element.Style.Style -> List Font
extractCustomStyles style =
    let
        get t =
            case t of
                Spec.Element.Style.NoTypo ->
                    Nothing

                Spec.Element.Style.CustomTypo custom ->
                    Just custom.font

                Spec.Element.Style.TypoFromDesignSystem _ ->
                    Nothing
    in
    [ style.elementText, style.placeholderText ]
        |> List.filterMap get


displayedFontLinkTags : Model.Model.UserModel -> Html.Html msg
displayedFontLinkTags userModel =
    let
        typoSystem =
            userModel.designSystem.typoEditor

        -- required for when the user sets up a style that is detached from the designsystem
        nonDesignSystemTypos =
            userModel.elementStyles
                |> Spec.Element.Id.dictToList
                |> List.map Tuple.second
                |> List.concatMap extractCustomStyles

        designSystemTypos =
            DesignSystem.Typography.allDisplayedFonts typoSystem

        allFonts =
            designSystemTypos ++ nonDesignSystemTypos
    in
    render allFonts


variantToMatrix v =
    case v of
        Google.Fonts.Regular ->
            "0_comma_400"

        Google.Fonts.Italic ->
            "1_comma_400"

        Google.Fonts.W100 ->
            "0_comma_100"

        Google.Fonts.W100italic ->
            "1_comma_100"

        Google.Fonts.W200 ->
            "0_comma_200"

        Google.Fonts.W200italic ->
            "1_comma_200"

        Google.Fonts.W300 ->
            "0_comma_300"

        Google.Fonts.W300italic ->
            "1_comma_300"

        Google.Fonts.W400 ->
            "0_comma_400"

        Google.Fonts.W400italic ->
            "1_comma_400"

        Google.Fonts.W500 ->
            "0_comma_500"

        Google.Fonts.W500italic ->
            "1_comma_500"

        Google.Fonts.W600 ->
            "0_comma_600"

        Google.Fonts.W600italic ->
            "1_comma_600"

        Google.Fonts.W700 ->
            "0_comma_700"

        Google.Fonts.W700italic ->
            "1_comma_700"

        Google.Fonts.W800 ->
            "0_comma_800"

        Google.Fonts.W800italic ->
            "1_comma_800"

        Google.Fonts.W900 ->
            "0_comma_900"

        Google.Fonts.W900italic ->
            "1_comma_900"
