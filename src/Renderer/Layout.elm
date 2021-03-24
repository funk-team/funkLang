module Renderer.Layout exposing (..)

import Element
import Interface.Data
import Interface.Model
import Model.Model
import Renderer.Help
import Spec
import Spec.Element.Id
import Spec.Element.Layout
import Spec.Element.Layout.Padding
import Spec.Element.Model
import Spec.Element.Style
import Spec.Model


addToOuter : List (Element.Attribute msg) -> Attribs msg -> Attribs msg
addToOuter additional { inner, outer } =
    { outer = additional ++ outer, inner = inner }


type alias Attribs msg =
    { outer : List (Element.Attribute msg)
    , inner : List (Element.Attribute msg)
    }


type alias ElmUiRenderFunction msg =
    Attribs msg
    -> List (Element.Element msg)
    -> Element.Element msg


{-| Version kept for responsify to prevent ripples and merge conflicts with other branch
-}
getElmUiElementByLayout_ :
    Spec.Element.Layout.Flow
    -> List (Element.Attribute msg)
    -> List (Element.Element msg)
    -> Element.Element msg
getElmUiElementByLayout_ layout =
    case layout of
        Spec.Element.Layout.Row ->
            Element.row

        Spec.Element.Layout.WrappedRow ->
            Element.wrappedRow

        Spec.Element.Layout.Column ->
            Element.column


renderShared :
    Spec.Element.Model.Shared
    ->
        { attribs : List (Element.Attribute msg)
        , el : ElmUiRenderFunction msg
        }
renderShared shared =
    let
        spacing =
            case shared.spacing of
                Nothing ->
                    []

                Just s ->
                    [ Element.spacing s ]

        el =
            getElmUiElementByLayout shared.flow
    in
    { el = el, attribs = spacing }


formatPadding : Spec.Element.Layout.Padding.Padding -> { bottom : Int, left : Int, right : Int, top : Int }
formatPadding padding =
    Spec.Element.Layout.Padding.deriveEachPadding padding


getElmUiElementByLayout : Spec.Element.Layout.Flow -> ElmUiRenderFunction msg
getElmUiElementByLayout flow =
    case flow of
        Spec.Element.Layout.Row ->
            \layoutAttribs children ->
                Element.row (flattenAttribs layoutAttribs) children

        Spec.Element.Layout.WrappedRow ->
            \layoutAttribs children ->
                Element.wrappedRow (Element.centerX :: layoutAttribs.inner) children
                    |> Element.el layoutAttribs.outer

        Spec.Element.Layout.Column ->
            \layoutAttribs children ->
                Element.column (flattenAttribs layoutAttribs) children


flattenAttribs { outer, inner } =
    outer ++ inner



-- STYLING HELPERS


{-| Generate elm-ui attribs for alignment
TODO: Fix top alignment
-}
makeAlignmentAttribs : Spec.Element.Layout.Alignment -> List (Element.Attribute msg)
makeAlignmentAttribs { x, y } =
    [ case x of
        Just Spec.Element.Layout.Left ->
            [ Element.alignLeft ]

        Just Spec.Element.Layout.CenterX ->
            [ Element.centerX
            ]

        Just Spec.Element.Layout.Right ->
            [ Element.alignRight ]

        Nothing ->
            []
    , case y of
        Just Spec.Element.Layout.Top ->
            [ Element.alignTop ]

        -- fix strange alignment
        Just Spec.Element.Layout.CenterY ->
            [ Element.centerY ]

        Just Spec.Element.Layout.Bottom ->
            [ Element.alignBottom ]

        Nothing ->
            []
    ]
        |> List.concat


{-| When the
-}
makeSizeAttribs :
    Model.Model.UserModel
    -> Spec.Element.Id.Id
    -> Interface.Model.ScopeData
    -> Spec.Element.Layout.Size
    -> List (Element.Attribute msg)
makeSizeAttribs userModel elementId scope { width, height } =
    let
        autoSized =
            autoSizeApplies userModel elementId scope

        renderedWidth =
            if autoSized.width then
                Element.width Element.shrink

            else
                Element.width <| makeLength userModel scope width

        renderedHeight =
            if autoSized.height then
                Element.height Element.shrink

            else
                Element.height <| makeLength userModel scope height
    in
    [ renderedWidth
    , renderedHeight
    ]


{-| Based on content rendered, determine if an element has auto width or height
-}
autoSizeApplies :
    Model.Model.UserModel
    -> Spec.Element.Id.Id
    -> Interface.Model.ScopeData
    -> { width : Bool, height : Bool }
autoSizeApplies userModel elementId scope =
    let
        refinedValue =
            Renderer.Help.getContent
                elementId
                userModel
                scope

        style =
            Spec.getStyle elementId userModel

        supportsObjectFit =
            Maybe.map
                .value
                refinedValue
                |> Interface.Data.supportsObjectFit

        ( width, height ) =
            case ( style.imageCropMode, supportsObjectFit ) of
                ( Spec.Element.Style.AutoWidth, True ) ->
                    ( True, False )

                ( Spec.Element.Style.AutoHeight, True ) ->
                    ( False, True )

                _ ->
                    ( False, False )
    in
    { width = width, height = height }


{-| Bake length attribs for elm-ui
-}
makeLength : Spec.Model.WithSpec userModel -> Interface.Model.ScopeData -> Spec.Element.Layout.Length -> Element.Length
makeLength userModel scope { behavior, minMax } =
    let
        { min, max } =
            minMax

        min_ =
            case Spec.resolveNullable userModel scope min of
                Nothing ->
                    identity

                Just resolvedMinimum ->
                    Element.minimum (round resolvedMinimum)

        max_ =
            case Spec.resolveNullable userModel scope max of
                Nothing ->
                    identity

                Just resolvedMaximum ->
                    Element.maximum (round resolvedMaximum)
    in
    case behavior of
        Spec.Element.Layout.Static length ->
            Element.px (Spec.resolve userModel scope length |> round)

        Spec.Element.Layout.Fill ->
            Element.fill |> min_ |> max_

        Spec.Element.Layout.Shrink ->
            Element.shrink |> min_ |> max_
