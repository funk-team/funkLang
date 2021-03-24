module Spec.Element.Layout.Padding.V1 exposing (..)

{-| Padding akin to elm-ui
-}

import Json.Decode as Decode
import Spec.Element.Style.Edges



-- Padding in px


type PaddingMode
    = PaddingEach
    | PaddingXY
    | EqualPadding


type alias Padding =
    { mode : PaddingMode
    , dimensions : Spec.Element.Style.Edges.EdgeDimensions
    }


{-|

    deriveEachPadding {dimensions = {top = Just 0, bottom = Nothing, left = Just 10, right = Nothing}, mode = PaddingEach}
    --> {top = 0, bottom = 0, left = 10, right = 10}

-}
deriveEachPadding : Padding -> Spec.Element.Style.Edges.ConcreteEdgeDimensions
deriveEachPadding { dimensions } =
    deriveEdgeDimensions dimensions


deriveEdgeDimensions dimensions =
    let
        { top, bottom, left, right } =
            dimensions

        top_ =
            top |> Maybe.withDefault 0

        bottom_ =
            bottom |> Maybe.withDefault top_

        left_ =
            left |> Maybe.withDefault top_

        right_ =
            right |> Maybe.withDefault left_
    in
    Spec.Element.Style.Edges.ConcreteEdgeDimensions top_ bottom_ left_ right_


decodePadding =
    Decode.map2
        Padding
        (Decode.field "mode" decodePaddingMode)
        (Decode.field "dimensions" Spec.Element.Style.Edges.decodeEdgeDimensions)


decodePaddingMode =
    let
        recover x =
            case x of
                "PaddingEach" ->
                    Decode.succeed PaddingEach

                "PaddingXY" ->
                    Decode.succeed PaddingXY

                "EqualPadding" ->
                    Decode.succeed EqualPadding

                other ->
                    Decode.fail <| "Unknown constructor for type PaddingMode: " ++ other
    in
    Decode.string |> Decode.andThen recover
