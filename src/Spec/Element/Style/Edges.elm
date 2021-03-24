module Spec.Element.Style.Edges exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


{-| Similar to the CSS shorthad for padding, margin, border-width etc
-}
type alias ConcreteEdgeDimensions =
    { top : Int, bottom : Int, left : Int, right : Int }


{-|

    deriveEdgeDimensions {top = Just 0, bottom = Nothing, left = Just 10, right = Nothing}
    --> {top = 0, bottom = 0, left = 10, right = 10}

-}
deriveEdgeDimensions : EdgeDimensions -> ConcreteEdgeDimensions
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
    ConcreteEdgeDimensions top_ bottom_ left_ right_


init : EdgeDimensions
init =
    { top = Nothing
    , bottom = Nothing
    , left = Nothing
    , right = Nothing
    }



-- [generator-start]


type alias EdgeDimensions =
    { top : Maybe Int
    , bottom : Maybe Int
    , left : Maybe Int
    , right : Maybe Int
    }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeEdgeDimensions =
    Decode.map4
        EdgeDimensions
        (Decode.field "top" (Decode.maybe Decode.int))
        (Decode.field "bottom" (Decode.maybe Decode.int))
        (Decode.field "left" (Decode.maybe Decode.int))
        (Decode.field "right" (Decode.maybe Decode.int))


encodeEdgeDimensions a =
    Encode.object
        [ ( "top", encodeMaybeInt a.top )
        , ( "bottom", encodeMaybeInt a.bottom )
        , ( "left", encodeMaybeInt a.left )
        , ( "right", encodeMaybeInt a.right )
        ]


encodeMaybeInt a =
    case a of
        Just b ->
            Encode.int b

        Nothing ->
            Encode.null



-- [generator-end]
