module Spec.Element.Layout.Padding exposing (..)

{-| Padding akin to elm-ui
-}

import Json.Decode as Decode
import Json.Encode as Encode
import Spec.Element.Layout.Padding.V1
import Spec.Element.Style.Edges


type alias Padding =
    Padding_


encodePadding =
    encodePadding_


decodePadding =
    Decode.oneOf
        [ decodePadding_
        , Spec.Element.Layout.Padding.V1.decodePadding
            |> Decode.map migrateFromV1
        ]


migrateFromV1 : Spec.Element.Layout.Padding.V1.Padding -> Padding
migrateFromV1 { mode, dimensions } =
    case mode of
        Spec.Element.Layout.Padding.V1.PaddingEach ->
            PaddingEach dimensions

        Spec.Element.Layout.Padding.V1.PaddingXY ->
            PaddingXY dimensions.right dimensions.top

        Spec.Element.Layout.Padding.V1.EqualPadding ->
            EqualPadding dimensions.top


{-| The simplest possible padding setting
-}
defaultPadding : Padding
defaultPadding =
    EqualPadding Nothing



-- TODO: improve transitions


paddingModes : Padding -> List Padding
paddingModes padding =
    let
        { top, bottom, left, right } =
            deriveEachPadding padding
    in
    [ EqualPadding (Just top)
    , PaddingXY (Just left) (Just top)
    , PaddingEach
        { top = Just top
        , bottom = Just bottom
        , left = Just left
        , right = Just right
        }
    ]



-- Padding in px
-- [generator-start]


type Padding_
    = PaddingEach Spec.Element.Style.Edges.EdgeDimensions
    | PaddingXY (Maybe Int) (Maybe Int)
    | EqualPadding (Maybe Int)



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodePadding_ =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodePadding_Help


decodePadding_Help constructor =
    case constructor of
        "PaddingEach" ->
            Decode.map
                PaddingEach
                (Decode.field "A1" Spec.Element.Style.Edges.decodeEdgeDimensions)

        "PaddingXY" ->
            Decode.map2
                PaddingXY
                (Decode.field "A1" (Decode.maybe Decode.int))
                (Decode.field "A2" (Decode.maybe Decode.int))

        "EqualPadding" ->
            Decode.map
                EqualPadding
                (Decode.field "A1" (Decode.maybe Decode.int))

        other ->
            Decode.fail <| "Unknown constructor for type Padding_: " ++ other


encodeMaybeInt a =
    case a of
        Just b ->
            Encode.int b

        Nothing ->
            Encode.null


encodePadding_ a =
    case a of
        PaddingEach a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "PaddingEach" )
                , ( "A1", Spec.Element.Style.Edges.encodeEdgeDimensions a1 )
                ]

        PaddingXY a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "PaddingXY" )
                , ( "A1", encodeMaybeInt a1 )
                , ( "A2", encodeMaybeInt a2 )
                ]

        EqualPadding a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "EqualPadding" )
                , ( "A1", encodeMaybeInt a1 )
                ]



-- [generator-end]


paddingLabel : Padding -> String
paddingLabel mode =
    case mode of
        EqualPadding _ ->
            "EqualPadding"

        PaddingXY _ _ ->
            "PaddingXY"

        PaddingEach _ ->
            "PaddingEach"


paddingDescription : Padding -> String
paddingDescription mode =
    case mode of
        EqualPadding _ ->
            "Each padding has a unique value"

        PaddingXY _ _ ->
            "Only set horizontal and vertical padding"

        PaddingEach _ ->
            "All padding values are equal"


{-|

    deriveEachPadding
        (PaddingEach {top = Just 5, bottom = Nothing, left = Just 10, right = Nothing})
    --> {top = 5, bottom = 0, left = 10, right = 0}

    deriveEachPadding
        (PaddingXY (Just 5) (Just 10))
    --> {top = 10, bottom = 10, left = 5, right = 5}

    deriveEachPadding
        (PaddingXY Nothing Nothing)
    --> {top =  0, bottom =  0, left = 0, right = 0}

-}
deriveEachPadding : Padding -> Spec.Element.Style.Edges.ConcreteEdgeDimensions
deriveEachPadding padding =
    case padding of
        EqualPadding num ->
            { top = num |> Maybe.withDefault 0
            , bottom = num |> Maybe.withDefault 0
            , left = num |> Maybe.withDefault 0
            , right = num |> Maybe.withDefault 0
            }

        PaddingXY x y ->
            { top = y |> Maybe.withDefault 0
            , bottom = y |> Maybe.withDefault 0
            , left = x |> Maybe.withDefault 0
            , right = x |> Maybe.withDefault 0
            }

        PaddingEach { top, bottom, left, right } ->
            { top = top |> Maybe.withDefault 0
            , bottom = bottom |> Maybe.withDefault 0
            , left = left |> Maybe.withDefault 0
            , right = right |> Maybe.withDefault 0
            }


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
