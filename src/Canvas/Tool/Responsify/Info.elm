module Canvas.Tool.Responsify.Info exposing
    ( Dim(..)
    , Fact(..)
    , FactCalc
    , Info
    , Param(..)
    , ParamCalc
    , SimpleFactCalc
    , SimpleParamCalc
    , addDim
    , addFact
    , addParam
    , emptyInfo
    , encodeInfo
    , getDim
    , getFact
    , getParam
    , memoFact
    , memoParam
    , memoSimpleFact
    , memoSimpleParam
    )

import Canvas.Tool.Responsify.Types exposing (Drawn)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)



-- A Fact is a boolean-values piece of information about a layout
-- situation.


type Fact
    = AllSimilarHeight
    | AllSimilarWidth
    | AllSimilarWidthButLast
    | FirstLastSimilarWidth
    | NoOverlapInX
    | NoOverlapInY
    | AlignedInX
    | AlignedInY
    | EdgeAlignedInY
    | IsARow
    | IsATopBottomRow
    | IsAColumn
    | FirstAlignedLeft
    | FirstAlignedTop
    | LastAlignedRight
    | LastAlignedBottom
    | GroupHorizCentreAligned
    | GroupVertCentreAligned
    | InteriorGroupHorizCentreAligned
    | IndexHorizCentreAligned Int
    | IndexVertCentreAligned Int
    | AllHorizCentreAligned
    | AllVertCentreAligned
    | AllHorizSideAligned
    | SameTopOffset
    | SameBottomOffset
    | AllHorizFill
    | DistinctStartEndGroups
    | HorizEvenlySpaced
    | VertEvenlySpaced
    | InteriorGroupHorizEvenlySpaced



-- A Param is a piece of integer-valued information about a layout
-- situation.


type Param
    = StartGroupSize
    | EndGroupSize
      -- Gravity is (-1, 0, +1) depending on direction of
      -- "attraction": for X gravity, -1 means left, 0 means centred
      -- in X, +1 means right; for Y gravity, -1 means upwards, 0
      -- means centred in Y, +1 means down.
    | IndexXGravity Int
    | IndexYGravity Int



-- A Dim is a piece of float-valued information about a layout
-- situation.


type Dim
    = StartGroupMinSpacing
    | StartGroupMeanSpacing
    | StartGroupMaxSpacing
    | EndGroupMinSpacing
    | EndGroupMeanSpacing
    | EndGroupMaxSpacing
    | MinSpacing
    | MeanSpacing
    | MaxSpacing
    | InteriorGroupMeanSpacing



-- A comparable value for storing facts.


type alias FactKey =
    String



-- A comparable value for storing parameters.


type alias ParamKey =
    String



-- A comparable value for storing dimensions.


type alias DimKey =
    String



-- Not exported: used to generate comparable keys for Info dictionary.


factName : Fact -> FactKey
factName fact =
    case fact of
        AllSimilarHeight ->
            "all-similar-height"

        AllSimilarWidth ->
            "all-similar-width"

        AllSimilarWidthButLast ->
            "all-similar-width-but-last"

        FirstLastSimilarWidth ->
            "first-last-similar-width"

        IsARow ->
            "is-a-row"

        IsATopBottomRow ->
            "is-a-top-bottom-row"

        IsAColumn ->
            "is-a-column"

        NoOverlapInX ->
            "no-overlap-in-x"

        NoOverlapInY ->
            "no-overlap-in-y"

        AlignedInX ->
            "aligned-in-x"

        AlignedInY ->
            "aligned-in-y"

        EdgeAlignedInY ->
            "edge-aligned-in-y"

        FirstAlignedLeft ->
            "first-aligned-left"

        FirstAlignedTop ->
            "first-aligned-top"

        LastAlignedRight ->
            "last-aligned-right"

        LastAlignedBottom ->
            "last-aligned-bottom"

        IndexHorizCentreAligned idx ->
            "index-horiz-centre-aligned-" ++ String.fromInt idx

        IndexVertCentreAligned idx ->
            "index-vert-centre-aligned-" ++ String.fromInt idx

        AllHorizCentreAligned ->
            "all-horiz-centre-aligned"

        AllVertCentreAligned ->
            "all-vert-centre-aligned"

        GroupHorizCentreAligned ->
            "group-horiz-centre-aligned"

        GroupVertCentreAligned ->
            "group-vert-centre-aligned"

        InteriorGroupHorizCentreAligned ->
            "interior-group-horiz-centre-aligned"

        AllHorizSideAligned ->
            "all-horiz-side-aligned"

        SameTopOffset ->
            "same-top-offset"

        SameBottomOffset ->
            "same-bottom-offset"

        AllHorizFill ->
            "all-horiz-fill"

        DistinctStartEndGroups ->
            "distinct-start-end-groups"

        HorizEvenlySpaced ->
            "horiz-evenly-spaced"

        VertEvenlySpaced ->
            "vert-evenly-spaced"

        InteriorGroupHorizEvenlySpaced ->
            "interior-group-horiz-evenly-spaced"



-- Not exported: used to generate comparable keys for Info dictionary.


paramName : Param -> ParamKey
paramName param =
    case param of
        StartGroupSize ->
            "start-group-size"

        EndGroupSize ->
            "end-group-size"

        IndexXGravity idx ->
            "index-x-gravity-" ++ String.fromInt idx

        IndexYGravity idx ->
            "index-y-gravity-" ++ String.fromInt idx



-- Not exported: used to generate comparable keys for Info dictionary.


dimName : Dim -> ParamKey
dimName dim =
    case dim of
        StartGroupMinSpacing ->
            "start-group-min-spacing"

        StartGroupMeanSpacing ->
            "start-group-mean-spacing"

        StartGroupMaxSpacing ->
            "start-group-max-spacing"

        EndGroupMinSpacing ->
            "end-group-min-spacing"

        EndGroupMeanSpacing ->
            "end-group-mean-spacing"

        EndGroupMaxSpacing ->
            "end-group-max-spacing"

        MinSpacing ->
            "min-spacing"

        MeanSpacing ->
            "mean-spacing"

        MaxSpacing ->
            "max-spacing"

        InteriorGroupMeanSpacing ->
            "interior-group-mean-spacing"




-- The current information we have about the layout situation as
-- derived during the rule evaluation process. This information is
-- maintained as we evaluate rules to avoid repeatedly recalculating
-- facts from the layout.


type alias Info =
    { facts : Dict FactKey Bool
    , params : Dict ParamKey Int
    , dims : Dict DimKey Float
    }


encodeInfo : Info -> Value
encodeInfo info =
    Encode.object
        [ ( "facts", Encode.dict identity Encode.bool info.facts )
        , ( "params", Encode.dict identity Encode.int info.params )
        , ( "dims", Encode.dict identity Encode.float info.dims )
        ]



-- Initial empty information store.


emptyInfo : Info
emptyInfo =
    Info Dict.empty Dict.empty Dict.empty



-- What we really want:
--
--  * To store a finite set of "facts", each of which is a boolean
--    flag.
--  * To store a finite set of "parameters", each of which is a
--    integer value.
--  * To store a finite set of "dimensions", each of which is a
--    floating point numeric value.
--  * To be able to determine whether we've already stored a
--    particular fact, parameter or dimension.
--  * To add new facts, parameters and dimensions. (Dimensions are so
--    far calculated only as part of the process of processing facts,
--    which is why there is no separate memoDim function.)
--
--
-- Add a fact.


addFact : Fact -> Bool -> Info -> Info
addFact fact val info =
    { info | facts = Dict.insert (factName fact) val info.facts }



-- Add a parameter.


addParam : Param -> Int -> Info -> Info
addParam param val info =
    { info | params = Dict.insert (paramName param) val info.params }



-- Add a dimension.


addDim : Dim -> Float -> Info -> Info
addDim dim val info =
    { info | dims = Dict.insert (dimName dim) val info.dims }



-- Retrieve a fact.


getFact : Fact -> Info -> Maybe Bool
getFact fact info =
    Dict.get (factName fact) info.facts



-- Retrieve a parameter.


getParam : Param -> Info -> Maybe Int
getParam param info =
    Dict.get (paramName param) info.params



-- Retrieve a dimension.


getDim : Dim -> Info -> Maybe Float
getDim dim info =
    Dict.get (dimName dim) info.dims



-- Functions that calculate simple fact values, without caching of
-- intermediate results.


type alias SimpleFactCalc =
    Info -> Drawn -> Bool



-- Functions that calculate fact values, potentially making use of
-- intermediate facts that they can save.


type alias FactCalc =
    Info -> Drawn -> ( Bool, Info )



-- Functions that calculate simple parameter values, without caching
-- of intermediate results.


type alias SimpleParamCalc =
    Info -> Drawn -> Int



-- Functions that calculate parameter values, potentially making use
-- of intermediate facts that they can save.


type alias ParamCalc =
    Info -> Drawn -> ( Int, Info )



-- Memoise a fact calculation that invokes (and so could cache)
-- intermediate facts.


memoFact : Fact -> FactCalc -> FactCalc
memoFact fact calc info data =
    case getFact fact info of
        Just result ->
            ( result, info )

        Nothing ->
            let
                ( result, info2 ) =
                    calc info data
            in
            ( result, addFact fact result info2 )



-- Memoise a simple fact calculation.


memoSimpleFact : Fact -> SimpleFactCalc -> FactCalc
memoSimpleFact fact calc info data =
    case getFact fact info of
        Just result ->
            ( result, info )

        Nothing ->
            let
                result =
                    calc info data
            in
            ( result, addFact fact result info )



-- Memoise a parameter calculation that invokes (and so could cache)
-- intermediate info.


memoParam : Param -> ParamCalc -> ParamCalc
memoParam param calc info data =
    case getParam param info of
        Just result ->
            ( result, info )

        Nothing ->
            let
                ( result, info2 ) =
                    calc info data
            in
            ( result, addParam param result info2 )



-- Memoise a simple parameter calculation.


memoSimpleParam : Param -> SimpleParamCalc -> ParamCalc
memoSimpleParam param calc info data =
    case getParam param info of
        Just result ->
            ( result, info )

        Nothing ->
            let
                result =
                    calc info data
            in
            ( result, addParam param result info )
