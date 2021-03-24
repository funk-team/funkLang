module Canvas.Tool.Responsify.Facts.Spacing exposing
    ( distinctStartEndGroups
    , horizEvenlySpaced
    , interiorGroupHorizEvenlySpaced
    , vertEvenlySpaced
    )

import Canvas.Tool.Responsify.Facts.Alignment exposing (firstAlignedLeft, lastAlignedRight)
import Canvas.Tool.Responsify.Facts.Layouts exposing (isAColumn, isARow, isATopBottomRow)
import Canvas.Tool.Responsify.Info exposing (Dim(..), Fact(..), FactCalc, Info, Param(..), addDim, addFact, addParam, getDim, getFact, getParam, memoFact)
import Canvas.Tool.Responsify.Types exposing (Drawn)
import Canvas.Tool.Responsify.Utils exposing (allSimilarWith, interleave, sideBySideSort)
import Rectangle exposing (Rectangle(..), similar)
import Spec.Element.Id exposing (Id(..))


{-| Fact: distinct evenly spaced start and end groups with gap between.

How to do this:

1.  Decide whether it's a row, a column, or neither. If neither,
    return false.

    1.  Sort the children along the major axis (X for row, Y for
        column).

    2.  Determine the gaps between the children along the major axis.

    3.  Break the elements into groups based on the gaps between them.

    4.  If there is only one group, it needs to be aligned either left
        or right. If there are two groups, one needs to be aligned
        left and one right. Any other arrangement doesn't correspond
        to distinct start and end groups.

    Steps from 2 onwards are handed off to a single "analyseSpacing"
    function that collects all the relevant grouping information at
    once.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)

    Tuple.first (distinctStartEndGroups emptyInfo F.filledRow)
    --> False
    Tuple.first (distinctStartEndGroups emptyInfo F.gappedRow)
    --> True

-}
distinctStartEndGroups : FactCalc
distinctStartEndGroups =
    memoFact DistinctStartEndGroups <|
        \info data ->
            let
                ( row, info2 ) =
                    isARow info data

                ( tbRow, info3 ) =
                    isATopBottomRow info2 data

                ( col, info4 ) =
                    isAColumn info3 data
            in
            if not (row || tbRow) && not col || (row || tbRow) && col then
                ( False, info4 )

            else
                let
                    dir =
                        if row || tbRow then
                            Row

                        else
                            Column

                    info5 =
                        analyseSpacing dir info4 data
                in
                case getFact DistinctStartEndGroups info5 of
                    Nothing ->
                        ( False, info5 )

                    Just distinct ->
                        ( distinct, info5 )


horizEvenlySpaced : FactCalc
horizEvenlySpaced =
    memoFact HorizEvenlySpaced <|
        \info data ->
            let
                ( row, info2 ) =
                    isARow info data
            in
            if not row then
                ( False, info2 )

            else
                let
                    info3 =
                        analyseSpacing Row info2 data
                in
                case getFact HorizEvenlySpaced info3 of
                    Nothing ->
                        ( False, info3 )

                    Just even ->
                        ( even, info3 )


vertEvenlySpaced : FactCalc
vertEvenlySpaced =
    memoFact VertEvenlySpaced <|
        \info data ->
            let
                ( column, info2 ) =
                    isAColumn info data
            in
            if not column then
                ( False, info2 )

            else
                let
                    info3 =
                        analyseSpacing Column info2 data
                in
                case getFact VertEvenlySpaced info3 of
                    Nothing ->
                        ( False, info3 )

                    Just even ->
                        ( even, info3 )


interiorGroupHorizEvenlySpaced : FactCalc
interiorGroupHorizEvenlySpaced =
    memoFact InteriorGroupHorizEvenlySpaced <|
        \info data ->
            let
                involved =
                    List.sortBy Rectangle.x1 data.children
                        |> List.drop 1
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse

                spaces =
                    List.map2 (\right left -> Rectangle.x1 right - Rectangle.x2 left)
                        (List.drop 1 involved)
                        involved

                meanSpacing =
                    if List.length spaces == 0 then
                        0

                    else
                        List.sum spaces / toFloat (List.length spaces)
            in
            if allSimilarWith similar spaces then
                ( True, addDim InteriorGroupMeanSpacing meanSpacing info )

            else
                ( False, info )



-- We're going to try to group space elements into groups as either a
-- row or a column. Which we're doing determines sort criteria and how
-- we measure gaps.


type GroupingDirection
    = Row
    | Column



-- Start, end or indeterminate group?


type Group
    = Start
    | End
    | Indeterminate



-- Analyse row and column grouping and spacing.


analyseSpacing : GroupingDirection -> Info -> Drawn -> Info
analyseSpacing direction info data =
    let
        -- Sort the children along the major axis (X for row, Y for
        -- column), sorting child IDs in parallel.
        sorted : List ( Rectangle, Id )
        sorted =
            sideBySideSort (sortCriterion direction) data.children data.childIds

        -- Determine the gaps between the children along the major
        -- axis.
        gaps =
            findGaps direction data.parent sorted

        -- Determine groupings of child elements.
        groups =
            findGroups gaps

        -- Determine whether the child elements are evenly spaced.
        isEven =
            evenlySpaced gaps

        -- Check alignment of first and last elements with respect to
        -- parent.
        ( leftAligned, info2 ) =
            firstAlignedLeft info data

        ( rightAligned, info3 ) =
            lastAlignedRight info2 data

        fail =
            addFact DistinctStartEndGroups False
    in
    case groups of
        [ group ] ->
            -- If we have a single group, it must be either
            -- left-aligned or right aligned, but not both.
            fail info3
                |> addEvennessInfo direction isEven
                |> (case ( leftAligned, rightAligned ) of
                        ( True, False ) ->
                            addGroupInfo direction Start group sorted >> addMeanSpacing

                        ( False, True ) ->
                            addGroupInfo direction End group sorted >> addMeanSpacing

                        _ ->
                            addGroupInfo direction Indeterminate group sorted
                   )

        [ startGroup, endGroup ] ->
            -- If we have two groups, we require the left-hand
            -- group to be left-aligned and the right-hand group
            -- to be right aligned.
            if leftAligned && rightAligned then
                info3
                    |> addFact DistinctStartEndGroups True
                    |> addGroupInfo direction Start startGroup sorted
                    |> addGroupInfo direction End endGroup sorted
                    |> addEvennessInfo direction isEven
                    |> addMeanSpacing

            else
                fail info3
                    |> addEvennessInfo direction isEven

        _ ->
            -- If we have more than two groups, then this isn't a
            -- "distinct groups" scenario we can handle here.
            fail info3
                |> addEvennessInfo direction isEven



-- Calculate mean spacing between all elements, taking account of
-- grouping.


addMeanSpacing : Info -> Info
addMeanSpacing info =
    case ( getDim StartGroupMeanSpacing info, getDim EndGroupMeanSpacing info ) of
        ( Just s, Just e ) ->
            case ( getParam StartGroupSize info, getParam EndGroupSize info ) of
                ( Just sszp, Just eszp ) ->
                    let
                        ssz =
                            toFloat sszp

                        esz =
                            toFloat eszp

                        num =
                            (ssz - 1) * s + (esz - 1) * e

                        denom =
                            ssz + esz - 2
                    in
                    if denom == 0 then
                        info

                    else
                        addDim MeanSpacing (num / denom) info

                _ ->
                    info

        ( Just s, Nothing ) ->
            addDim MeanSpacing s info

        ( Nothing, Just e ) ->
            addDim MeanSpacing e info

        ( Nothing, Nothing ) ->
            info



-- Pull rectangles out for spacing summary calculation: both the ID
-- and the (ID, rectangle) lists must be sorted in the major axis
-- direction when this is called.


extractRects : List Id -> List ( Rectangle, Id ) -> List Rectangle
extractRects ids data =
    case ( ids, data ) of
        ( id :: idrest, ( r, rid ) :: rs ) ->
            if id == rid then
                r :: extractRects idrest rs

            else
                extractRects ids rs

        _ ->
            []



-- Add information about the evenness of group spacing to information
-- dictionary.


addEvennessInfo : GroupingDirection -> Bool -> Info -> Info
addEvennessInfo dir even info =
    let
        fact =
            case dir of
                Row ->
                    HorizEvenlySpaced

                _ ->
                    VertEvenlySpaced
    in
    addFact fact even info



-- Add information about group size and spacing to information
-- dictionary for an identified group of child elements.


addGroupInfo : GroupingDirection -> Group -> List Id -> List ( Rectangle, Id ) -> Info -> Info
addGroupInfo dir group ids sorted info =
    let
        ( ( grpSize, grpMean ), ( grpMin, grpMax ) ) =
            case group of
                Start ->
                    ( ( Just StartGroupSize, StartGroupMeanSpacing )
                    , ( StartGroupMinSpacing, StartGroupMaxSpacing )
                    )

                End ->
                    ( ( Just EndGroupSize, EndGroupMeanSpacing )
                    , ( EndGroupMinSpacing, EndGroupMaxSpacing )
                    )

                Indeterminate ->
                    ( ( Nothing, MeanSpacing )
                    , ( MinSpacing, MaxSpacing )
                    )

        rects =
            extractRects ids sorted

        gaps =
            basicGaps dir identity rects

        sizer =
            case grpSize of
                Just szparam ->
                    addParam szparam (List.length ids)

                Nothing ->
                    identity
    in
    info
        |> addDim grpMin (Maybe.withDefault 0.0 <| List.minimum gaps)
        |> addDim grpMean
            (if List.length gaps == 0 then
                0

             else
                List.sum gaps / toFloat (List.length gaps)
            )
        |> addDim grpMax (Maybe.withDefault 0.0 <| List.maximum gaps)
        |> sizer



-- How to sort elements that are supposed to be in a row or column for
-- gap determination.


sortCriterion : GroupingDirection -> (Rectangle -> Float)
sortCriterion dir =
    case dir of
        Row ->
            -- Sort left edges.
            Rectangle.x1

        Column ->
            -- Sort top edges.
            Rectangle.y1



-- We use these things as an intermediate step in the analysis of
-- child elements in a row or column into groups. Keep track of IDs
-- and rectangles for content during this process so that we can
-- associate IDs to groups.


type Piece
    = Gap Float
    | Content Id Float



-- Generate list of "pieces", i.e. gaps between elements and sizing
-- for elements, in order of a potential row or column layout: for N
-- elements, there are N+1 gaps, counting the gaps at either end to
-- the boundary of the parent rectangle.


findGaps : GroupingDirection -> Rectangle -> List ( Rectangle, Id ) -> List Piece
findGaps dir parent rs =
    let
        ( sizer, start, end ) =
            case dir of
                Row ->
                    -- Elements are sized by width; fake start and end
                    -- elements are made from the x-coordinates of the
                    -- parent rectangle.
                    ( Rectangle.width
                    , ( Rectangle
                            { x1 = Rectangle.x1 parent - 1
                            , x2 = Rectangle.x1 parent
                            , y1 = 1
                            , y2 = 2
                            }
                      , Id -1
                      )
                    , ( Rectangle
                            { x1 = Rectangle.x2 parent
                            , x2 = Rectangle.x2 parent + 1
                            , y1 = 1
                            , y2 = 2
                            }
                      , Id -2
                      )
                    )

                Column ->
                    -- Elements are sized by height; fake start and
                    -- end elements are made from the y-coordinates of
                    -- the parent rectangle.
                    ( Rectangle.height
                    , ( Rectangle
                            { x1 = 1
                            , x2 = 2
                            , y1 = Rectangle.y1 parent - 1
                            , y2 = Rectangle.y1 parent
                            }
                      , Id -1
                      )
                    , ( Rectangle
                            { x1 = 1
                            , x2 = 2
                            , y1 = Rectangle.y2 parent
                            , y2 = Rectangle.y2 parent + 1
                            }
                      , Id -2
                      )
                    )

        -- Add fake start and end rectangles to make gap calculation
        -- uniform.
        rsWithEnds =
            start :: rs ++ [ end ]

        -- Find gaps between adjacent sorted elements, including fake
        -- elements at ends.
        gaps =
            List.map Gap <| basicGaps dir Tuple.first rsWithEnds

        -- Find size of each piece of content.
        content =
            List.map (\( r, id ) -> Content id (sizer r)) rs
    in
    -- Gaps and content in the order they occur.
    interleave gaps content


{-| Find groups of elements within rows or columns by looking at
spacing relative to element sizes. An element is considered to be
in the same group as the preceding element if the gap between them
is less than half the width of the element.

    TEST DISABLED BECAUSE IT USES PRIVATE API

    import Canvas.Tool.Responsify.Fixtures as F

    List.length <| findGroups <|
        findGaps Row F.gappedRow.parent F.gappedRow.children
    X--> 2
    List.length <| findGroups <|
        findGaps Row F.filledRow.parent F.filledRow.children
    X--> 1

-}
findGroups : List Piece -> List (List Id)
findGroups pieces =
    let
        -- Tail recursive helper.
        go : Float -> List Piece -> List Piece -> List (List Piece)
        go lastgap group ps =
            case ps of
                [] ->
                    case group of
                        [] ->
                            []

                        _ ->
                            [ group ]

                (Gap gap) :: rest ->
                    go gap group rest

                ((Content i width) as c) :: rest ->
                    case group of
                        [] ->
                            go lastgap [ c ] rest

                        _ ->
                            -- The meaty bit...
                            if width > lastgap then
                                go lastgap (group ++ [ c ]) rest

                            else
                                group :: go lastgap [ c ] rest

        -- Pull IDs out of content entries.
        extract : Piece -> Maybe Id
        extract p =
            case p of
                Gap _ ->
                    Nothing

                Content id _ ->
                    Just id
    in
    pieces
        |> List.take (List.length pieces - 1)
        |> List.drop 1
        |> go 0 []
        |> List.map (List.filterMap extract)



-- Basic gap calculation, used in a couple of places.


basicGaps : GroupingDirection -> (a -> Rectangle) -> List a -> List Float
basicGaps dir f rs =
    let
        gapper =
            case dir of
                Row ->
                    -- Gaps between elements are found from left edge
                    -- of 2nd rectangle, right edge of 1st rectangle
                    -- (in sorted order).
                    \r1 r2 -> Rectangle.x1 (f r2) - Rectangle.x2 (f r1)

                Column ->
                    -- Gaps between elements are found from top edge
                    -- of 2nd rectangle, bottom edge of 1st rectangle
                    -- (in sorted order).
                    \r1 r2 -> Rectangle.y1 (f r2) - Rectangle.y2 (f r1)
    in
    List.map2 gapper rs (List.drop 1 rs)



-- Work out whether the gaps between a sequence of items are even.


evenlySpaced : List Piece -> Bool
evenlySpaced pieces =
    let
        getGap piece =
            case piece of
                Gap g ->
                    Just g

                _ ->
                    Nothing

        allGaps =
            List.filterMap getGap pieces

        gaps =
            List.drop 1 <| List.take (List.length allGaps - 1) allGaps

        meanGap =
            List.sum gaps / toFloat (List.length gaps)

        tolerance =
            max (0.1 * meanGap) 0.05

        gapEven gap =
            abs (gap - meanGap) < tolerance
    in
    List.foldl (&&) True <| List.map gapEven gaps
