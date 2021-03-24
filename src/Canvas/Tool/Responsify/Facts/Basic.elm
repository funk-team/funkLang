module Canvas.Tool.Responsify.Facts.Basic exposing
    ( alignedInX
    , alignedInY
    , allSimilarHeight
    , allSimilarWidth
    , allSimilarWidthButLast
    , edgeAlignedInY
    , firstLastSimilarWidth
    , noOverlapInX
    , noOverlapInY
    )

import Canvas.Tool.Responsify.Info exposing (Fact(..), FactCalc, memoSimpleFact)
import Canvas.Tool.Responsify.Utils exposing (allSimilarWith, testAllAdjacent)
import Rectangle exposing (Rectangle(..))


{-| Fact: all child elements are similar height (+/- tolerance).

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)

    Tuple.first (allSimilarHeight emptyInfo F.filledRow)
    --> True
    Tuple.first (allSimilarHeight emptyInfo F.simHeightFail)
    --> False

-}
allSimilarHeight : FactCalc
allSimilarHeight =
    memoSimpleFact AllSimilarHeight <|
        \info data ->
            allSimilarWith Rectangle.similarHeight data.children


{-| Fact: all child elements are similar width (+/- tolerance).

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)

    Tuple.first (allSimilarWidth emptyInfo F.filledRow)
    --> True
    Tuple.first (allSimilarWidth emptyInfo F.filledRowDiffLast)
    --> False
    Tuple.first (allSimilarWidth emptyInfo F.simWidthFail)
    --> False

-}
allSimilarWidth : FactCalc
allSimilarWidth =
    memoSimpleFact AllSimilarWidth <|
        \info data ->
            allSimilarWith Rectangle.similarWidth data.children


{-| Fact: all child elements are similar width (+/- tolerance) except
for last.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)

    Tuple.first (allSimilarWidthButLast emptyInfo F.filledRow)
    --> False
    Tuple.first (allSimilarWidthButLast emptyInfo F.filledRowDiffLast)
    --> True
    Tuple.first (allSimilarWidthButLast emptyInfo F.simWidthFail)
    --> False

-}
allSimilarWidthButLast : FactCalc
allSimilarWidthButLast =
    memoSimpleFact AllSimilarWidthButLast <|
        \info data ->
            let
                sortedChildren =
                    List.sortBy Rectangle.x1 data.children

                nchildren =
                    List.length data.children
            in
            not (Tuple.first <| allSimilarWidth info data)
                && (allSimilarWith Rectangle.similarWidth <|
                        List.take (nchildren - 1) sortedChildren
                   )


firstLastSimilarWidth : FactCalc
firstLastSimilarWidth =
    memoSimpleFact FirstLastSimilarWidth <|
        \info data ->
            let
                sortedChildren =
                    List.sortBy Rectangle.x1 data.children

                nchildren =
                    List.length data.children

                firstLast =
                    List.take 1 sortedChildren
                        ++ List.drop (nchildren - 1) sortedChildren
            in
                allSimilarWith Rectangle.similarWidth firstLast


{-| Fact: child rectangles do not overlap in the x-direction.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)

    Tuple.first (noOverlapInX emptyInfo F.filledRow)
    --> True
    Tuple.first (noOverlapInX emptyInfo F.xOverlapFail)
    --> False

-}
noOverlapInX : FactCalc
noOverlapInX =
    memoSimpleFact NoOverlapInX <|
        \info data ->
            testAllAdjacent Rectangle.notOverlappingX Rectangle.x1 data.children



-- Fact: child rectangles do not overlap in the y-direction.


noOverlapInY : FactCalc
noOverlapInY =
    memoSimpleFact NoOverlapInY <|
        \info data ->
            testAllAdjacent Rectangle.notOverlappingY Rectangle.y1 data.children


{-| Fact: centroids of child rectangles are aligned in y-direction.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)

    Tuple.first (alignedInY emptyInfo F.filledRow)
    --> True
    Tuple.first (alignedInY emptyInfo F.yAlignFail)
    --> False

-}
alignedInY : FactCalc
alignedInY =
    let
        similarCenterY r1 r2 =
            Rectangle.similar (Rectangle.center r1).y (Rectangle.center r2).y
    in
    memoSimpleFact AlignedInY <|
        \info data ->
            allSimilarWith similarCenterY data.children


{-| Fact: edges of child rectangles are aligned to top or bottom of
parent.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)

    Tuple.first (edgeAlignedInY emptyInfo F.filledRow)
    --> True

-}
edgeAlignedInY : FactCalc
edgeAlignedInY =
    let
        similarTop r1 r2 =
            Rectangle.similar (Rectangle.y1 r1) (Rectangle.y1 r2)

        similarBottom r1 r2 =
            Rectangle.similar (Rectangle.y2 r1) (Rectangle.y2 r2)
    in
    memoSimpleFact EdgeAlignedInY <|
        \info data ->
            allSimilarWith similarTop data.children
                || allSimilarWith similarBottom data.children



-- Fact: centroids of child rectangles are aligned in x-direction.


alignedInX : FactCalc
alignedInX =
    let
        similarCenterX r1 r2 =
            Rectangle.similar (Rectangle.center r1).x (Rectangle.center r2).x
    in
    memoSimpleFact AlignedInX <|
        \info data ->
            allSimilarWith similarCenterX data.children
