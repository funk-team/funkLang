module Canvas.Tool.Responsify.Facts.Layouts exposing
    ( isAColumn
    , isARow
    , isATopBottomRow
    )

import Canvas.Tool.Responsify.Facts.Alignment exposing (sameBottomOffset, sameTopOffset)
import Canvas.Tool.Responsify.Facts.Basic exposing (alignedInY, noOverlapInX, noOverlapInY)
import Canvas.Tool.Responsify.Info exposing (Fact(..), FactCalc, memoFact)
import Rectangle exposing (Rectangle(..))


{-| Fact: we have a row layout -- child elements don't overlap in the
x-direction and are roughly aligned in the y-direction.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)

    Tuple.first (isARow emptyInfo F.filledRow)
    --> True
    Tuple.first (isARow emptyInfo F.xOverlapFail)
    --> False

-}
isARow : FactCalc
isARow =
    memoFact IsARow <|
        \info data ->
            let
                ( noOverlap, info2 ) =
                    noOverlapInX info data

                ( aligned, info3 ) =
                    alignedInY info2 data
            in
            ( noOverlap && aligned, info3 )


{-| Fact: we have a "top/bottom" row layout -- child elements don't
overlap in the x-direction and are either top, centre or bottom
aligned in the y-direction.
-}
isATopBottomRow : FactCalc
isATopBottomRow =
    memoFact IsATopBottomRow <|
        \info data ->
            let
                ( noOverlap, info2 ) =
                    noOverlapInX info data

                ( top, info3 ) =
                    sameTopOffset info2 data

                ( bottom, info4 ) =
                    sameBottomOffset info3 data
            in
            ( noOverlap && (top || bottom), info4 )


{-| Fact: we have a column layout -- child elements don't overlap in
the y-direction. We don't impose any conditions on alignment in
the x-direction because this is handled by other facts.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)

    Tuple.first (isAColumn emptyInfo F.filledRow)
    --> False

-}
isAColumn : FactCalc
isAColumn =
    memoFact IsAColumn <|
        \info data ->
            let
                ( noOverlap, info2 ) =
                    noOverlapInY info data
            in
            ( noOverlap, info2 )
