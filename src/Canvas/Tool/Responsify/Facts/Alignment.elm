module Canvas.Tool.Responsify.Facts.Alignment exposing
    ( allHorizCentreAligned
    , allHorizFill
    , allHorizSideAligned
    , allVertCentreAligned
    , firstAlignedLeft
    , firstAlignedTop
    , groupHorizCentreAligned
    , groupVertCentreAligned
    , indexHorizCentreAligned
    , indexVertCentreAligned
    , interiorGroupHorizCentreAligned
    , lastAlignedBottom
    , lastAlignedRight
    , sameBottomOffset
    , sameTopOffset
    )

import Canvas.Tool.Responsify.Info exposing (Fact(..), FactCalc, memoSimpleFact)
import Canvas.Tool.Responsify.Utils exposing (comparison)
import List.Extra exposing (maximumBy, minimumBy)
import Rectangle exposing (Point, Rectangle(..))



-- Edge coincidence is defined in terms of child element size: if the
-- gap between the edge of the child and the edge of the parent is
-- less than half the size of the child element in the relevant
-- direction, then the child is considered to be aligned with the
-- parent.
--
-- The reason for doing it this way rather than having some fixed
-- pixel tolerance for alignment is that the padding setting for the
-- parent can take up more or less any size of displacement between
-- the parent and the child, and it's not obvious that it even makes
-- sense to care too much about any absolute alignment between the
-- child and parent edges.


{-| Fact: first child aligned left.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)

    Tuple.first (firstAlignedLeft emptyInfo F.filledRow)
    --> True
    Tuple.first (firstAlignedLeft emptyInfo F.leftAlignFail)
    --> False

-}
firstAlignedLeft : FactCalc
firstAlignedLeft =
    memoSimpleFact FirstAlignedLeft <|
        \info data ->
            case minimumBy Rectangle.x1 data.children of
                Nothing ->
                    False

                Just ch ->
                    abs (Rectangle.x1 ch - Rectangle.x1 data.parent)
                        < comparison Rectangle.width data



-- Fact: first child aligned top.


firstAlignedTop : FactCalc
firstAlignedTop =
    memoSimpleFact FirstAlignedTop <|
        \info data ->
            case minimumBy Rectangle.y1 data.children of
                Nothing ->
                    False

                Just ch ->
                    abs (Rectangle.y1 ch - Rectangle.y1 data.parent)
                        < comparison Rectangle.height data


{-| Fact: last child aligned right.

    import Canvas.Tool.Responsify.Fixtures as F
    import Canvas.Tool.Responsify.Info exposing (emptyInfo)

    Tuple.first (lastAlignedRight emptyInfo F.filledRow)
    --> True
    Tuple.first (lastAlignedRight emptyInfo F.rightAlignFail)
    --> False

-}
lastAlignedRight : FactCalc
lastAlignedRight =
    memoSimpleFact LastAlignedRight <|
        \info data ->
            case maximumBy Rectangle.x2 data.children of
                Nothing ->
                    False

                Just ch ->
                    abs (Rectangle.x2 ch - Rectangle.x2 data.parent)
                        < comparison Rectangle.width data



-- Fact: last child aligned bottom.


lastAlignedBottom : FactCalc
lastAlignedBottom =
    memoSimpleFact LastAlignedBottom <|
        \info data ->
            case maximumBy Rectangle.y2 data.children of
                Nothing ->
                    False

                Just ch ->
                    abs (Rectangle.y2 ch - Rectangle.y2 data.parent)
                        < comparison Rectangle.height data



-- Fact: child group is horizontally centred as a whole within parent.


groupHorizCentreAligned : FactCalc
groupHorizCentreAligned =
    memoSimpleFact GroupHorizCentreAligned <|
        \info data ->
            let
                centres =
                    List.map Rectangle.center data.children

                centroidX =
                    List.sum (List.map (\pt -> pt.x) centres)
                        / toFloat (List.length centres)

                parentCentroidX =
                    (Rectangle.center data.parent).x
            in
            abs (parentCentroidX - centroidX)
                < comparison Rectangle.width data



-- Fact: child group without first and last entries is horizontally
-- centred as a whole within parent.


interiorGroupHorizCentreAligned : FactCalc
interiorGroupHorizCentreAligned =
    memoSimpleFact GroupHorizCentreAligned <|
        \info data ->
            let
                centres =
                    List.map Rectangle.center data.children
                        |> List.sortBy (\pt -> pt.x)
                        |> List.drop 1
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse

                centroidX =
                    List.sum (List.map (\pt -> pt.x) centres)
                        / toFloat (List.length centres)

                parentCentroidX =
                    (Rectangle.center data.parent).x
            in
            abs (parentCentroidX - centroidX)
                < comparison Rectangle.width data



-- Fact: child group is vertically centred as a whole within parent.


groupVertCentreAligned : FactCalc
groupVertCentreAligned =
    memoSimpleFact GroupVertCentreAligned <|
        \info data ->
            let
                centres =
                    List.map Rectangle.center data.children

                centroidY =
                    List.sum (List.map (\pt -> pt.y) centres)
                        / toFloat (List.length centres)

                parentCentroidY =
                    (Rectangle.center data.parent).y
            in
            abs (parentCentroidY - centroidY)
                < comparison Rectangle.width data



-- Fact: item with given index is horizontally centred.


indexHorizCentreAligned : Int -> FactCalc
indexHorizCentreAligned idx =
    memoSimpleFact (IndexHorizCentreAligned idx) <|
        \info data ->
            case List.drop idx (List.sortBy Rectangle.x1 data.children) of
                [] ->
                    False

                c :: _ ->
                    let
                        centroidX =
                            (Rectangle.center c).x

                        parentCentroidX =
                            (Rectangle.center data.parent).x
                    in
                    abs (parentCentroidX - centroidX)
                        < comparison Rectangle.width data



-- Fact: item with given index is vertically centred.


indexVertCentreAligned : Int -> FactCalc
indexVertCentreAligned idx =
    memoSimpleFact (IndexVertCentreAligned idx) <|
        \info data ->
            case List.drop idx (List.sortBy Rectangle.y1 data.children) of
                [] ->
                    False

                c :: _ ->
                    let
                        centroidY =
                            (Rectangle.center c).y

                        parentCentroidY =
                            (Rectangle.center data.parent).y
                    in
                    abs (parentCentroidY - centroidY)
                        < comparison Rectangle.height data



-- Fact: all children are horizontally centred within parent.


allHorizCentreAligned : FactCalc
allHorizCentreAligned =
    memoSimpleFact AllHorizCentreAligned <|
        \info data ->
            let
                centroidsX =
                    List.map (\c -> (Rectangle.center c).x) data.children

                parentCentroidX =
                    (Rectangle.center data.parent).x

                delta =
                    comparison Rectangle.width data
            in
            List.foldl (&&) True <|
                List.map (\x -> abs (parentCentroidX - x) < delta) centroidsX



-- Fact: all children are vertically centred within parent.


allVertCentreAligned : FactCalc
allVertCentreAligned =
    memoSimpleFact AllVertCentreAligned <|
        \info data ->
            let
                centroidsY =
                    List.map (\c -> (Rectangle.center c).y) data.children

                parentCentroidY =
                    (Rectangle.center data.parent).y

                delta =
                    comparison Rectangle.height data
            in
            List.foldl (&&) True <|
                List.map (\y -> abs (parentCentroidY - y) < delta) centroidsY



-- Fact: all children are aligned to either the left- or right-hand
-- edge of the parent.


allHorizSideAligned : FactCalc
allHorizSideAligned =
    memoSimpleFact AllHorizSideAligned <|
        \info data ->
            let
                delta =
                    comparison Rectangle.width data

                check r =
                    (abs (Rectangle.x1 r - Rectangle.x1 data.parent) < delta)
                        || (abs (Rectangle.x2 r - Rectangle.x2 data.parent) < delta)
            in
            List.foldl (&&) True <| List.map check data.children



-- Fact: all children have their top edges at the same distance from
-- the parent's top edge.


sameTopOffset : FactCalc
sameTopOffset =
    memoSimpleFact SameTopOffset <|
        \info data ->
            case data.children of
                ch :: rest ->
                    let
                        offset =
                            abs (Rectangle.y1 ch - Rectangle.y1 data.parent)

                        check r =
                            Rectangle.similar offset <|
                                abs (Rectangle.y1 r - Rectangle.y1 data.parent)
                    in
                    List.foldl (&&) True <| List.map check rest

                _ ->
                    False



-- Fact: all children have their bottom edges at the same distance
-- from the parent's bottom edge.


sameBottomOffset : FactCalc
sameBottomOffset =
    memoSimpleFact SameBottomOffset <|
        \info data ->
            case data.children of
                ch :: rest ->
                    let
                        offset =
                            abs (Rectangle.y2 ch - Rectangle.y2 data.parent)

                        check r =
                            Rectangle.similar offset <|
                                abs (Rectangle.y2 r - Rectangle.y2 data.parent)
                    in
                    List.foldl (&&) True <| List.map check rest

                _ ->
                    False



-- Fact: all children are aligned to both the left- and right-hand
-- edges of the parent.


allHorizFill : FactCalc
allHorizFill =
    memoSimpleFact AllHorizFill <|
        \info data ->
            let
                delta =
                    comparison Rectangle.width data

                check r =
                    (abs (Rectangle.x1 r - Rectangle.x1 data.parent) < delta)
                        && (abs (Rectangle.x2 r - Rectangle.x2 data.parent) < delta)
            in
            List.foldl (&&) True <| List.map check data.children
