module Canvas.Tool.Responsify.Fixtures exposing
    ( centredColumn1
    , centredColumn2
    , centredRow
    , columnFromTop1
    , columnOf2
    , columnOf2Centred
    , filledRow
    , filledRowDiffLast
    , gappedRow
    , leftAlignFail
    , matcherCheck
    , read
    , rightAlignFail
    , rowOf3
    , sideAlignedColumn1
    , sideAlignedColumn2
    , simHeightFail
    , simWidthFail
    , singleton1
    , singleton2
    , write
    , xOverlapFail
    , yAlignFail
    )

import Canvas.Tool.Responsify.Info exposing (emptyInfo)
import Canvas.Tool.Responsify.Rules exposing (Match, MatchResult)
import Canvas.Tool.Responsify.Types exposing (Drawn)
import Generator
import Http
import Json.Encode as Encode
import Model
import Model.Model
import Rectangle exposing (Rectangle(..))
import Spec.Element.Id exposing (Id(..))
import Spec.Element.Layout as SEL
import Spec.Element.Layout.Length as SELL
import Spec.Element.Layout.Padding as SELP
import Spec.Element.Model as SEM



-- Helper to run checks on rule matchers against a set of example
-- cases.


matcherNames : List String
matcherNames =
    [ "gappedRow"
    , "filledRow"
    , "centredRow"
    , "singleton1"
    , "singleton2"
    , "rowOf3"
    , "centredColumn1"
    , "centredColumn2"
    , "sideAlignedColumn1"
    , "sideAlignedColumn2"
    , "columnOf2"
    , "columnOf2Centred"
    , "columnFromTop1"
    ]


matcherCheck : Match -> List ( Drawn, MatchResult ) -> Bool
matcherCheck match cases =
    let
        check1 ( data, result ) =
            Tuple.first (match emptyInfo data) == result

        checks =
            List.map check1 cases
    in
    if List.foldl (&&) True checks then
        True

    else
        -- let
        --     _ =
        --         checks
        --             |> List.map2 Tuple.pair matcherNames
        --             |> List.filter (not << Tuple.second)
        --             |> List.map Tuple.first
        --             |> Debug.log "failures"
        -- in
        False



-- Write examples to local file system.


write : (Result Http.Error () -> msg) -> Model.Model.UserModel -> Cmd msg
write msg userModel =
    let
        txt =
            Generator.renderFile userModel

        json =
            Encode.string txt
    in
    Http.post
        { url = "/api/generated-writer"
        , body = Http.jsonBody json
        , expect = Http.expectWhatever msg
        }



-- Read examples from local file system.


read : (Result Http.Error String -> msg) -> Cmd msg
read msg =
    Http.post
        { url = "/api/responsify-examples"
        , body = Http.emptyBody
        , expect = Http.expectString msg
        }



-- TEST FIXTURES FOR elm-verify-examples EXAMPLES --
--
--
-- IDs
--
--
-- ID generation for test layouts: parent (containing element) ID,
-- plus as many IDs as needed for the child elements.


parentId : Id
parentId =
    Id 0


childIds : Int -> List Id
childIds =
    List.map Id << List.range 1



-- SINGLETON LAYOUTS, I.E. A SINGLE CHILD WITHIN A PARENT --


singleton1 : Drawn
singleton1 =
    let
        children =
            [ Rectangle { x1 = 50, x2 = 150, y1 = 50, y2 = 100 } ]

        childrenIds =
            [ Id 1 ]
    in
    { parentId = parentId
    , childIds = childrenIds
    , parent = rowParent
    , children = children
    , element = makeElement childrenIds rowParent children
    }


singleton2 : Drawn
singleton2 =
    let
        children =
            [ Rectangle { x1 = 10, x2 = 240, y1 = 60, y2 = 90 } ]

        childrenIds =
            [ Id 1 ]
    in
    { parentId = parentId
    , childIds = childrenIds
    , parent = rowParent
    , children = children
    , element = makeElement childrenIds rowParent children
    }



-- ROWS OF ELEMENTS --
--
--
-- Containing parent element for row-based rule tests.


rowParent : Rectangle
rowParent =
    Rectangle { x1 = 50, x2 = 250, y1 = 50, y2 = 100 }



-- Full row of children filling parent: pass case for most basic
-- facts.


filledRow : Drawn
filledRow =
    let
        child : Int -> Rectangle
        child x =
            Rectangle
                { x1 = toFloat (x + 1) * 50
                , x2 = toFloat (x + 2) * 50
                , y1 = 50
                , y2 = 100
                }

        children =
            List.map child <| List.range 0 3

        childrenIds =
            childIds 4
    in
    { parentId = parentId
    , childIds = childrenIds
    , parent = rowParent
    , children = children
    , element = makeElement childrenIds rowParent children
    }


filledRowDiffLast : Drawn
filledRowDiffLast =
    let
        child : Int -> Rectangle
        child x =
            Rectangle
                { x1 = toFloat (x + 1) * 50
                , x2 = toFloat (x + 2) * 50
                , y1 = 50
                , y2 = 100
                }

        children =
            [ Rectangle { x1 = 60, x2 = 90, y1 = 50, y2 = 100 }
            , Rectangle { x1 = 110, x2 = 140, y1 = 50, y2 = 100 }
            , Rectangle { x1 = 160, x2 = 240, y1 = 50, y2 = 100 }
            ]

        childrenIds =
            childIds 3
    in
    { parentId = parentId
    , childIds = childrenIds
    , parent = rowParent
    , children = children
    , element = makeElement childrenIds rowParent children
    }



-- Row of children centred in parent.


centredRow : Drawn
centredRow =
    let
        child : Int -> Rectangle
        child x =
            Rectangle
                { x1 = toFloat <| 87 + x * 32
                , x2 = toFloat <| 87 + x * 32 + 30
                , y1 = 50
                , y2 = 100
                }

        children =
            List.map child <| List.range 0 3

        childrenIds =
            childIds 4
    in
    { parentId = parentId
    , childIds = childrenIds
    , parent = rowParent
    , children = children
    , element = makeElement childrenIds rowParent children
    }



-- Special "row of 3" case: one child aligned left, one aligned right,
-- one centred.


rowOf3 : Drawn
rowOf3 =
    let
        child : Int -> Rectangle
        child x =
            Rectangle
                { x1 = toFloat <| 50 + x * 90
                , x2 = toFloat <| 50 + x * 90 + 20
                , y1 = 50
                , y2 = 100
                }

        children =
            List.map child <| List.range 0 2

        childrenIds =
            childIds 3
    in
    { parentId = parentId
    , childIds = childrenIds
    , parent = rowParent
    , children = children
    , element = makeElement childrenIds rowParent children
    }



-- COLUMNS OF ELEMENTS --
--
--
-- Containing parent element for row-based rule tests.


colParent : Rectangle
colParent =
    Rectangle { x1 = 50, x2 = 250, y1 = 50, y2 = 650 }



-- Column of children filling parent in both directions.


centredColumn1 : Drawn
centredColumn1 =
    let
        child : Int -> Rectangle
        child y =
            Rectangle
                { x1 = 50
                , x2 = 250
                , y1 = 50 + toFloat (y * 220)
                , y2 = 50 + toFloat (y * 220) + 160
                }

        children =
            List.map child <| List.range 0 2

        childrenIds =
            childIds 3
    in
    { parentId = parentId
    , childIds = childrenIds
    , parent = colParent
    , children = children
    , element = makeElement childrenIds colParent children
    }


centredColumn2 : Drawn
centredColumn2 =
    let
        child : Int -> Rectangle
        child y =
            let
                w =
                    case y of
                        0 ->
                            80

                        1 ->
                            200

                        _ ->
                            150
            in
            Rectangle
                { x1 = 150 - toFloat w / 2
                , x2 = 150 + toFloat w / 2
                , y1 = 50 + toFloat (y * 220)
                , y2 = 50 + toFloat (y * 220) + 160
                }

        children =
            List.map child <| List.range 0 2

        childrenIds =
            childIds 3
    in
    { parentId = parentId
    , childIds = childrenIds
    , parent = colParent
    , children = children
    , element = makeElement childrenIds colParent children
    }


sideAlignedColumn1 : Drawn
sideAlignedColumn1 =
    let
        child : Int -> Rectangle
        child y =
            Rectangle
                { x1 = 200
                , x2 = 250
                , y1 = 50 + toFloat (y * 220)
                , y2 = 50 + toFloat (y * 220) + 160
                }

        children =
            List.map child <| List.range 0 2

        childrenIds =
            childIds 3
    in
    { parentId = parentId
    , childIds = childrenIds
    , parent = colParent
    , children = children
    , element = makeElement childrenIds colParent children
    }


sideAlignedColumn2 : Drawn
sideAlignedColumn2 =
    let
        child : Int -> Rectangle
        child y =
            Rectangle
                { x1 =
                    case y of
                        1 ->
                            50

                        _ ->
                            200
                , x2 =
                    case y of
                        1 ->
                            100

                        _ ->
                            250
                , y1 = 50 + toFloat (y * 220)
                , y2 = 50 + toFloat (y * 220) + 160
                }

        children =
            List.map child <| List.range 0 2

        childrenIds =
            childIds 3
    in
    { parentId = parentId
    , childIds = childrenIds
    , parent = colParent
    , children = children
    , element = makeElement childrenIds colParent children
    }


columnOf2 : Drawn
columnOf2 =
    let
        child : Int -> Rectangle
        child y =
            Rectangle
                { x1 = 50
                , x2 = 250
                , y1 =
                    if y == 0 then
                        50

                    else
                        550
                , y2 =
                    if y == 0 then
                        150

                    else
                        650
                }

        children =
            List.map child <| List.range 0 1

        childrenIds =
            childIds 2
    in
    { parentId = parentId
    , childIds = childrenIds
    , parent = colParent
    , children = children
    , element = makeElement childrenIds colParent children
    }


columnOf2Centred : Drawn
columnOf2Centred =
    let
        child : Int -> Rectangle
        child y =
            Rectangle
                { x1 = 50
                , x2 = 250
                , y1 =
                    if y == 0 then
                        50

                    else
                        300
                , y2 =
                    if y == 0 then
                        150

                    else
                        400
                }

        children =
            List.map child <| List.range 0 1

        childrenIds =
            childIds 2
    in
    { parentId = parentId
    , childIds = childrenIds
    , parent = colParent
    , children = children
    , element = makeElement childrenIds colParent children
    }


columnFromTop1 : Drawn
columnFromTop1 =
    let
        child : Int -> Rectangle
        child y =
            Rectangle
                { x1 = 50
                , x2 = 250
                , y1 = toFloat <| 50 + 70 * y
                , y2 = toFloat <| 50 + 70 * y + 50
                }

        children =
            List.map child <| List.range 0 2

        childrenIds =
            childIds 3
    in
    { parentId = parentId
    , childIds = childrenIds
    , parent = colParent
    , children = children
    , element = makeElement childrenIds colParent children
    }



-- BASIC FACTS --
--
--
-- Fail case for "all children similar height" fact.


simHeightFail : Drawn
simHeightFail =
    { filledRow | children = changeHeight filledRow.children }



-- Fail case for "all children similar width" fact.


simWidthFail : Drawn
simWidthFail =
    { filledRow | children = changeWidth filledRow.children }



-- Fail case for "no overlap in x-direction" fact.


xOverlapFail : Drawn
xOverlapFail =
    { filledRow | children = changeX filledRow.children }



-- Fail case for "centroids aligned in y-direction" fact.


yAlignFail : Drawn
yAlignFail =
    { filledRow | children = yShift filledRow.children }



-- Fail case for "first child aligned left" fact.


leftAlignFail : Drawn
leftAlignFail =
    { filledRow | children = leftSqueeze filledRow.children }



-- Fail case for "last child aligned right" fact.


rightAlignFail : Drawn
rightAlignFail =
    { filledRow | children = rightSqueeze filledRow.children }



-- GAPS AND GROUPS --
-- Row of children with gaps: base case for grouping tests.


gappedRow : Drawn
gappedRow =
    let
        child : Int -> Rectangle
        child x =
            Rectangle
                { x1 = 50 + toFloat x * 25 + 1
                , x2 = 50 + toFloat (x + 1) * 25 - 1
                , y1 = 50
                , y2 = 100
                }

        children =
            List.map child <| [ 0, 1, 2, 3, 6, 7 ]

        childrenIds =
            childIds 6
    in
    { parentId = parentId
    , childIds = childrenIds
    , parent = rowParent
    , children = children
    , element = makeElement childrenIds rowParent children
    }



-- HELPERS --
-- Helpers for modifying child elements to make them fail various
-- tests.


changeHeight : List Rectangle -> List Rectangle
changeHeight rs =
    case rs of
        [ Rectangle r1, r2, r3, r4 ] ->
            [ Rectangle { r1 | y2 = 80 }, r2, r3, r4 ]

        _ ->
            rs


changeWidth : List Rectangle -> List Rectangle
changeWidth rs =
    case rs of
        [ Rectangle r1, r2, r3, r4 ] ->
            [ Rectangle { r1 | x2 = 80 }, r2, r3, r4 ]

        _ ->
            rs


changeX : List Rectangle -> List Rectangle
changeX rs =
    case rs of
        [ r1, Rectangle r2, r3, r4 ] ->
            [ r1, Rectangle { r2 | x2 = r2.x2 + 10 }, r3, r4 ]

        _ ->
            rs


yShift : List Rectangle -> List Rectangle
yShift rs =
    case rs of
        [ r1, r2, Rectangle r3, r4 ] ->
            [ r1, r2, Rectangle { r3 | y1 = r3.y1 + 20, y2 = r3.y2 + 20 }, r4 ]

        _ ->
            rs


leftSqueeze : List Rectangle -> List Rectangle
leftSqueeze rs =
    case rs of
        [ Rectangle r1, r2, r3, r4 ] ->
            [ Rectangle { r1 | x1 = r1.x1 + 60 }, r2, r3, r4 ]

        _ ->
            rs


rightSqueeze : List Rectangle -> List Rectangle
rightSqueeze rs =
    case rs of
        [ r1, r2, r3, Rectangle r4 ] ->
            [ r1, r2, r3, Rectangle { r4 | x2 = r4.x2 - 60 } ]

        _ ->
            rs



-- Make a fake value for the element field of Drawn.


makeElement : List Id -> Rectangle -> List Rectangle -> SEM.EitherElement
makeElement chIds (Rectangle parent) children =
    let
        padding =
            SELP.EqualPadding Nothing

        ch =
            SEM.AbsoluteChildren []

        dim =
            SEM.AbsoluteElementDimensions
                (SELL.init <| parent.x2 - parent.x1)
                (SELL.init <| parent.y2 - parent.y1)
                (SELL.init parent.x1)
                (SELL.init parent.y1)
    in
    SEM.EitherElement
        (SEM.Shared parentId
            padding
            Nothing
            SEL.Column
            ch
            "fixture-element"
            SEM.Box
        )
        (SEM.AbsoluteElementGeometry dim)
