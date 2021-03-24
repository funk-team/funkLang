module Rectangle exposing (..)

{-| Represtentations and operations for Rectangles on a dimensionless cartesian coordinate system
-}

import Compass
import Element
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra
import Spec.Element.Id



---- DATA STRUCTURES ---


type alias Dimensions =
    { height : Float, width : Float, x : Float, y : Float }


type alias ClusterPrams =
    { rectangles : List Rectangle
    , boundry : Maybe Rectangle
    , contains : List Int
    , density : Float
    , clusterType : ClusterType
    , id : Int
    }


type ClusterType
    = GroupRow
    | GroupCol



-- [generator-start]


type alias DetectedCluster =
    List ( Spec.Element.Id.Id, Rectangle )


type alias Boundaries =
    { x1 : Float
    , x2 : Float
    , y1 : Float
    , y2 : Float
    }


type Rectangle
    = Rectangle Boundaries


type alias Point =
    { x : Float, y : Float }


type VertexId
    = X1
    | X2
    | Y1
    | Y2


{-|

    moveTo {x = 100, y = 10} (fromDimensionsFloat {x = 100, y = 100, width = 100, height = 100})
    --> fromDimensionsFloat {x = 100, y = 10, width = 100, height = 100}
    moveTo {x = 50, y = 150} (fromDimensionsFloat {x = 100, y = 100, width = 100, height = 100})
    --> fromDimensionsFloat {x = 50, y = 150, width = 100, height = 100}

-}
moveTo : Point -> Rectangle -> Rectangle
moveTo { x, y } rect =
    let
        offsetX =
            x - x1 rect

        offsetY =
            y - y1 rect
    in
    moveBy offsetX offsetY rect


moveToOrigin : Rectangle -> Rectangle
moveToOrigin (Rectangle rect) =
    Rectangle { x1 = 0, y1 = 0, x2 = rect.x2 - rect.x1, y2 = rect.y2 - rect.y1 }


fromDimensionsFloat : Dimensions -> Rectangle
fromDimensionsFloat d =
    Rectangle
        { x1 = d.x
        , y1 = d.y
        , x2 = d.x + d.width
        , y2 = d.y + d.height
        }


{-| Get the vector that projects one point onto another
-}
getPointOffset : Point -> Point -> Point
getPointOffset origin target =
    { x = target.x - origin.x, y = target.y - origin.y }


{-| Move a point given a vector
-}
movePoint : Point -> Point -> Point
movePoint vector point =
    { x = point.x + vector.x
    , y = point.y + vector.y
    }


fromPoints r =
    Rectangle r


fromIntPoints r =
    Rectangle { x1 = toFloat <| r.x1, x2 = toFloat <| r.x2, y1 = toFloat <| r.y1, y2 = toFloat <| r.y2 }


{-| <https:/gist.github.com/Daniel-Hug/d7984d82b58d6d2679a087d896ca3d2b>
-}
x1 : Rectangle -> Float
x1 (Rectangle r) =
    r.x1


y1 : Rectangle -> Float
y1 (Rectangle r) =
    r.y1


x2 : Rectangle -> Float
x2 (Rectangle r) =
    r.x2


y2 : Rectangle -> Float
y2 (Rectangle r) =
    r.y2


{-| Find the rectangle that encloses a list of rectangles

    boundingBox [Rectangle.fromPoints {x1 = 0, x2 = 200, y1 = 400, y2 = 500}]
    --> Just <| Rectangle.fromPoints {x1 = 0, x2 = 200, y1 = 400, y2 = 500}

    boundingBox [Rectangle.fromPoints {x1 = 0, x2 = 200, y1 = 400, y2 = 500}, Rectangle.fromPoints {x1 = -100, x2 = 110, y1 = 110, y2 = 510}]
    --> Just <| Rectangle.fromPoints {x1 = -100, x2 = 200, y1 = 110, y2 = 510}

    boundingBox []
    --> Nothing

-}
boundingBox : List Rectangle -> Maybe Rectangle
boundingBox rectangle =
    let
        minX =
            List.map x1 rectangle
                |> List.minimum

        maxX =
            List.map x2 rectangle
                |> List.maximum

        minY =
            List.map y1 rectangle
                |> List.minimum

        maxY =
            List.map y2 rectangle
                |> List.maximum
    in
    Maybe.map4 Boundaries minX maxX minY maxY
        |> Maybe.map Rectangle


{-| Find the distance between parent and child on either side.
We assume that the child within the parent.

    findPadding (Rectangle {x1 = 150, x2 = 230, y1 = 30, y2 = 80}) (Rectangle {x1 = 180, x2 = 200, y1 = 40, y2 = 60})
    --> {left = 30, top = 10, bottom = 20, right = 30}

-}
findPadding : Rectangle -> Rectangle -> { top : Float, bottom : Float, left : Float, right : Float }
findPadding parent child =
    { top = y1 parent - y1 child |> abs
    , bottom = y2 child - y2 parent |> abs
    , left = x1 parent - x1 child |> abs
    , right = x2 child - x2 parent |> abs
    }


{-| Find the offset vector pointing from one point to another one

    getOffset {x = 10, y = 20} {x = 45, y = 55}
        --> {x = 35, y = 35}

-}
getOffset : Point -> Point -> Point
getOffset from to =
    Point (to.x - from.x) (to.y - from.y)


move : ( Float, Float ) -> Rectangle -> Rectangle
move ( xOffset, yOffset ) (Rectangle rect) =
    Rectangle
        { rect
            | x1 = rect.x1 + xOffset
            , x2 = rect.x2 + xOffset
            , y1 = rect.y1 + yOffset
            , y2 = rect.y2 + yOffset
        }


fix : Rectangle -> Rectangle
fix r =
    let
        ( x1_, x2_ ) =
            sortTuple ( x1 r, x2 r )

        ( y1_, y2_ ) =
            sortTuple ( y1 r, y2 r )
    in
    Rectangle { x1 = x1_, x2 = x2_, y1 = y1_, y2 = y2_ }


{-| Move a rectangle so that its center is on a point
-}
moveCenter : Point -> Rectangle -> Rectangle
moveCenter newCenter rect =
    let
        c =
            center rect

        offsetX =
            newCenter.x - c.x

        offsetY =
            newCenter.y - c.y
    in
    moveBy offsetX offsetY rect


transformWithPoint : Compass.Direction -> Point -> Rectangle -> Rectangle
transformWithPoint direction newPoint (Rectangle rect) =
    fix <|
        case direction of
            Compass.Center ->
                moveCenter newPoint (Rectangle rect)

            Compass.North ->
                Rectangle { rect | y1 = newPoint.y }

            Compass.NorthWest ->
                Rectangle { rect | y1 = newPoint.y, x1 = newPoint.x }

            Compass.West ->
                Rectangle { rect | x1 = newPoint.x }

            Compass.SouthWest ->
                Rectangle { rect | y2 = newPoint.y, x1 = newPoint.x }

            Compass.South ->
                Rectangle { rect | y2 = newPoint.y }

            Compass.SouthEast ->
                Rectangle { rect | y2 = newPoint.y, x2 = newPoint.x }

            Compass.East ->
                Rectangle { rect | x2 = newPoint.x }

            Compass.NorthEast ->
                Rectangle { rect | y1 = newPoint.y, x2 = newPoint.x }


moveBy x y (Rectangle rect) =
    Rectangle
        { x1 = rect.x1 + x
        , x2 = rect.x2 + x
        , y1 = rect.y1 + y
        , y2 = rect.y2 + y
        }


{-| Given a resize handle location and a point that describes the movement transform an element
-}
transformWithVector : Compass.Direction -> Point -> Rectangle -> Rectangle
transformWithVector direction offset (Rectangle rect) =
    fix <|
        Rectangle <|
            case direction of
                Compass.North ->
                    { rect | y1 = rect.y1 + offset.y }

                Compass.NorthWest ->
                    { rect | y1 = rect.y1 + offset.y, x1 = rect.x1 + offset.x }

                Compass.West ->
                    { rect | x1 = rect.x1 + offset.x }

                Compass.SouthWest ->
                    { rect | y2 = rect.y2 + offset.y, x1 = rect.x1 + offset.x }

                Compass.South ->
                    { rect | y2 = rect.y2 + offset.y }

                Compass.SouthEast ->
                    { rect | y2 = rect.y2 + offset.y, x2 = rect.x2 + offset.x }

                Compass.East ->
                    { rect | x2 = rect.x2 + offset.x }

                Compass.NorthEast ->
                    { rect | y1 = rect.y1 + offset.y, x2 = rect.x2 + offset.x }

                Compass.Center ->
                    { rect | y1 = rect.y1 + offset.y, x1 = rect.x1 + offset.x, y2 = rect.y2 + offset.y, x2 = rect.x2 + offset.x }


{-| check whether a contains b
is True if

  - b is smaller to a and
  - b is within the boundaries of A

-}
contains : Relationship
contains a b =
    x1 b
        < x1 a
        || y1 b
        < y1 a
        || x2 b
        > x2 a
        || y2 b
        > y2 a
        |> not


similarWidth a b =
    similar (width a) (width b)


similarHeight a b =
    similar (height a) (height b)


similar : Float -> Float -> Bool
similar a b =
    let
        absMin =
            5

        absMax =
            50

        deviation =
            20 / 100

        diff =
            abs (a - b)
    in
    (diff < absMin)
        || ((diff < absMax)
                && (diff < (a + b) * deviation / 2)
           )


same : Float -> Float -> Bool
same a b =
    round a == round b



-- Can we form a clluster with no collection


clusterWithinCluster_ : List Rectangle -> Rectangle -> List Bool
clusterWithinCluster_ allClusters testCluster =
    List.map (intersects testCluster) allClusters


{-| Find the offset vector pointing from one point to another one
cluster
GroupRow
(Spec.Element.Id.Id 926857637,Rectangle { x1 = 57.99999838457529, x2 = 203.99999033900542, y1 = 46.99999197612311, y2 = 129.99998394435784 })
(Spec.Element.Id.Id 926857637,Rectangle { x1 = 57.99999838457529, x2 = 203.99999033900542, y1 = 46.99999197612311, y2 = 129.99998394435784 })
-}
cluster : ClusterType -> ( Spec.Element.Id.Id, Rectangle ) -> ( Spec.Element.Id.Id, Rectangle ) -> Maybe ( Spec.Element.Id.Id, Rectangle )
cluster clusterType testRect allrect_ =
    let
        deviation =
            5

        ( idA, a ) =
            testRect

        ( idB, b ) =
            allrect_

        ( a1, b1 ) =
            case clusterType of
                GroupCol ->
                    ( x1 a, x1 b )

                GroupRow ->
                    ( y1 a, y1 b )

        ( a2, b2 ) =
            case clusterType of
                GroupCol ->
                    ( x2 a, x2 b )

                GroupRow ->
                    ( y2 a, y2 b )
    in
    case
        ((a1 * 100 > b1 * (100 - deviation))
            && (a1 * 100 < b1 * (100 + deviation))
        )
            && ((a2 * 100 > b2 * (100 - deviation))
                    && (a2 * 100 < b2 * (100 + deviation))
               )
    of
        True ->
            case clusterType of
                -- check width too
                GroupRow ->
                    -- Just ( idB, ( y1 b, y2 b ), ( x1 b, x2 b ) )
                    Just
                        ( idB
                        , Rectangle
                            { x1 = x1 b
                            , x2 = x2 b
                            , y1 = y1 b
                            , y2 = y2 b
                            }
                        )

                GroupCol ->
                    -- Just ( idB, ( x1 b, x2 b ), ( y1 b, y2 b ) )
                    Just
                        ( idA
                        , Rectangle
                            { x1 = x1 b
                            , x2 = x2 b
                            , y1 = y1 b
                            , y2 = y2 b
                            }
                        )

        False ->
            Nothing


similarCol : List ( Spec.Element.Id.Id, Rectangle ) -> ( Spec.Element.Id.Id, Rectangle ) -> DetectedCluster
similarCol allRects testRect =
    List.map
        (cluster GroupCol testRect)
        allRects
        |> Maybe.Extra.values


{-|

    similarRow
        [ ( Spec.Element.Id 926857637, Rectangle { x1 = 57.99999838457529, x2 = 203.99999033900542, y1 = 46.99999197612311, y2 = 129.99998394435784 } )
        , ( Spec.Element.Id 75879328, Rectangle { x1 = 228.9999983471056, x2 = 367.9999678650505, y1 = 46.99999197612311, y2 = 129.99998394435784 } )
        , ( Spec.Element.Id 889346134, Rectangle { x1 = 397.00001754288087, x2 = 525.0000239256959, y1 = 46.99999197612311, y2 = 129.99998394435784 } )
        , ( Spec.Element.Id 730853941, Rectangle { x1 = 128.9999951635864, x2 = 467.9999998974513, y1 = 182.00000637098253, y2 = 403.99996785716223 } )
        ]
        ( Spec.Element.Id 926857637, Rectangle { x1 = 57.99999838457529, x2 = 203.99999033900542, y1 = 46.99999197612311, y2 = 129.99998394435784 } )
        -->
        [ ( Spec.Element.Id 926857637, Rectangle { x1 = 57.99999838457529, x2 = 203.99999033900542, y1 = 46.99999197612311, y2 = 129.99998394435784 } )
        , ( Spec.Element.Id 75879328, Rectangle { x1 = 228.9999983471056, x2 = 367.9999678650505, y1 = 46.99999197612311, y2 = 129.99998394435784 } )
        , ( Spec.Element.Id 889346134, Rectangle { x1 = 397.00001754288087, x2 = 525.0000239256959, y1 = 46.99999197612311, y2 = 129.99998394435784 } )
        ]

-}
similarRow : List ( Spec.Element.Id.Id, Rectangle ) -> ( Spec.Element.Id.Id, Rectangle ) -> DetectedCluster
similarRow allRects testRect =
    List.map
        (cluster GroupRow testRect)
        allRects
        |> Maybe.Extra.values


similarSize : Rectangle -> Rectangle -> Bool
similarSize a b =
    similar (width a) (width b) && similar (height a) (height b)


center : Rectangle -> Point
center rect =
    { x = centerX rect, y = centerY rect }


centerX : Rectangle -> Float
centerX (Rectangle r) =
    (r.x1 + r.x2) / 2


centerY : Rectangle -> Float
centerY (Rectangle r) =
    (r.y1 + r.y2) / 2


type alias Relationship =
    Rectangle -> Rectangle -> Bool


notOverlappingX a b =
    x1 a >= x2 b || x1 b >= x2 a


notOverlappingY a b =
    y1 a >= y2 b || y1 b >= y2 a


overlaps : Relationship
overlaps a b =
    if
        notOverlappingX a b
        -- no horizontal overlap
    then
        False

    else if
        notOverlappingY a b
        -- no vertical overlap
    then
        False

    else
        True


size : Rectangle -> Float
size r =
    width r * height r


render : Rectangle -> List (Element.Attribute msg)
render =
    renderEach
        >> (\each ->
                [ Element.width <| Element.px each.width
                , Element.height <| Element.px each.height
                ]
                    ++ each.offset
           )


{-| Render a rectangle
-}
renderEach :
    Rectangle
    ->
        { height : Int
        , offset : List (Element.Attr decorative msg)
        , width : Int
        }
renderEach (Rectangle r) =
    { width = round <| width_ r
    , height = round <| height_ r
    , offset = [ Element.moveDown r.y1, Element.moveRight r.x1 ]
    }


width (Rectangle r) =
    width_ r


height (Rectangle r) =
    height_ r


width_ r =
    r.x2 - r.x1


height_ r =
    r.y2 - r.y1


{-| Convert the movement described by two points to a rectangle
-}
fromMovement : Point -> Point -> Rectangle
fromMovement start end =
    let
        ( x1_, x2_ ) =
            sortTuple ( start.x, end.x )

        ( y1_, y2_ ) =
            sortTuple ( start.y, end.y )
    in
    Rectangle
        { x1 = x1_
        , x2 = x2_
        , y1 = y1_
        , y2 = y2_
        }


sortTuple : ( comparable, comparable ) -> ( comparable, comparable )
sortTuple ( a, b ) =
    if a > b then
        ( b, a )

    else
        ( a, b )


{-| When one rectangle cuts the borders of another one but is not completely enclosed
-}
intersects : Rectangle -> Rectangle -> Bool
intersects a b =
    overlaps a b && not (contains a b) && not (contains b a)



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeBoundaries =
    Decode.map4
        Boundaries
        (Decode.field "x1" Decode.float)
        (Decode.field "x2" Decode.float)
        (Decode.field "y1" Decode.float)
        (Decode.field "y2" Decode.float)


decodeDetectedCluster =
    Decode.list decodeTuple_Spec_Element_Model_Id_Id_Rectangle_


decodePoint =
    Decode.map2
        Point
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


decodeRectangle =
    Decode.map Rectangle decodeBoundaries


decodeTuple_Spec_Element_Model_Id_Id_Rectangle_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Spec.Element.Id.decodeId)
        (Decode.field "A2" decodeRectangle)


decodeVertexId =
    let
        recover x =
            case x of
                "X1" ->
                    Decode.succeed X1

                "X2" ->
                    Decode.succeed X2

                "Y1" ->
                    Decode.succeed Y1

                "Y2" ->
                    Decode.succeed Y2

                other ->
                    Decode.fail <| "Unknown constructor for type VertexId: " ++ other
    in
    Decode.string |> Decode.andThen recover


encodeBoundaries a =
    Encode.object
        [ ( "x1", Encode.float a.x1 )
        , ( "x2", Encode.float a.x2 )
        , ( "y1", Encode.float a.y1 )
        , ( "y2", Encode.float a.y2 )
        ]


encodeDetectedCluster a =
    Encode.list encodeTuple_Spec_Element_Model_Id_Id_Rectangle_ a


encodePoint a =
    Encode.object
        [ ( "x", Encode.float a.x )
        , ( "y", Encode.float a.y )
        ]


encodeRectangle (Rectangle a1) =
    encodeBoundaries a1


encodeTuple_Spec_Element_Model_Id_Id_Rectangle_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", Spec.Element.Id.encodeId a1 )
        , ( "A2", encodeRectangle a2 )
        ]


encodeVertexId a =
    case a of
        X1 ->
            Encode.string "X1"

        X2 ->
            Encode.string "X2"

        Y1 ->
            Encode.string "Y1"

        Y2 ->
            Encode.string "Y2"



-- [generator-end]


readClientPoint =
    Decode.map2
        Point
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
