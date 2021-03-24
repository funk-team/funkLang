module Rectangle.Tree.Tests exposing (..)

import Canopy
import Expect
import Rectangle
import Rectangle.Tree
import Spec.Element.Id
import Test exposing (..)


all : Test
all =
    describe "Rectangle.Tree"
        [ describe "Rectangle.Tree.fromRects"
            [ test "Builds good tree" <|
                \_ ->
                    Rectangle.Tree.fromRects good
                        |> Expect.equal goodTree
            , test "Builds chokes on bad tree" <|
                \_ ->
                    Rectangle.Tree.fromRects bad
                        |> Expect.equal (Err (Rectangle.Tree.CanNotContain c))
            , test "Can not build from empty list" <|
                \_ ->
                    Rectangle.Tree.fromRects Spec.Element.Id.emptyDict
                        |> Expect.equal (Err Rectangle.Tree.Empty)
            ]
        ]


good =
    [ root, b, c, d ]
        |> Spec.Element.Id.dictFromList


bad =
    [ b, c, d ]
        |> Spec.Element.Id.dictFromList


root : ( Spec.Element.Id.Id, Rectangle.Rectangle )
root =
    ( Spec.Element.Id.idFromInt 0, Rectangle.fromPoints { x1 = 336, y1 = 106, y2 = 297, x2 = 772 } )


b : ( Spec.Element.Id.Id, Rectangle.Rectangle )
b =
    ( Spec.Element.Id.idFromInt 1, Rectangle.fromPoints { x1 = 343, y1 = 114, y2 = 275, x2 = 546 } )


c : ( Spec.Element.Id.Id, Rectangle.Rectangle )
c =
    ( Spec.Element.Id.idFromInt 2, Rectangle.fromPoints { x1 = 559, y1 = 118, y2 = 274, x2 = 759 } )


d : ( Spec.Element.Id.Id, Rectangle.Rectangle )
d =
    ( Spec.Element.Id.idFromInt 3, Rectangle.fromPoints { x1 = 419, y1 = 147, y2 = 251, x2 = 532 } )


goodTree =
    Ok (Canopy.Node root [ Canopy.Node b [ Canopy.Node d [] ], Canopy.Node c [] ])
