module Rectangle.Tests exposing (..)

import Expect
import Rectangle
import Test exposing (..)


all : Test
all =
    describe "Rectangle"
        [ describe "Rectangle.contains"
            [ test "Detects contained" <|
                \_ ->
                    Rectangle.contains root b
                        |> Expect.equal True
            , test "Not false positive" <|
                \_ ->
                    Rectangle.contains b c
                        |> Expect.equal False
            , test "Not symmetric" <|
                \_ ->
                    Rectangle.contains b root
                        |> Expect.equal False
            , test "Same size can be contained" <|
                \_ ->
                    Rectangle.contains root root
                        |> Expect.equal True
            ]
        , describe "Rectangle.overlaps"
            [ test "Detects overlapping" <|
                \_ ->
                    Rectangle.overlaps root b
                        |> Expect.equal True
            , test "Not false positive" <|
                \_ ->
                    Rectangle.overlaps root overlapsB
                        |> Expect.equal False
            , test "Symmetric" <|
                \_ ->
                    Rectangle.overlaps overlapsA overlapsB
                        |> Expect.equal True
            , test "Same size is overlapping" <|
                \_ ->
                    Rectangle.contains root root
                        |> Expect.equal True
            ]
        ]


root =
    Rectangle.Rectangle
        { x1 = 336, y1 = 106, x2 = 772, y2 = 297 }


b =
    Rectangle.Rectangle
        { x1 = 343, y1 = 114, x2 = 546, y2 = 275 }


c =
    Rectangle.Rectangle
        { x1 = 559, y1 = 118, x2 = 759, y2 = 274 }


d =
    Rectangle.Rectangle
        { x1 = 419, y1 = 147, x2 = 532, y2 = 251 }


overlapsA =
    Rectangle.Rectangle
        { x1 = 337, y1 = 402, x2 = 602, y2 = 529 }


overlapsB =
    Rectangle.Rectangle
        { x1 = 406, y1 = 436, x2 = 700, y2 = 495 }
