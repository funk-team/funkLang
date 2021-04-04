module Canvas.Tool.Responsify.Facts.Gravity exposing
    ( Gravity(..)
    , convertGravity
    , indexXGravity
    , indexYGravity
    )

import Canvas.Tool.Responsify.Info exposing (Param(..), ParamCalc, memoSimpleParam)
import Rectangle exposing (Rectangle(..))



-- Parameter: determine X-gravity (-1, 0, +1) for item with given
-- index.


indexXGravity : Int -> ParamCalc
indexXGravity idx =
    memoSimpleParam (IndexXGravity idx) <|
        \info data ->
            case List.drop idx (List.sortBy Rectangle.x1 data.children) of
                [] ->
                    0

                c :: _ ->
                    calcGravity
                        (Rectangle.x1 c - Rectangle.x1 data.parent)
                        (Rectangle.x2 data.parent - Rectangle.x2 c)



-- Parameter: determine Y-gravity (-1, 0, +1) for item with given
-- index.


indexYGravity : Int -> ParamCalc
indexYGravity idx =
    memoSimpleParam (IndexYGravity idx) <|
        \info data ->
            case List.drop idx (List.sortBy Rectangle.y1 data.children) of
                [] ->
                    0

                c :: _ ->
                    calcGravity
                        (Rectangle.y1 c - Rectangle.y1 data.parent)
                        (Rectangle.y2 data.parent - Rectangle.y2 c)



-- "Gravity" is based on the asymmetry between gaps on opposite sides
-- of an element. If there's no asymmetry, the element is centred,
-- otherwise there's an attraction to the closer side of the
-- containing element.


calcGravity : Float -> Float -> Int
calcGravity minus plus =
    case ( abs (minus - plus) < 1, minus < plus ) of
        ( True, _ ) ->
            0

        ( False, True ) ->
            -1

        _ ->
            1



-- Friendly gravity type for pattern matching.


type Gravity
    = Minus
    | Zero
    | Plus
    | None



-- The stupid case+if thing here is because Elm can't match a "Just
-- (-1)" pattern.


convertGravity : Maybe Int -> Gravity
convertGravity ig =
    case ig of
        Just g ->
            if g == 0 then
                Zero

            else if g < 0 then
                Minus

            else
                Plus

        _ ->
            None
