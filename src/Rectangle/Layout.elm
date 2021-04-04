module Rectangle.Layout exposing (FlowDetectionError(..), Layout(..), detectClustering, detectFlow, toWrappedRows)

{-| Detect layouts
-}

import List.Extra
import List.Unique
import Rectangle
import Spec.Element.Id
import Spec.Element.Layout


{-| A Flow can never be empty
Also, some layouts are not detectable (yet)
-}
type FlowDetectionError
    = Empty
    | CanNotFit
    | TooDissimilar


{-| A Flow is either row, column or contains a single element
-}
type Layout
    = Singleton Rectangle.Rectangle
    | Collection (List Rectangle.Rectangle) Spec.Element.Layout.Flow


{-| Try detecting the flow of a list of recangles

    import Spec.Element.Layout
    import Rectangle exposing (Rectangle)

    wrappedRow : List Rectangle.Rectangle
    wrappedRow = [Rectangle.Rectangle { x1 = 48.999969482421875, x2 = 230.99520874023438, y1 = 57.999969482421875, y2 = 216.9903564453125 },Rectangle.Rectangle { x1 = 274, x2 = 468.98793029785156, y1 = 57.999969482421875, y2 = 216.9903564453125 },Rectangle.Rectangle { x1 = 503, x2 = 706.9903869628906, y1 = 57.999969482421875, y2 = 216.9903564453125 },Rectangle.Rectangle { x1 = 48.999969482421875, x2 = 230.99514770507812, y1 = 266.0000305175781, y2 = 427.99522399902344 },Rectangle.Rectangle { x1 = 274, x2 = 468.98793029785156, y1 = 266.0000305175781, y2 = 427.99522399902344 },Rectangle.Rectangle { x1 = 503, x2 = 706.9903869628906, y1 = 266.0000305175781, y2 = 427.99522399902344 }]

    wrappedRow2 : List Rectangle.Rectangle
    wrappedRow2 = [Rectangle.Rectangle { x1 = 20, x2 = 171, y1 = 20, y2 = 38 },Rectangle.Rectangle { x1 = 191, x2 = 372, y1 = 20, y2 = 38 },Rectangle.Rectangle { x1 = 20, x2 = 143, y1 = 51, y2 = 69 }]

    row : List Rectangle.Rectangle
    row = [Rectangle.Rectangle { x1 = 27, x2 = 160, y1 = 17.5, y2 = 65.5 },Rectangle.Rectangle { x1 = 274, x2 = 423, y1 = 17.5, y2 = 65.5 }]

    detectFlow wrappedRow
    --> Ok <|
    -->     Collection
    -->     [Rectangle.Rectangle { x1 = 48.999969482421875, x2 = 230.99520874023438, y1 = 57.999969482421875, y2 = 216.9903564453125 },Rectangle.Rectangle { x1 = 274, x2 = 468.98793029785156, y1 = 57.999969482421875, y2 = 216.9903564453125 },Rectangle.Rectangle { x1 = 503, x2 = 706.9903869628906, y1 = 57.999969482421875, y2 = 216.9903564453125 },Rectangle.Rectangle { x1 = 48.999969482421875, x2 = 230.99514770507812, y1 = 266.0000305175781, y2 = 427.99522399902344 },Rectangle.Rectangle { x1 = 274, x2 = 468.98793029785156, y1 = 266.0000305175781, y2 = 427.99522399902344 },Rectangle.Rectangle { x1 = 503, x2 = 706.9903869628906, y1 = 266.0000305175781, y2 = 427.99522399902344 }]
    -->     Spec.Element.Layout.WrappedRow

    detectFlow wrappedRow2
    --> Ok <|
    -->     Collection
    -->     wrappedRow2
    -->     Spec.Element.Layout.WrappedRow

    detectFlow row
    --> Ok <|
    -->     Collection
    -->     [Rectangle.Rectangle { x1 = 27, x2 = 160, y1 = 17.5, y2 = 65.5 },Rectangle.Rectangle { x1 = 274, x2 = 423, y1 = 17.5, y2 = 65.5 }]
    -->     Spec.Element.Layout.Row



    detectFlow

-}
detectFlow : List Rectangle.Rectangle -> Result FlowDetectionError Layout
detectFlow drawn =
    case drawn of
        -- empty element
        [] ->
            Err Empty

        -- single element - no flow direction
        first :: [] ->
            Ok <| Singleton first

        -- if more than one element was found,
        -- use the first element as anchor for comparing other elements
        first :: rest ->
            let
                -- similar elements are a hint towards lists
                allSimilarHeight =
                    List.all
                        (Rectangle.similarHeight first)
                        rest

                -- a layout can be a column if elements are not overlapping on the Y axis
                canBeColumn =
                    List.all
                        (\( a, b ) -> Rectangle.notOverlappingY a b)
                        (List.Extra.uniquePairs drawn)

                -- a layout can be a regular row if elements are in a row and not overlapping on X
                canBeRow =
                    List.all
                        (\( a, b ) -> Rectangle.notOverlappingX a b)
                        (List.Extra.uniquePairs drawn)

                -- sort by x position
                layout =
                    -- try fitting into row or column layouts
                    case ( canBeColumn, canBeRow, allSimilarHeight ) of
                        ( True, _, _ ) ->
                            Ok (Collection (List.sortBy Rectangle.y1 drawn) Spec.Element.Layout.Column)

                        ( False, True, _ ) ->
                            Ok (Collection (List.sortBy Rectangle.x1 drawn) Spec.Element.Layout.Row)

                        -- simple Responsive fix
                        ( False, False, True ) ->
                            case toWrappedRows drawn of
                                Ok rows ->
                                    rows
                                        -- sort each row
                                        |> List.map sortRow
                                        -- reverse because the reducer returns them in revers Y order [TODO: THINK ABOUT / WRITE TESTS]
                                        |> List.reverse
                                        -- flatten
                                        |> List.concatMap identity
                                        -- finalize
                                        |> toWrappedRowLayout
                                        |> Ok

                                Err err ->
                                    Err err

                        -- elements are not similar enough to detect repetition
                        -- note: this can be improved by using different node data like it being a symbol
                        ( False, False, False ) ->
                            Err TooDissimilar

                -- refinedLayout =
                --   case layout of
                --     Err _ ->
                --         layout
                --
                --     Ok (Singleton _) ->
                --         layout
                --
                --     Ok (Collection sortedRect layout_) ->
                --       let
                --         sortedRect_ =
                --           detectClustering sortedRect
                --
                --       in
                --       Ok (Collection sortedRect_ layout_)
            in
            layout


{-| Try detecting the clusteres
-}
detectClustering : List ( Spec.Element.Id.Id, Rectangle.Rectangle ) -> List Rectangle.DetectedCluster
detectClustering drawn =
    case drawn of
        -- empty element
        [] ->
            []

        _ ->
            let
                isSingle l =
                    case List.length l of
                        1 ->
                            False

                        _ ->
                            True

                -- compare each element to determine if they are arranged in rows
                clustersRow =
                    List.map (Rectangle.similarRow drawn) drawn
                        |> List.Unique.filterDuplicates
                        |> List.filter isSingle

                -- if the number of rects within the cluster is the same as the total number of rects on the
                -- screen assume the responsify algo will pick them up...
                -- TODO improve this hack
            in
            clustersRow


{-| Wrap a list of elements into the appropriate type for a wrapped row
-}
toWrappedRowLayout drawn =
    Collection
        drawn
        Spec.Element.Layout.WrappedRow


sortRow ( firstRect, otherRects ) =
    List.sortBy Rectangle.x1 (firstRect :: otherRects)


{-| Try fitting each rectangle into a row that already exists or creat a new one
Rows are NEVER next to each other
-}
toWrappedRows : List Rectangle.Rectangle -> Result FlowDetectionError Rows
toWrappedRows drawn =
    List.foldl rowFittingReducer (Ok []) drawn


type alias Rows =
    List Row


{-| A row always has a head to simplify further processing
-}
type alias Row =
    ( Rectangle.Rectangle, List Rectangle.Rectangle )


rowFittingReducer : Rectangle.Rectangle -> Result FlowDetectionError Rows -> Result FlowDetectionError Rows
rowFittingReducer nextRect rowsBuiltSoFar =
    case rowsBuiltSoFar of
        Err err ->
            Err err

        -- if there are no rows yet, make the first row
        Ok [] ->
            Ok [ ( nextRect, [] ) ]

        Ok atLeastOneComingRow ->
            -- fit the current rectangle into existing rows
            tryFittingInto
                []
                -- initializing recursion: there are no past rows
                atLeastOneComingRow
                nextRect


tryFittingInto : Rows -> Rows -> Rectangle.Rectangle -> Result FlowDetectionError Rows
tryFittingInto passedRows nextRows rect =
    case nextRows of
        -- can not fit if there are no rows left
        [] ->
            Err CanNotFit

        -- try fitting into the next row
        currentRow :: otherRows ->
            case fitsInto rect currentRow of
                -- if it fits, put the rect into the current row and put the current row between the past and coming rows
                Fits ->
                    Ok (passedRows ++ (addRect rect currentRow :: otherRows))

                -- if it does not fit but conflicts with the boundaries, return an error
                Conflict ->
                    Err CanNotFit

                -- if it does not fit, add the current row to the past rows and retry with the rest
                NotFitting ->
                    case otherRows of
                        [] ->
                            Ok (( rect, [] ) :: passedRows ++ nextRows)

                        _ ->
                            tryFittingInto (currentRow :: passedRows) otherRows rect


addRect : Rectangle.Rectangle -> Row -> Row
addRect rect ( first, rest ) =
    ( first, rect :: rest )


type FittingResult
    = Fits
    | NotFitting
    | Conflict


{-| Check if a rectangle fits into a row of other rectangles
-}
fitsInto : Rectangle.Rectangle -> Row -> FittingResult
fitsInto rect row =
    let
        ( firstRectInRow, otherRects ) =
            row

        -- detect the distance of the current rectangle from the centroid of the first one
        centerDist =
            abs (Rectangle.centerY rect) - Rectangle.centerY firstRectInRow

        -- there is some height / similarity tolerance
        -- Tolerance should be controllable by user when snapping is good enough
        maxHeight =
            max
                (Rectangle.height firstRectInRow)
                (Rectangle.height rect)

        precisionFactor =
            5

        -- the bigger the factor the smaller the tolerance
        centerAligned =
            -- the distance from the center is relative to the height of the rectangle
            centerDist * precisionFactor < maxHeight

        -- this does check intersections but if an element is
        -- 1. outside the tolerance of a track and
        -- 2. intersects the boundaries of another track
        notCollidingWithAnythingOnX =
            List.all (Rectangle.notOverlappingX rect) (firstRectInRow :: otherRects)

        notCollidingWithAnythingOnY =
            List.all (Rectangle.notOverlappingY rect) (firstRectInRow :: otherRects)
    in
    case ( centerAligned, notCollidingWithAnythingOnX, notCollidingWithAnythingOnY ) of
        -- it fits if is horizontall aligned and does not collide with anything on the X-axis
        ( True, True, _ ) ->
            Fits

        -- it is in a different row when it is not aligned and not colliding with anything on the Y-Axis it is on a different row
        ( False, True, _ ) ->
            NotFitting

        -- when it is colliding on the X-axis and the Y-axis we can not determine the layout
        ( _, False, False ) ->
            Conflict

        ( _, False, True ) ->
            NotFitting
