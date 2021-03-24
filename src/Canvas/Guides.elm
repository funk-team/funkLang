module Canvas.Guides exposing
    ( Alignment
    , SnapReason(..)
    , getXGuides
    , horizontalLine
    , horizontalLineWithinElement
    , snap
    ,  verticalLine
       -- , viewAll

    , verticalLineWithinElement
    , viewHorizontalLine
    , viewReasons
    , viewVerticalLine
    )

{-| Detecting, Applying and Viewing Guides
-}

import Canvas.Camera
import Canvas.Camera.Model
import Canvas.Events
import Dict
import Element
import Element.Background
import Element.Border
import Element.Font
import Html.Attributes
import Rectangle
import Spec.Element.Id
import Spec.Model
import Ui.Help
import Ui.Style



-- SETTINGS --


type alias SnapSettings =
    { guides : Float
    , lengths : Float
    }


settings =
    SnapSettings 5 1


{-| Find the closest guide given a function that maps a rectangle onto the guides' scale
-}
findClosestBy : Canvas.Camera.Model.Model -> Guides -> Rectangle.Rectangle -> (Rectangle.Rectangle -> Float) -> Maybe ( Float, List Alignment )
findClosestBy camera guides someRect metric =
    let
        distanceToSnapPoint : ( Float, List Alignment ) -> Float
        distanceToSnapPoint ( snapPoint, rect ) =
            abs <| snapPoint - metric someRect

        -- is within threshold?
        doesSnap : ( Float, List Alignment ) -> Bool
        doesSnap ( snapPoint, _ ) =
            abs (metric someRect - snapPoint) <= (settings.guides / camera.zoom)
    in
    Dict.toList guides
        -- sort by vicinity
        -- |> List.sortBy distanceToSnapPoint
        -- only keep the ones that actually snap
        |> List.filter doesSnap
        -- only keep the closest one
        |> List.head


findClosestLength : Canvas.Camera.Model.Model -> Guides -> Rectangle.Rectangle -> (Rectangle.Rectangle -> Float) -> SnapReason -> Maybe ( Float, List Alignment )
findClosestLength camera guides someRect metric snapReason =
    let
        distanceToSnapPoint : ( Float, List Alignment ) -> Float
        distanceToSnapPoint ( snapLength, lengthAlignment ) =
            abs <| snapLength - metric someRect

        -- is within threshold?
        doesSnap : ( Float, List Alignment ) -> Bool
        doesSnap ( snapLength, lengthAlignment ) =
            case snapReason of
                Height ->
                    Rectangle.same snapLength (Rectangle.height someRect)

                Width ->
                    Rectangle.same snapLength (Rectangle.width someRect)

                _ ->
                    False
    in
    Dict.toList guides
        -- sort by vicinity
        -- |> List.sortBy distanceToSnapPoint
        -- only keep the ones that actually snap
        |> List.filter doesSnap
        -- only keep the closest one
        |> List.head


snap :
    Canvas.Camera.Model.Model
    -> Spec.Model.Rectangles
    -> Canvas.Events.AbsoluteRectangle
    -> ( Canvas.Events.AbsoluteRectangle, List Alignment )
snap camera existing (Canvas.Events.AbsoluteRectangle originalRect) =
    let
        xSnaps =
            getXGuides existing

        ySnaps =
            getYGuides existing

        centerXSnaps =
            getCenterXGuides existing

        centerYSnaps =
            getCenterYGuides existing

        heightSnaps =
            getHeightGuides existing

        widthSnaps =
            getWidthGuides existing

        ( heightSnapResult, heightSnapReason ) =
            findClosestLength camera heightSnaps originalRect Rectangle.centerY Height
                |> Maybe.withDefault ( Rectangle.centerY originalRect, [] )

        ( widthSnapResult, widthSnapReason ) =
            findClosestLength camera widthSnaps originalRect Rectangle.centerX Width
                |> Maybe.withDefault ( Rectangle.centerX originalRect, [] )

        ( centerXResult, centerXReason ) =
            findClosestBy camera centerXSnaps originalRect Rectangle.centerX
                |> Maybe.withDefault ( Rectangle.centerX originalRect, [] )

        ( centerYResult, centerYReason ) =
            findClosestBy camera centerYSnaps originalRect Rectangle.centerY
                |> Maybe.withDefault ( Rectangle.centerY originalRect, [] )

        ( x1Result, x1Reason ) =
            findClosestBy camera xSnaps originalRect Rectangle.x1
                |> Maybe.withDefault ( Rectangle.x1 originalRect, [] )

        ( x2Result, x2Reason ) =
            findClosestBy camera xSnaps originalRect Rectangle.x2
                |> Maybe.withDefault ( Rectangle.x2 originalRect, [] )

        ( y1Result, y1Reason ) =
            findClosestBy camera ySnaps originalRect Rectangle.y1
                |> Maybe.withDefault ( Rectangle.y1 originalRect, [] )

        ( y2Result, y2Reason ) =
            findClosestBy camera ySnaps originalRect Rectangle.y2
                |> Maybe.withDefault ( Rectangle.y2 originalRect, [] )

        -- try snapping onto borders
        borderSnapResult =
            Rectangle.fromPoints { x1 = x1Result, y1 = y1Result, x2 = x2Result, y2 = y2Result }

        -- try snapping onto center
        centerSnapResult =
            Rectangle.moveCenter { x = centerXResult, y = centerYResult } originalRect

        xNotSnappedToBorder =
            ( Rectangle.x1 borderSnapResult, Rectangle.x2 borderSnapResult )
                == ( Rectangle.x1 originalRect, Rectangle.x2 originalRect )

        yNotSnappedToBorder =
            ( Rectangle.y1 borderSnapResult, Rectangle.y2 borderSnapResult )
                == ( Rectangle.y1 originalRect, Rectangle.y2 originalRect )

        -- try snapping to lengths
        heightSnapped =
            ((Rectangle.height originalRect - settings.lengths) <= heightSnapResult)
                && (heightSnapResult <= (Rectangle.height originalRect + settings.lengths))

        lengthSnapped =
            ((Rectangle.width originalRect - settings.lengths) <= widthSnapResult)
                && (widthSnapResult <= (Rectangle.width originalRect + settings.lengths))

        -- for each axis, if the borders don't snap, try snapping onto center
        -- this allows the user to snap an element to the center and align it to the boundaries of another element
        finalResult =
            let
                ( x1, x2 ) =
                    if lengthSnapped then
                        ( Rectangle.x1 originalRect, Rectangle.x1 originalRect + widthSnapResult )

                    else if xNotSnappedToBorder then
                        ( Rectangle.x1 centerSnapResult, Rectangle.x2 centerSnapResult )

                    else
                        ( Rectangle.x1 borderSnapResult, Rectangle.x2 borderSnapResult )

                ( y1, y2 ) =
                    if heightSnapped then
                        ( Rectangle.y1 originalRect, Rectangle.y1 originalRect + heightSnapResult )

                    else if yNotSnappedToBorder then
                        ( Rectangle.y1 centerSnapResult, Rectangle.y2 centerSnapResult )

                    else
                        ( Rectangle.y1 borderSnapResult, Rectangle.y2 borderSnapResult )
            in
            Rectangle.fromPoints
                { x1 = x1, x2 = x2, y1 = y1, y2 = y2 }

        -- show every element a rectangle is aligned with
        allAlignments =
            -- if the resulting drawn is aligned with a specific guide, Style.importantHighlightColorSolid it
            [ ( Rectangle.x1 finalResult == x1Result, x1Reason )
            , ( Rectangle.x2 finalResult == x2Result, x2Reason )
            , ( Rectangle.y1 finalResult == y1Result, y1Reason )
            , ( Rectangle.y2 finalResult == y2Result, y2Reason )

            -- same goes for center alignment
            , ( Rectangle.centerY finalResult == centerYResult, centerYReason )
            , ( Rectangle.centerX finalResult == centerXResult, centerXReason )

            -- length
            , ( Rectangle.centerY finalResult == centerYResult, heightSnapReason )
            , ( Rectangle.centerX finalResult == centerXResult, widthSnapReason )
            ]
                |> List.filter Tuple.first
                |> List.map Tuple.second
                |> List.concatMap identity
    in
    ( finalResult |> Canvas.Events.AbsoluteRectangle, allAlignments )


type alias HomoMorphism a =
    a -> a


{-| Chain a list of functions
-}
pipe : List (HomoMorphism a) -> HomoMorphism a
pipe fns arg =
    List.foldl
        (\update_ model_ -> update_ model_)
        arg
        fns


{-| guides have a position and more than one origin/cause
-}
type alias Guides =
    Dict.Dict SnapPoint (List Alignment)


{-| We currently do not support detecting padding and such
TODO: Think about this
-}
type SnapReason
    = X1
    | X2
    | Y1
    | Y2
    | CenterX
    | CenterY
    | Height
    | Width


{-| A snap origin can be used to identify which element and where on that element a
-}
type alias Alignment =
    ( SnapReason, Spec.Element.Id.Id )


{-| A guide has no axis
-}
type alias SnapPoint =
    Float


{-| Given a list of metrics, i.e. conversions from Rect to Alignment
Create a dict of guides
-}
getGuides : List (Rectangle.Rectangle -> ( SnapReason, Float )) -> Spec.Model.Rectangles -> Guides
getGuides metrics =
    let
        insertOriginByMetric :
            ( Spec.Element.Id.Id, Rectangle.Rectangle )
            -> (Rectangle.Rectangle -> ( SnapReason, Float ))
            -> Guides
            -> Guides
        insertOriginByMetric ( rectId, rect ) metric =
            let
                measurement : ( SnapReason, Float )
                measurement =
                    metric rect

                ( reason, position ) =
                    measurement
            in
            Dict.update position (insertReason ( reason, rectId ))

        {- for every metric insert add a reason and possibly a guide in our guides -}
        measureAndInsert : ( Spec.Element.Id.Id, Rectangle.Rectangle ) -> Guides -> Guides
        measureAndInsert ( rectId, rect ) =
            let
                updates =
                    List.map
                        (insertOriginByMetric ( rectId, rect ))
                        metrics
            in
            pipe updates

        insertReason : Alignment -> Maybe (List Alignment) -> Maybe (List Alignment)
        insertReason reason reasons =
            case reasons of
                Nothing ->
                    Just [ reason ]

                Just r ->
                    Just <| reason :: r
    in
    Spec.Element.Id.dictToList
        >> List.foldl
            measureAndInsert
            Dict.empty


getXGuides : Spec.Model.Rectangles -> Guides
getXGuides =
    getGuides [ Rectangle.x1 >> Tuple.pair X1, Rectangle.x2 >> Tuple.pair X2 ]


getYGuides : Spec.Model.Rectangles -> Guides
getYGuides =
    getGuides [ Rectangle.y1 >> Tuple.pair Y1, Rectangle.y2 >> Tuple.pair Y2 ]


getCenterXGuides : Spec.Model.Rectangles -> Guides
getCenterXGuides =
    getGuides
        [ Rectangle.centerX >> Tuple.pair CenterX
        ]


getCenterYGuides : Spec.Model.Rectangles -> Guides
getCenterYGuides =
    getGuides
        [ Rectangle.centerY >> Tuple.pair CenterY
        ]


getHeightGuides : Spec.Model.Rectangles -> Guides
getHeightGuides =
    getGuides
        [ Rectangle.height >> Tuple.pair Height
        ]


getWidthGuides : Spec.Model.Rectangles -> Guides
getWidthGuides =
    getGuides
        [ Rectangle.width >> Tuple.pair Width
        ]


viewReasons : Canvas.Camera.Model.Model -> Spec.Model.Rectangles -> List Alignment -> List (Element.Element msg)
viewReasons camera drawn reasons =
    let
        v ( reason, rectId ) =
            let
                rect =
                    Spec.Element.Id.getFromDict rectId drawn
            in
            case rect of
                Nothing ->
                    Nothing

                Just r ->
                    Just <|
                        case reason of
                            X1 ->
                                [ viewVerticalLine camera (Rectangle.x1 r)
                                , viewCornetDot camera { moveRight = Rectangle.x1 r, moveDown = Rectangle.y1 r }
                                , viewCornetDot camera { moveRight = Rectangle.x1 r, moveDown = Rectangle.y2 r }
                                ]

                            X2 ->
                                [ viewVerticalLine camera (Rectangle.x2 r)
                                , viewCornetDot camera { moveRight = Rectangle.x2 r, moveDown = Rectangle.y1 r }
                                , viewCornetDot camera { moveRight = Rectangle.x2 r, moveDown = Rectangle.y2 r }
                                ]

                            CenterX ->
                                [ viewVerticalLine camera (Rectangle.centerX r)
                                , viewCenterDot camera r
                                ]

                            Y1 ->
                                [ viewHorizontalLine camera (Rectangle.y1 r)
                                , viewCornetDot camera { moveRight = Rectangle.x2 r, moveDown = Rectangle.y1 r }
                                , viewCornetDot camera { moveRight = Rectangle.x1 r, moveDown = Rectangle.y1 r }
                                ]

                            Y2 ->
                                [ viewHorizontalLine camera (Rectangle.y2 r)
                                , viewCornetDot camera { moveRight = Rectangle.x2 r, moveDown = Rectangle.y2 r }
                                , viewCornetDot camera { moveRight = Rectangle.x1 r, moveDown = Rectangle.y2 r }
                                ]

                            CenterY ->
                                [ viewHorizontalLine camera (Rectangle.centerY r)
                                , viewCenterDot camera r
                                ]

                            Height ->
                                [ Element.row []
                                    [ Element.el
                                        [ Element.inFront (viewSnappedHeight camera r)
                                        ]
                                        (highlightVerticleLine camera r)
                                    ]
                                ]

                            Width ->
                                [ Element.row []
                                    [ Element.el
                                        [ Element.inFront (viewSnappedWidth camera r)
                                        ]
                                        (highlightHorozontalLine camera r)
                                    ]
                                ]
    in
    reasons
        |> List.filterMap v
        |> List.concatMap identity


viewSnappedHeight : Canvas.Camera.Model.Model -> Rectangle.Rectangle -> Element.Element msg
viewSnappedHeight { zoom } rect_ =
    let
        offsetFromEdge =
            Rectangle.x2 rect_

        heightValue =
            String.fromInt (Rectangle.height rect_ |> round)
    in
    Element.el
        [ Element.moveDown (Rectangle.centerY rect_ - 15)
        , Element.below
            (Element.el
                ([ Element.centerX
                 , Element.moveRight (Rectangle.x1 rect_)
                 , Ui.Help.noPointerEvents
                 , Element.scale (1 / zoom)
                 , Element.Font.color Ui.Style.lightRed
                 , Element.Font.size 13
                 , Element.padding 5
                 , Element.Border.rounded 3
                 , Element.Background.color Ui.Style.white
                 , Element.Border.color Ui.Style.grey
                 , Element.Border.width 1
                 ]
                    ++ Ui.Help.initFontStyles
                )
                (Element.text heightValue)
            )
        , Ui.Help.noPointerEvents
        ]
        Element.none


viewSnappedWidth : Canvas.Camera.Model.Model -> Rectangle.Rectangle -> Element.Element msg
viewSnappedWidth { zoom } rect_ =
    let
        offsetFromEdge =
            Rectangle.centerX rect_

        heightValue =
            String.fromInt (Rectangle.height rect_ |> round)
    in
    Element.el
        [ Element.moveDown (Rectangle.y2 rect_ - 10)
        , Element.below
            (Element.el
                ([ Element.centerX
                 , Element.moveRight offsetFromEdge
                 , Ui.Help.noPointerEvents
                 , Element.scale (1 / zoom)
                 , Element.Font.color Ui.Style.lightRed
                 , Element.Font.size 13
                 , Element.padding 5
                 , Element.Border.rounded 3
                 , Element.Background.color Ui.Style.white
                 , Element.Border.color Ui.Style.grey
                 , Element.Border.width 1
                 ]
                    ++ Ui.Help.initFontStyles
                )
                (Element.text
                    (String.fromInt (Rectangle.width rect_ |> round))
                )
            )
        , Ui.Help.noPointerEvents
        ]
        Element.none


viewCenterDot : Canvas.Camera.Model.Model -> Rectangle.Rectangle -> Element.Element msg
viewCenterDot { zoom } r =
    let
        centerX =
            Rectangle.centerX r - (radius * 2 |> round |> toFloat |> (*) 0.5)

        centerY =
            Rectangle.centerY r - (radius * 2 |> round |> toFloat |> (*) 0.5)

        diameter =
            Element.px (radius * 2 |> round)

        radius =
            3 / zoom
    in
    Element.el
        [ Element.Background.color Ui.Style.importantHighlightColorSolid
        , Element.width diameter
        , Element.height diameter
        , Element.Border.rounded (radius |> round)
        , Element.moveRight (centerX + (radius * 0.25))
        , Element.moveDown (centerY + (radius * 0.25))
        , noPointerEvents
        ]
        Element.none


viewVerticalLine : Canvas.Camera.Model.Model -> Float -> Element.Element msg
viewVerticalLine camera xPos =
    Element.el
        (Element.Background.color Ui.Style.importantHighlightColorSolid :: verticalLine camera xPos)
        Element.none


lineWidth =
    1


verticalLine : Canvas.Camera.Model.Model -> Float -> List (Element.Attribute msg)
verticalLine { zoom } xPos =
    [ Element.moveRight xPos

    -- , Element.width (Element.px (lineWidth / zoom |> round))
    , Element.width (Element.px lineWidth)
    , Element.height Element.fill
    , noPointerEvents
    ]


verticalLineWithinElement : Canvas.Camera.Model.Model -> Float -> List (Element.Attribute msg)
verticalLineWithinElement { zoom } xPos =
    [ Element.moveRight xPos

    -- , Element.width (Element.px (lineWidth / zoom |> round))
    , Element.width (Element.px lineWidth)
    , Element.height (Element.fill |> Element.minimum 10)
    , noPointerEvents
    ]


noPointerEvents =
    Element.htmlAttribute (Html.Attributes.style "pointer-events" "none")


viewHorizontalLine : Canvas.Camera.Model.Model -> Float -> Element.Element msg
viewHorizontalLine camera yPos =
    Element.el
        (Element.Background.color Ui.Style.importantHighlightColorSolid :: horizontalLine camera yPos)
        Element.none


horizontalLine : Canvas.Camera.Model.Model -> Float -> List (Element.Attribute msg)
horizontalLine { zoom } yPos =
    [ Element.moveDown yPos
    , Element.width Element.fill

    -- , Element.height (Element.px (lineWidth / zoom |> round))
    , Element.height (Element.px lineWidth)

    -- , Element.moveUp 1
    , noPointerEvents
    ]


highlightHorozontalLine : Canvas.Camera.Model.Model -> Rectangle.Rectangle -> Element.Element msg
highlightHorozontalLine camera r =
    let
        styles =
            [ Element.moveDown (Rectangle.y2 r)
            , Element.width (Element.px (round (Rectangle.width r)))
            , Element.height (Element.px lineWidth)
            , Element.moveRight (Rectangle.x1 r)
            , noPointerEvents
            ]
    in
    Element.el
        (Element.Background.color Ui.Style.importantHighlightColorSolid :: styles)
        Element.none


highlightVerticleLine : Canvas.Camera.Model.Model -> Rectangle.Rectangle -> Element.Element msg
highlightVerticleLine camera r =
    let
        styles =
            [ Element.moveDown (Rectangle.y1 r)
            , Element.width (Element.px lineWidth)
            , Element.height (Element.px (round (Rectangle.height r)))
            , Element.moveRight (Rectangle.x1 r)
            , noPointerEvents
            ]
    in
    Element.el
        (Element.Background.color Ui.Style.importantHighlightColorSolid :: styles)
        Element.none


horizontalLineWithinElement : Canvas.Camera.Model.Model -> Float -> List (Element.Attribute msg)
horizontalLineWithinElement { zoom } yPos =
    [ Element.moveDown yPos
    , Element.width (Element.fill |> Element.minimum 10)

    -- , Element.height (Element.px (lineWidth / zoom |> round))
    , Element.height (Element.px lineWidth)
    , Element.moveUp 1
    , noPointerEvents
    ]


type alias CornerDot =
    { moveRight : Float, moveDown : Float }


viewCornetDot : Canvas.Camera.Model.Model -> CornerDot -> Element.Element msg
viewCornetDot { zoom } cornetDot =
    let
        diameter =
            Element.px (radius * 2 |> round)

        radius =
            3 / zoom
    in
    Element.el
        [ Element.Background.color Ui.Style.importantHighlightColorSolid
        , Element.width diameter
        , Element.height diameter
        , Element.Border.rounded (radius |> round)
        , Element.moveRight (cornetDot.moveRight - radius)
        , Element.moveDown (cornetDot.moveDown - radius)
        , noPointerEvents
        ]
        Element.none
