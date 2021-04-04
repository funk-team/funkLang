module Canvas.Tool.Responsify.Transforms exposing
    ( FlowBuilder
    , IndexedFlowBuilder
    , SimpleFlowBuilder
    , TransformExtras
    , columnTransform
    , columnTransformWith
    , defaultFlowConfig
    , genericColumnXAlign
    , genericRowYAlign
    , indexedColumnTransform
    , indexedColumnTransformWith
    , indexedRowTransform
    , indexedRowTransformWith
    , rowTransform
    , simpleColumnTransform
    , simpleColumnTransformWith
    , simpleRowTransform
    , simpleRowTransformWith
    , singletonTransform
    , transform
    )

import Canvas.Tool.Responsify.Info exposing (Dim(..), Fact(..), Info, Param(..), getDim, getFact)
import Canvas.Tool.Responsify.Rules exposing (MatchResult(..), Transform)
import Canvas.Tool.Responsify.Types exposing (Drawn, sortDrawnChildren)
import Canvas.Tool.Responsify.Utils exposing (bottomCheck, centerCheckX, centerCheckY, correctPadding, leftCheck, orderAbsoluteElementsByID, rightCheck, topCheck)
import Maybe.Extra exposing (orElse)
import Rectangle exposing (Rectangle)
import Spec.Convert.Util exposing (correctSpacing)
import Spec.Element as Element
import Spec.Element.Layout as Layout exposing (Flow(..))
import Spec.Element.Layout.Padding exposing (Padding)
import Spec.Element.Model exposing (AbsoluteElement, Children(..), FlowElement, SizeAndAlignment)
import Spec.Element.Style.Edges exposing (EdgeDimensions)



-- Function types used in tree transforms.


type alias TransformExtras a =
    Info -> Drawn -> List a


type alias SimpleFlowBuilder =
    Info -> Drawn -> Padding -> AbsoluteElement -> Rectangle -> SizeAndAlignment


type alias FlowBuilder a =
    a -> SimpleFlowBuilder


type alias OptFlowBuilder a =
    FlowBuilder (Maybe a)


type alias IndexedFlowBuilder =
    FlowBuilder Int



-- Overriding for layout configuration.


type alias FlowConfig =
    { spacing : Maybe Int
    , flow : Maybe Flow
    , sort : Rectangle -> Float
    , correctPadding : EdgeDimensions -> Padding
    }


type alias FlowConfigBuilder =
    Info -> Drawn -> FlowConfig


defaultFlowConfig : FlowConfig
defaultFlowConfig =
    { spacing = Nothing
    , flow = Nothing
    , sort = Rectangle.x1
    , correctPadding = correctPadding
    }



-- Common transform behaviour for row-like and singleton layouts.
-- Parameterised over a flow geometry builder and a generator of extra
-- information needed by that builder.


rowTransform : TransformExtras a -> FlowBuilder a -> Transform m
rowTransform =
    transform (\_ _ -> defaultFlowConfig)


columnTransform : TransformExtras a -> FlowBuilder a -> Transform m
columnTransform =
    transform
        (\_ _ ->
            { defaultFlowConfig
                | flow = Just Column
                , sort = Rectangle.y1
            }
        )


columnTransformWith : FlowConfigBuilder -> TransformExtras a -> FlowBuilder a -> Transform m
columnTransformWith configBuilder =
    transform
        (\info data ->
            let
                config =
                    configBuilder info data
            in
            { config
                | flow = Just Column
                , sort = Rectangle.y1
            }
        )


indexedRowTransform : IndexedFlowBuilder -> Transform m
indexedRowTransform builder =
    rowTransform
        (\_ d -> List.range 0 (List.length d.children - 1))
        (\idx abs data rect -> builder idx abs data rect)


indexedRowTransformWith : FlowConfigBuilder -> IndexedFlowBuilder -> Transform m
indexedRowTransformWith configBuilder builder =
    transform
        configBuilder
        (\_ d -> List.range 0 (List.length d.children - 1))
        (\idx abs data rect -> builder idx abs data rect)


indexedColumnTransform : IndexedFlowBuilder -> Transform m
indexedColumnTransform builder =
    columnTransform
        (\_ d -> List.range 0 (List.length d.children - 1))
        (\idx abs data rect -> builder idx abs data rect)


indexedColumnTransformWith : FlowConfigBuilder -> IndexedFlowBuilder -> Transform m
indexedColumnTransformWith configBuilder builder =
    columnTransformWith
        configBuilder
        (\_ d -> List.range 0 (List.length d.children - 1))
        (\idx abs rect -> builder idx abs rect)


simpleRowTransform : SimpleFlowBuilder -> Transform m
simpleRowTransform builder =
    rowTransform
        (\_ d -> List.repeat (List.length d.children) ())
        (\_ abs rect -> builder abs rect)


simpleRowTransformWith : FlowConfigBuilder -> SimpleFlowBuilder -> Transform m
simpleRowTransformWith configBuilder builder =
    transform
        configBuilder
        (\_ d -> List.repeat (List.length d.children) ())
        (\_ abs rect -> builder abs rect)


simpleColumnTransform : SimpleFlowBuilder -> Transform m
simpleColumnTransform builder =
    columnTransform
        (\_ d -> List.repeat (List.length d.children) ())
        (\_ abs rect -> builder abs rect)


simpleColumnTransformWith : FlowConfigBuilder -> SimpleFlowBuilder -> Transform m
simpleColumnTransformWith configBuilder builder =
    columnTransformWith
        configBuilder
        (\_ d -> List.repeat (List.length d.children) ())
        (\_ abs rect -> builder abs rect)


singletonTransform : FlowConfigBuilder -> SimpleFlowBuilder -> Transform m
singletonTransform configBuilder builder =
    transform
        configBuilder
        (\_ d -> List.repeat (List.length d.children) Nothing)
        (\_ abs rect -> builder abs rect)



-- Basic transform, pulling out common features for row and singleton
-- layouts.


transform : FlowConfigBuilder -> TransformExtras a -> FlowBuilder a -> Transform m
transform configBuilder extras builder userModel inData info =
    let
        config =
            configBuilder info inData

        data =
            sortDrawnChildren config.sort inData

        spacing =
            Maybe.map round <| getDim MeanSpacing info

        correctedSpacing =
            correctSpacing data.parent data.children (Just Row) spacing <|
                Rectangle.boundingBox data.children

        padding =
            config.correctPadding <| getPadding data.parent data.children

        -- This doesn't touch the flow of the children, so they need
        -- to be ordered correctly before mapping this function over
        -- them: see the call to orderAbsoluteElementsByID below.
        transformChild :
            FlowBuilder a
            -> a
            -> AbsoluteElement
            -> Rectangle
            -> FlowElement
        transformChild build val ch rect =
            { outerGeometry = build val info data padding ch rect
            , shared = ch.shared
            }

        flowChildren =
            case data.element.shared.children of
                AbsoluteChildren abs ->
                    FlowChildren <|
                        List.map3 (transformChild builder)
                            (extras info data)
                            (orderAbsoluteElementsByID abs data.childIds)
                            data.children

                -- Should never happen as the user can only responsify
                -- absolute children.
                -- TODO: IS IT POSSIBLE TO MAKE THIS NOT APPEAR HERE
                -- (OR INDEED ANYWHERE THAT IT'S KNOWN THAT ONLY
                -- ABSOLUTE CHILDREN CAN APPEAR?)
                FlowChildren _ ->
                    data.element.shared.children
    in
    -- Allow configuration input to override default spacing and
    -- flow here (default flow is Row).
    Element.mapShared
        (\shared ->
            { shared
                | spacing = config.spacing |> orElse correctedSpacing
                , padding = padding
                , flow = Maybe.withDefault Row config.flow
                , children = flowChildren
            }
        )
        data.element


{-| Detect the minimum distance between a list of rectancles and their enclosing rectangle

@@TODO : improve with: symmetry and children not covering the full width

-}
getPadding : Rectangle -> List Rectangle -> EdgeDimensions
getPadding parent children =
    case Rectangle.boundingBox children of
        Nothing ->
            { top = Nothing, bottom = Nothing, left = Nothing, right = Nothing }

        Just childrenBoundingBox ->
            let
                { top, bottom, left, right } =
                    Rectangle.findPadding parent childrenBoundingBox
            in
            { top = round top |> Just
            , bottom = round bottom |> Just
            , left = round left |> Just
            , right = round right |> Just
            }


genericRowYAlign : Info -> Drawn -> Padding -> Rectangle.Rectangle -> Maybe Layout.AlignmentY
genericRowYAlign info data padding r =
    case
        ( getFact IsARow info
        , getFact SameTopOffset info
        , getFact SameBottomOffset info
        )
    of
        ( Just True, _, _ ) ->
            Just Layout.CenterY

        ( _, Just True, _ ) ->
            Just Layout.Top

        ( _, _, Just True ) ->
            Just Layout.Bottom

        _ ->
            case
                ( topCheck data padding r
                , bottomCheck data padding r
                , centerCheckY data r
                )
            of
                ( _, _, True ) ->
                    Just Layout.CenterY

                ( True, _, _ ) ->
                    Just Layout.Top

                ( _, True, _ ) ->
                    Just Layout.Bottom

                _ ->
                    Nothing


genericColumnXAlign : Info -> Drawn -> Padding -> Rectangle.Rectangle -> Maybe Layout.AlignmentX
genericColumnXAlign info data padding r =
    case
        ( leftCheck data padding r
        , rightCheck data padding r
        , centerCheckX data r
        )
    of
        ( _, _, True ) ->
            Just Layout.CenterX

        ( True, _, _ ) ->
            Just Layout.Left

        ( _, True, _ ) ->
            Just Layout.Right

        _ ->
            Nothing
