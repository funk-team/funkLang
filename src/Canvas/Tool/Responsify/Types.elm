module Canvas.Tool.Responsify.Types exposing
    ( Drawn
    , PredictedOuterGeometry
    , PredictionParams
    , predictionParamsToDrawn
    , sortDrawnChildren
    )

import Canvas.Events
import Rectangle exposing (Rectangle)
import Spec.Element.Id exposing (Id)
import Spec.Element.Layout exposing (Alignment, Size)
import Spec.Element.Model exposing (EitherElement)


type alias PredictionParams =
    { parentDimensions : ( Id, Canvas.Events.ElementRectangle )
    , siblingsDimensions : List ( Id, Canvas.Events.ElementRectangle )
    , element : EitherElement
    }


type alias PredictedOuterGeometry =
    { id : Id
    , alignment : Alignment
    , size : Size
    }



-- Prediction input data pulled out into a form that's easier to deal
-- with here.
-- TODO: STILL UNDECIDED ABOUT TUPLING IDS AND RECTANGLES TOGETHER
-- HERE.
-- type alias Drawn =
--     { parent : (Id, Rectangle)
--     , children : List (Id, Rectangle) }


type alias Drawn =
    { parentId : Id
    , childIds : List Id
    , parent : Rectangle
    , children : List Rectangle
    , element : EitherElement
    }



-- Convert prediction input data into a form that's easier to
-- manipulate here.


predictionParamsToDrawn : PredictionParams -> Drawn
predictionParamsToDrawn data =
    let
        ( parentId, Canvas.Events.ElementRectangle parentRect ) =
            data.parentDimensions

        ( childIds, children ) =
            data.siblingsDimensions
                |> List.map (\( i, Canvas.Events.ElementRectangle r ) -> ( i, r ))
                |> List.unzip
    in
    Drawn parentId childIds parentRect children data.element



-- Sort child elements by some criterion.


sortDrawnChildren : (Rectangle -> Float) -> Drawn -> Drawn
sortDrawnChildren cmp drawn =
    let
        ( cids, crects ) =
            List.map2 Tuple.pair drawn.childIds drawn.children
                |> List.sortBy (Tuple.second >> cmp)
                |> List.unzip
    in
    { drawn
        | childIds = cids
        , children = crects
    }
