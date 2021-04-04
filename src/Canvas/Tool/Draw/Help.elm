module Canvas.Tool.Draw.Help exposing (..)

import Canvas.Events
import Spec.Element.Layout
import Spec.Element.Model


type alias RelevantForScreen =
    { newChildDimensions : Canvas.Events.AbsoluteRectangle
    , sceneElement : Spec.Element.Model.Screen
    , allSiblings : List Canvas.Events.AbsoluteRectangle
    , absoluteElementContextRect : Canvas.Events.AbsoluteRectangle
    }


type alias ResultingData =
    ( Spec.Element.Model.AbsoluteElement, Spec.Element.Layout.Layout )



{- This function does coordinate the heavy lifting of the smart tool logic
   makeScreen : RelevantForScreen -> ResultingData
   makeScreen data =
       let
           { sceneElement } =
               data

           -- make sure we got the context right
           allSiblings =
               data.allSiblings
                   |> List.map (\(Canvas.Events.AbsoluteRectangle rect) -> rect)

           (Canvas.Events.AbsoluteRectangle newChildDimensions) =
               data.newChildDimensions

           (Canvas.Events.AbsoluteRectangle absoluteElementContextRect) =
               data.absoluteElementContextRect

           size : Spec.Element.Layout.Size
           size =
               { width =
                   { behavior = Spec.Element.Layout.Static (Rectangle.width newChildDimensions |> round)
                   , minMax = Spec.Element.Layout.MinMax Nothing Nothing
                   }
               , height =
                   { behavior = Spec.Element.Layout.Static (Rectangle.height newChildDimensions |> round)
                   , minMax = Spec.Element.Layout.MinMax Nothing Nothing
                   }
               }

           newLayout : Spec.Element.Model.Layout
           newLayout =
               { size = size
               , layout = Spec.Element.Layout.Column

               -- figure out padding basaed on where user has drawn rectangle
               -- check if first element in root, if so set to 0
               , padding = 0
               , spacing = 0
               , alignment = { x = Spec.Element.Layout.Left, y = Spec.Element.Layout.Top }
               }

           padding : Int
           padding =
               case Spec.Convert.Util.getPadding absoluteElementContextRect allSiblings of
                   Nothing ->
                       0

                   Just paddingFloat ->
                       round paddingFloat

           ( flow, spacing ) =
               case Rectangle.Layout.detectFlow allSiblings of
                   Err _ ->
                       ( sceneElement.shared.flow, Just 0 )

                   Ok (Rectangle.Layout.Singleton _) ->
                       ( sceneElement.shared.flow, Just 0 )

                   Ok (Rectangle.Layout.Collection _ layout_) ->
                       case layout_ of
                           Spec.Element.Layout.Column ->
                               Spec.Convert.Util.getSpacing allSiblings Rectangle.y1 Rectangle.y2
                                   |> Maybe.map round
                                   |> Tuple.pair layout_

                           Spec.Element.Layout.Row ->
                               Spec.Convert.Util.getSpacing allSiblings Rectangle.x1 Rectangle.x2
                                   |> Maybe.map round
                                   |> Tuple.pair layout_

                           Spec.Element.Layout.WrappedRow ->
                               Spec.Convert.Util.getSpacing allSiblings Rectangle.x1 Rectangle.x2
                                   |> Maybe.map round
                                   |> Tuple.pair layout_

           newElement : Spec.Element.Model.AbsoluteElement
           newElement =

           result =
               ( newElement
               , newLayout
               )
       in
       result
-}
