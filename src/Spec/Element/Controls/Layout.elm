module Spec.Element.Controls.Layout exposing (..)

{-| Render controls for defining layout, i.e. Non-Decorative attributes
-}

import Canvas.Selection
import Element
import Element.Background
import Element.Input
import List.Extra
import Model.Model
import Spec.Element
import Spec.Element.Controls.Layout.SizeAndAlignment
import Spec.Element.Model
import Ui.Style


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


absoluteLengthInput =
    Element.Input.text
        []
        { placeholder = Nothing
        , label = Element.Input.labelAbove [] (Element.text "px")
        , onChange = identity
        , text = ""
        }


highlight =
    [ Element.Background.color Ui.Style.highlightColor ]


sizeAndAlignmenControlsOfSingleChild :
    Model.Model.UserModel
    -> Canvas.Selection.SelectionItem
    -> Int
    -> Spec.Element.Model.FlowElement
    -> Element.Element Spec.Element.Model.FlowElement
sizeAndAlignmenControlsOfSingleChild userModel selectionItem index flowElement =
    Element.map
        (\y -> { flowElement | outerGeometry = y })
    <|
        Spec.Element.Controls.Layout.SizeAndAlignment.viewFlowGeometryControls
            userModel
            selectionItem
            (Just <| index + 1)
            flowElement.outerGeometry


viewSizeAndAlignmentControlsForMultipleFlowElements :
    Model.Model.UserModel
    -> Canvas.Selection.SelectionItem
    -> List Spec.Element.Model.FlowElement
    -> Element.Element (List Spec.Element.Model.FlowElement)
viewSizeAndAlignmentControlsForMultipleFlowElements userModel selectionItem allChildren =
    let
        viewOne index el =
            el
                |> sizeAndAlignmenControlsOfSingleChild
                    userModel
                    (Canvas.Selection.addOne el.shared.id selectionItem)
                    index
                |> Element.map (\x -> List.Extra.setAt index x allChildren)
    in
    Element.column [] <| List.indexedMap viewOne allChildren
