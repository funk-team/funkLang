module Canvas.AttributesPanel.Layout exposing (view)

{-| Layout panel in the right sidebar
-}

import Canvas.AttributesPanel.Shared
import Canvas.Events
import Canvas.Msg
import Canvas.Selection
import Canvas.Tool
import Canvas.Tool.AugmentationParams
import Canvas.Tool.Responsify
import Element
import Element.Font
import Html
import Html.Parser
import Interface.Scope
import Model
import Model.Model
import Rectangle
import Spec.Element
import Spec.Element.Controls.Layout
import Spec.Element.Controls.Layout.Context
import Spec.Element.Controls.Layout.SizeAndAlignment
import Spec.Element.Model
import Spec.Mutation


{-| Provide controls for laying out elements and their children
-}
view :
    Model.Model.Model
    -> ( Canvas.Selection.SelectionItem, Spec.Element.Model.EitherElement )
    -> Element.Element Canvas.Msg.Msg
view rootModel ( selectionItem, element ) =
    let
        model =
            Model.latest rootModel

        augmentationParams : Canvas.Tool.AugmentationParams.AugmentationParams
        augmentationParams =
            { camera = model.camera
            , selectionItem = selectionItem
            , selection = Just selectionItem
            , element = element
            , scope = Interface.Scope.populateForElement selectionItem model
            , pressedKeys = rootModel.pressedKeys
            }

        responsifySection =
            Element.column
                Canvas.AttributesPanel.Shared.sectionStyles
                [ Element.el
                    [ Element.Font.bold
                    , Element.paddingEach { edges | bottom = 15 }
                    ]
                  <|
                    Element.text "Arrangement"
                , responsify
                ]

        responsify =
            Canvas.Tool.Responsify.renderPanel
                (Canvas.Tool.Responsify.detectEnvironment rootModel.url)
                model
                augmentationParams
                model.evolveState
                |> Element.map Canvas.Msg.ResponsifyToolMsg

        { shared, outerGeometry } =
            element

        -- display both size and alignment
        sizeAndAlignmentControls =
            case outerGeometry of
                Spec.Element.Model.FlowElementGeometry geometry ->
                    Spec.Element.Controls.Layout.SizeAndAlignment.viewFlowGeometryControls
                        model
                        selectionItem
                        Nothing
                        geometry
                        |> Element.map
                            (\geo ->
                                Canvas.Msg.GotMutation <|
                                    Spec.Mutation.UpdateFlowElement selectionItem
                                        { outerGeometry = geo, shared = shared }
                            )

                Spec.Element.Model.ScreenGeometry screenSize ->
                    let
                        (Canvas.Events.AbsoluteRectangle rect) =
                            Spec.Mutation.screenSizeToAbsoluteRectangle screenSize
                    in
                    Spec.Element.Controls.Layout.SizeAndAlignment.viewScreenGeometryControls rect
                        |> Element.map
                            (\geo ->
                                let
                                    -- if the size of an element has not changed we keep the preset and just update the offset
                                    -- else we approximate the preset.
                                    sizeUnchanged =
                                        (Rectangle.width geo == Rectangle.width rect) && (Rectangle.height geo == Rectangle.height rect)

                                    maybePreset =
                                        case ( element.outerGeometry, sizeUnchanged ) of
                                            ( Spec.Element.Model.ScreenGeometry (Spec.Element.Model.Preset device _), True ) ->
                                                Spec.Element.Model.Preset
                                                    device
                                                    { x = Rectangle.x1 geo
                                                    , y = Rectangle.y1 geo
                                                    }

                                            _ ->
                                                Spec.Element.rectangleToScreenSize geo
                                in
                                { shared = element.shared, outerGeometry = maybePreset }
                                    |> Spec.Mutation.UpdateScreen selectionItem
                                    |> Canvas.Msg.GotMutation
                            )

                Spec.Element.Model.AbsoluteElementGeometry dimensions ->
                    Spec.Element.Controls.Layout.SizeAndAlignment.viewAbsoluteGeometryControls
                        model
                        selectionItem
                        dimensions
                        |> Element.map
                            (\geo ->
                                Canvas.Msg.GotMutation <|
                                    Spec.Mutation.UpdateAbsoluteElement selectionItem
                                        { outerGeometry = geo
                                        , shared = shared
                                        }
                            )

        setPadding =
            Element.map
                (\padding -> Canvas.Msg.GotMutation <| Spec.Mutation.UpdateShared selectionItem { shared | padding = padding })

        setFlow =
            Element.map
                (\flow -> Canvas.Msg.GotMutation <| Spec.Mutation.UpdateShared selectionItem { shared | flow = flow })

        setSpacing =
            Element.map
                (\spacing -> Canvas.Msg.GotMutation <| Spec.Mutation.UpdateShared selectionItem { shared | spacing = spacing })

        contextControls =
            case shared.children of
                Spec.Element.Model.AbsoluteChildren _ ->
                    [ Spec.Element.Controls.Layout.Context.paddingSelector shared.padding |> setPadding
                    , responsifySection
                    ]

                -- padding only has an effect when we have more than one child
                -- but if we have only one child it can still be repeated
                -- here we do not know if it will be repeated
                -- TODO: Improve display of padding selector when there are at least two rendered children
                Spec.Element.Model.FlowChildren [] ->
                    [ Spec.Element.Controls.Layout.Context.paddingSelector shared.padding |> setPadding
                    , responsifySection
                    ]

                Spec.Element.Model.FlowChildren manyChildren ->
                    [ Spec.Element.Controls.Layout.Context.paddingSelector shared.padding |> setPadding
                    , Spec.Element.Controls.Layout.Context.arrangementSelector shared.flow |> setFlow
                    , Spec.Element.Controls.Layout.Context.spacingControls shared
                        |> setSpacing
                    ]
    in
    Element.column
        [ Element.width Element.fill ]
        (sizeAndAlignmentControls :: contextControls)



-- ELEMENT-SPECIFIC CONTROLS


edges =
    { bottom = 0, left = 0, right = 0, top = 0 }


{-| This is a hotfix: If we set the code without parsing, it will trigger some kind of re-render for the code editor element, causing it to break
Another idea would be to defer removal or sth. Nevertheless: to be investiaged
-}
setCodeIfItParses id code =
    case Html.Parser.run code of
        Ok _ ->
            Canvas.Msg.SetLabel id code

        Err _ ->
            Canvas.Msg.NoOp


editor value =
    let
        rendered =
            Html.text "Coming soon"
    in
    Element.row [ Element.spacing 10 ]
        [ rendered |> Element.html ]
