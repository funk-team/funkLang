module Spec.Element.Controls.Layout.SizeAndAlignment exposing (..)

import Canvas.AttributesPanel.Shared
import Canvas.Selection
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Interface.Scope
import Model.Model
import Rectangle
import Renderer.Layout
import Spec.Element.Layout
import Spec.Element.Layout.Length
import Spec.Element.Model
import Spec.Model
import Ui.Dropdown
import Ui.LinkableInput
import Ui.Scrubbable
import Ui.Style


edges =
    Ui.Style.edges


viewAlignmentSelector : Spec.Element.Layout.Alignment -> Element.Element Spec.Element.Layout.Alignment
viewAlignmentSelector { x, y } =
    let
        highlightIf val target =
            let
                highlight_ =
                    if val == Just target then
                        [ Element.Background.color Ui.Style.black
                        , Element.Font.color Ui.Style.white
                        , Element.Border.width 1
                        , Element.mouseOver
                            [ Element.Background.color Ui.Style.transparent
                            , Element.Border.color Ui.Style.black
                            , Element.Font.color Ui.Style.black
                            ]
                        ]

                    else
                        [ Element.mouseOver [ Element.Background.color Ui.Style.lightGrey ]
                        , Element.Border.width 1
                        , Element.Border.color Ui.Style.transparent
                        ]
            in
            [ Element.padding 5
            , Element.Border.rounded 2
            ]
                ++ highlight_

        toggleX target =
            case x of
                Nothing ->
                    { y = y, x = Just target }

                Just someAlignment ->
                    { y = y
                    , x =
                        if someAlignment == target then
                            Nothing

                        else
                            Just target
                    }

        toggleY target =
            case y of
                Nothing ->
                    { x = x, y = Just target }

                Just someAlignment ->
                    { x = x
                    , y =
                        if someAlignment == target then
                            Nothing

                        else
                            Just target
                    }

        selectorX =
            Element.column [ Element.spacing 5 ]
                [ Element.text "X Align"
                , Element.row [ Element.spacing 5, Element.moveLeft 5 ]
                    [ Element.Input.button
                        (highlightIf x Spec.Element.Layout.Left)
                        { label = Element.text "Left"
                        , onPress = Just <| toggleX Spec.Element.Layout.Left
                        }
                    , Element.Input.button
                        (highlightIf x Spec.Element.Layout.CenterX)
                        { label = Element.text "CenterX"
                        , onPress = Just <| toggleX Spec.Element.Layout.CenterX
                        }
                    , Element.Input.button
                        (highlightIf x Spec.Element.Layout.Right)
                        { label = Element.text "Right"
                        , onPress = Just <| toggleX Spec.Element.Layout.Right
                        }
                    ]
                ]

        selectorY =
            Element.column [ Element.spacing 5 ]
                [ Element.text "Y Align"
                , Element.row [ Element.spacing 5, Element.moveLeft 5 ]
                    [ Element.Input.button
                        (highlightIf y Spec.Element.Layout.Top)
                        { label = Element.text "Top"
                        , onPress = Just <| toggleY Spec.Element.Layout.Top
                        }
                    , Element.Input.button
                        (highlightIf y Spec.Element.Layout.CenterY)
                        { label = Element.text "CenterY"
                        , onPress = Just <| toggleY Spec.Element.Layout.CenterY
                        }
                    , Element.Input.button
                        (highlightIf y Spec.Element.Layout.Bottom)
                        { label = Element.text "Bottom"
                        , onPress = Just <| toggleY Spec.Element.Layout.Bottom
                        }
                    ]
                ]
    in
    Element.column [ Element.spacing 10 ] [ selectorX, selectorY ]


buttonRow =
    Element.row [ Element.width Element.fill ]


{-| Provide controls for relative sizing.
-}
viewFlowGeometryControls :
    Model.Model.UserModel
    -> Canvas.Selection.SelectionItem
    -> Maybe Int
    -> Spec.Element.Model.SizeAndAlignment
    -> Element.Element Spec.Element.Model.SizeAndAlignment
viewFlowGeometryControls userModel selection index groupAttribs =
    let
        id =
            Canvas.Selection.getTargetId selection

        scope =
            Interface.Scope.populateForElement selection userModel

        { alignment, size } =
            groupAttribs

        explainer =
            Element.paragraph
                [ Element.Font.color Ui.Style.grey
                ]
                [ Element.text "Alignment can be used to align an Element within another Element. They may push other elements around too." ]

        alignmentEditor =
            Element.column
                (Element.spacing 15 :: Canvas.AttributesPanel.Shared.sectionStyles)
                [ Element.row [ Element.width Element.fill ]
                    [ Element.el [ Element.Font.bold, Element.alignLeft ] <|
                        Element.text "Alignment"
                    , Element.el
                        [ Element.alignRight
                        , Element.Font.bold
                        , Element.Font.color Ui.Style.highlightColorSolidImportant
                        ]
                      <|
                        Element.text <|
                            Maybe.withDefault "" (Maybe.map String.fromInt index)
                    ]
                , explainer
                , viewAlignmentSelector alignment |> Element.map (\a -> { groupAttribs | alignment = a })
                ]

        autoSized =
            Renderer.Layout.autoSizeApplies userModel id scope

        sizeControls =
            Element.column
                Canvas.AttributesPanel.Shared.sectionStyles
                [ Element.el [ Element.Font.bold, Element.paddingEach { edges | bottom = 5 } ] (Element.text "Width")
                , if autoSized.width then
                    Element.paragraph [ Element.paddingEach { edges | top = 5 }, Element.alpha 0.5 ] [ Element.text "Width is automatically determined by size of content. See Style > Image fit." ]

                  else
                    viewLengthSelector
                        userModel
                        selection
                        size.width
                        |> Element.map (\width -> { groupAttribs | size = { size | width = width } })
                , Element.el [ Element.Font.bold, Element.paddingEach { edges | bottom = 5, top = 10 } ] (Element.text "Height")
                , if autoSized.height then
                    Element.paragraph [ Element.paddingEach { edges | top = 5 }, Element.alpha 0.5 ] [ Element.text "Height is automatically determined by size of content. See Style > Image fit." ]

                  else
                    viewLengthSelector
                        userModel
                        selection
                        size.height
                        |> Element.map (\height -> { groupAttribs | size = { size | height = height } })
                ]

        controls =
            [ alignmentEditor, sizeControls ]
    in
    Element.column
        [ Element.width Element.fill ]
        controls


{-| Provide controls for modifying a rectangle
@@TODO: allow direct number input
@@TODO: display values of visually modified elements in realtime
-}
viewScreenGeometryControls :
    Rectangle.Rectangle
    -> Element.Element Rectangle.Rectangle
viewScreenGeometryControls rect =
    let
        (Rectangle.Rectangle dims) =
            rect

        description =
            "The absoloute size and position of this element. Responsify it so it becomes part of the layout flow"

        header =
            Element.column [ Element.spacing 15 ]
                [ Element.el [ Element.Font.bold ] (Element.text "Size & Position")
                , Element.paragraph [ Element.Font.color Ui.Style.grey ] [ Element.text description ]
                ]

        -- WIDTH + HEIGHT
        sizeControls =
            Element.column
                [ Element.spacing 10, Element.width Element.fill ]
                [ Element.row [ Element.spacing 10 ] [ Element.text "Width", Ui.Scrubbable.float (Rectangle.width rect) ]
                    |> Element.map (\width -> Rectangle.fromDimensionsFloat { width = width, height = Rectangle.height rect, x = dims.x1, y = dims.y1 })
                , Element.row [ Element.spacing 10 ] [ Element.text "Height", Ui.Scrubbable.float (Rectangle.height rect) ]
                    |> Element.map (\height -> Rectangle.fromDimensionsFloat { width = Rectangle.width rect, height = height, x = dims.x1, y = dims.y1 })
                ]

        -- X + Y
        positionControls =
            Element.column
                [ Element.spacing 10, Element.width Element.fill ]
                [ Element.row [ Element.spacing 10 ] [ Element.text "X", Ui.Scrubbable.float (Rectangle.x1 rect) ]
                    |> Element.map (\newX -> Rectangle.moveBy (newX - dims.x1) 0 rect)
                , Element.row [ Element.spacing 10 ] [ Element.text "Y", Ui.Scrubbable.float (Rectangle.y1 rect) ]
                    |> Element.map (\newY -> Rectangle.moveBy 0 (newY - dims.y1) rect)
                ]

        controls =
            Element.row
                [ Element.spacing 10
                , Element.paddingEach { edges | top = 15 }
                , Element.width Element.fill
                ]
                [ sizeControls, positionControls ]
    in
    Element.column
        Canvas.AttributesPanel.Shared.sectionStyles
        [ header, controls ]


{-| Provide controls for modifying a rectangle
@@TODO: allow direct number input
@@TODO: display values of visually modified elements in realtime
-}
viewAbsoluteGeometryControls :
    Spec.Model.WithSpec userModel
    -> Canvas.Selection.SelectionItem
    -> Spec.Element.Model.AbsoluteElementDimensions
    -> Element.Element Spec.Element.Model.AbsoluteElementDimensions
viewAbsoluteGeometryControls userModel selection dimensions =
    let
        viewInput =
            Ui.LinkableInput.view userModel selection

        description =
            "The absolute size and position of this element. Responsify it to make it part of the layout flow."

        header =
            Element.column [ Element.spacing 15 ]
                [ Element.el [ Element.Font.bold ] (Element.text "Size & Position")
                , Element.paragraph [ Element.Font.color Ui.Style.grey ] [ Element.text description ]
                ]

        widthAndHeight =
            Element.column [ Element.width (Element.px 50), Element.spacing 10 ]
                [ viewInput
                    { label = "Height"
                    , value = dimensions.height
                    }
                    False
                    |> Element.map (\newHeight -> { dimensions | height = newHeight })
                , viewInput
                    { label = "Width"
                    , value = dimensions.width
                    }
                    False
                    |> Element.map (\newWidth -> { dimensions | width = newWidth })
                ]

        position =
            Element.column [ Element.width (Element.px 50), Element.spacing 10 ]
                [ viewInput
                    { label = "X"
                    , value = dimensions.x
                    }
                    False
                    |> Element.map (\newX -> { dimensions | x = newX })
                , viewInput
                    { label = "Y"
                    , value = dimensions.y
                    }
                    False
                    |> Element.map (\newY -> { dimensions | y = newY })
                ]

        controls =
            Element.row
                [ Element.spacing 30
                , Element.paddingEach { edges | top = 15 }
                , Element.width Element.fill
                ]
                [ widthAndHeight
                , position
                ]
    in
    Element.column
        Canvas.AttributesPanel.Shared.sectionStyles
        [ header, controls ]


{-| Allow the use to define the behavior of how an element is resizing fluidly
-}
viewLengthSelector : Spec.Model.WithSpec userModel -> Canvas.Selection.SelectionItem -> Spec.Element.Layout.Length -> Element.Element Spec.Element.Layout.Length
viewLengthSelector userModel selection length =
    let
        viewInputNullable =
            Ui.LinkableInput.viewNullable userModel selection

        viewInput =
            Ui.LinkableInput.view userModel selection

        { behavior, minMax } =
            length

        absoluteInputs =
            case behavior of
                Spec.Element.Layout.Static staticSize ->
                    viewInput { label = "Length", value = staticSize } True
                        |> Element.el [ Element.width Element.fill, Element.moveDown 2 ]
                        |> Element.map (\len -> { length | behavior = Spec.Element.Layout.Static len })

                _ ->
                    Element.row
                        [ Element.spacing 5, Element.width Element.fill ]
                        [ viewInputNullable { label = "Min", value = minMax.min }
                            |> Element.map (\min -> { length | minMax = { minMax | min = min } })
                        , viewInputNullable { label = "Max", value = minMax.max }
                            |> Element.map (\max -> { length | minMax = { minMax | max = max } })
                        ]
    in
    Element.row [ Element.width Element.fill, Element.spacing 5 ]
        [ behaviorSelection length
        , absoluteInputs
        ]


{-| Allow the user to choose between shrink, fill and static sizing
-}
behaviorSelection : Spec.Element.Layout.Length -> Element.Element Spec.Element.Layout.Length
behaviorSelection length =
    let
        -- BAKE OPTIONS
        { behavior, minMax } =
            length

        staticSelector =
            let
                onPress =
                    { length
                        | behavior = Spec.Element.Layout.Static staticLength
                    }

                ( isStatic, staticLength ) =
                    case behavior of
                        Spec.Element.Layout.Static staticSize ->
                            ( True, staticSize )

                        _ ->
                            ( False, minMax.min |> Spec.Element.Layout.Length.fromNullable )
            in
            Ui.Dropdown.viewRow
                { isSelected = isStatic
                , onSelect = onPress
                , label = Ui.Dropdown.Description "Static"
                , sideNote = Ui.Dropdown.NoDetail
                , detail = Ui.Dropdown.Description "The element will always be the same size"
                , rightHandText = Nothing
                }

        shrinkSelector =
            let
                onPress =
                    { length | behavior = Spec.Element.Layout.Shrink }
            in
            Ui.Dropdown.viewRow
                { isSelected = behavior == Spec.Element.Layout.Shrink
                , onSelect = onPress
                , label = Ui.Dropdown.Description "Shrink"
                , sideNote = Ui.Dropdown.NoDetail
                , detail = Ui.Dropdown.Description "The element will shrink to the size of its contents."
                , rightHandText = Nothing
                }

        fillSelector =
            let
                onPress =
                    { length | behavior = Spec.Element.Layout.Fill }
            in
            Ui.Dropdown.viewRow
                { isSelected = behavior == Spec.Element.Layout.Fill
                , onSelect = onPress
                , label = Ui.Dropdown.Description "Fill"
                , sideNote = Ui.Dropdown.NoDetail
                , detail = Ui.Dropdown.Description "The avaliable space will be split evenly between elements that are also ‘fill’"
                , rightHandText = Nothing
                }

        contents =
            [ shrinkSelector
            , fillSelector
            , staticSelector
            ]

        -- DETERMINE LABEL
        label =
            case behavior of
                Spec.Element.Layout.Fill ->
                    "Fill"

                Spec.Element.Layout.Shrink ->
                    "Shrink"

                Spec.Element.Layout.Static _ ->
                    "Static"
    in
    -- RENDER DROPDOWN
    Ui.Dropdown.view [] { label = label, contents = contents }
        |> Element.el [ Ui.Style.style "margin-left" "-10px" ]


flexibleLengthSelector label isActive =
    let
        layout =
            [ Element.padding 5
            , Element.width Element.fill
            , Element.Font.center
            , Element.Border.width 1
            , Element.Border.color Ui.Style.white
            ]

        attrs =
            layout
                ++ (if isActive then
                        [ Element.Font.color Ui.Style.white, Element.Background.color Ui.Style.highlightColorSolid ]

                    else
                        [ Element.Background.color Ui.Style.highlightColor ]
                   )

        selectButton =
            Element.Input.button
                attrs
                { onPress = Just ()
                , label = Element.text label
                }
    in
    Element.row [ Element.spacing 5, Element.width Element.fill ] [ selectButton ]
