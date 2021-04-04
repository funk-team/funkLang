module Spec.Element.Controls.Layout.Context exposing (..)

import Canvas.AttributesPanel.Shared
import Element
import Element.Border
import Element.Font
import Element.Input
import Renderer.Layout
import Spec.Element.Layout
import Spec.Element.Layout.Padding
import Ui.Dropdown
import Ui.Input
import Ui.RadioRow
import Ui.Style


edges =
    Ui.Style.edges


viewLayoutSelector :
    Spec.Element.Layout.Flow
    -> Element.Element Spec.Element.Layout.Flow
viewLayoutSelector l =
    Ui.RadioRow.view
        { toLabel = Spec.Element.Layout.flowToLabel
        , selected = (==) l
        , items = Spec.Element.Layout.flows
        }


{-| Provide controls for padding
This component has different modes for better user comprehension
-}
paddingControls :
    Spec.Element.Layout.Padding.Padding
    -> Element.Element msg --  later: Spec.Element.Layout.Padding
paddingControls rawPadding =
    let
        { top, bottom, left, right } =
            Renderer.Layout.formatPadding rawPadding

        seperator =
            Element.el
                [ Element.Border.widthEach { bottom = 0, left = 0, right = 1, top = 2 }
                , Element.height Element.fill
                ]
                Element.none

        columnTextSpacing =
            Element.spacing 5
    in
    Element.row
        [ Element.spacing 5 ]
        [ Element.column [ columnTextSpacing ] [ Element.text "Top", Element.text (String.fromInt top) ]
        , seperator
        , Element.column [ columnTextSpacing ] [ Element.text "Bottom", Element.text (String.fromInt bottom) ]
        , seperator
        , Element.column [ columnTextSpacing ] [ Element.text "Left", Element.text (String.fromInt left) ]
        , seperator
        , Element.column [ columnTextSpacing ] [ Element.text "Right", Element.text (String.fromInt right) ]
        ]


paddingSelector : Spec.Element.Layout.Padding.Padding -> Element.Element Spec.Element.Layout.Padding.Padding
paddingSelector padding =
    let
        header : Element.Element Spec.Element.Layout.Padding.Padding
        header =
            Element.row
                [ Element.spacing 5, Element.paddingEach { edges | bottom = 5 } ]
                [ Element.el [ Element.Font.bold ] <| Element.text "Padding"
                , modeSelection
                ]

        -- explain to the user what padding is
        explanation =
            Element.paragraph [ Element.Font.color Ui.Style.grey ]
                [ Element.text "Padding is the distance between an elements outer edge and it’s content."
                ]

        -- render a mode in the dropdown
        renderModeRow : Spec.Element.Layout.Padding.Padding -> Ui.Dropdown.Row Spec.Element.Layout.Padding.Padding
        renderModeRow mode =
            Ui.Dropdown.viewRow
                { label = Ui.Dropdown.Description <| Spec.Element.Layout.Padding.paddingLabel mode
                , rightHandText = Nothing
                , detail = Ui.Dropdown.Description <| Spec.Element.Layout.Padding.paddingDescription mode
                , onSelect = mode
                , sideNote = Ui.Dropdown.NoDetail
                , isSelected = Spec.Element.Layout.Padding.paddingLabel mode == Spec.Element.Layout.Padding.paddingLabel padding
                }

        -- allow the user to switch between different padding modes
        modeSelection : Element.Element Spec.Element.Layout.Padding.Padding
        modeSelection =
            Ui.Dropdown.view
                []
                { contents =
                    Spec.Element.Layout.Padding.paddingModes padding
                        |> List.map renderModeRow
                , label = Spec.Element.Layout.Padding.paddingLabel padding
                }

        -- we u
        derived =
            Spec.Element.Layout.Padding.deriveEachPadding padding

        -- every padding mode has different controls
        controls =
            case padding of
                Spec.Element.Layout.Padding.PaddingEach dimensions ->
                    Canvas.AttributesPanel.Shared.edgesEditor
                        dimensions
                        derived
                        |> Element.map Spec.Element.Layout.Padding.PaddingEach

                Spec.Element.Layout.Padding.PaddingXY x y ->
                    Element.row [ Element.spacing 10, Element.paddingEach { edges | top = 15 } ]
                        [ Ui.Input.smartInt "Y" Element.Input.labelAbove y (Maybe.withDefault 0 y) |> Element.map (\y_ -> Spec.Element.Layout.Padding.PaddingXY x y_)
                        , Ui.Input.smartInt "X" Element.Input.labelAbove x (Maybe.withDefault 0 x) |> Element.map (\x_ -> Spec.Element.Layout.Padding.PaddingXY x_ y)
                        ]

                Spec.Element.Layout.Padding.EqualPadding size ->
                    Element.row [ Element.spacing 10, Element.paddingEach { edges | top = 15 } ]
                        [ Ui.Input.smartInt "All Edges" Element.Input.labelAbove size (Maybe.withDefault 0 size) |> Element.map Spec.Element.Layout.Padding.EqualPadding
                        ]
    in
    Element.column
        Canvas.AttributesPanel.Shared.sectionStyles
        [ header
        , explanation
        , controls
        ]


spacingControls shared =
    let
        heading =
            Element.el
                [ Element.Font.bold ]
                (Element.text "Spacing")

        explainer =
            Element.paragraph
                [ Element.Font.color Ui.Style.grey ]
                [ Element.text "Spacing is the distance between the children of an element." ]

        input_ =
            Ui.Input.smartInt "spacing" Element.Input.labelAbove shared.spacing (Maybe.withDefault 0 shared.spacing)
    in
    Element.column (Element.spacing 15 :: Canvas.AttributesPanel.Shared.sectionStyles)
        [ heading
        , explainer
        , input_
        ]



-- ELEMENT-SPECIFIC CONTROLS


arrangementSelector flow =
    Element.column
        (Element.spacing 15 :: Canvas.AttributesPanel.Shared.sectionStyles)
        [ Element.el [ Element.Font.bold ] <| Element.text "Arrangement"
        , Element.paragraph
            [ Element.Font.color Ui.Style.grey ]
            [ Element.text "When an element has contents or a child, we need to be specific about how they will be arranged." ]
        , viewLayoutSelector flow
        , Element.paragraph
            [ Element.Font.color Ui.Style.grey ]
            [ Element.text "Info: Grid layout coming soon. This will allow you to create collections of evenly sized elements that break into new lines." ]
        ]
