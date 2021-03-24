module Canvas.AttributesPanel.Shared exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import EventsExtra
import Html.Attributes
import Spec.Element.Style.Edges
import Ui.Input
import Ui.Style


{-| Each section presents its contents nicely and with a separator
-}
sectionStyles : List (Element.Attribute msg)
sectionStyles =
    [ Element.padding 15
    , Element.Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
    , Element.Border.color Ui.Style.lightGrey
    , Element.width Element.fill
    ]


edges =
    { bottom = 0, left = 0, right = 0, top = 0 }


{-| Provide "top/bottom/left/right" controls
-}
edgesEditor :
    Spec.Element.Style.Edges.EdgeDimensions
    -> Spec.Element.Style.Edges.ConcreteEdgeDimensions
    -> Element.Element Spec.Element.Style.Edges.EdgeDimensions
edgesEditor dimensions derived =
    let
        { top, bottom, left, right } =
            dimensions
    in
    Element.row
        [ Element.spacing 10
        , Element.paddingEach { edges | top = 15 }
        ]
        [ Ui.Input.smartInt "Top" Element.Input.labelAbove top derived.top |> Element.map (\topVal -> { dimensions | top = topVal })
        , Ui.Input.smartInt "Bottom" Element.Input.labelAbove bottom derived.bottom |> Element.map (\bottomVal -> { dimensions | bottom = bottomVal })
        , Ui.Input.smartInt "Left" Element.Input.labelAbove left derived.left |> Element.map (\leftVal -> { dimensions | left = leftVal })
        , Ui.Input.smartInt "Right" Element.Input.labelAbove right derived.right |> Element.map (\rightVal -> { dimensions | right = rightVal })
        ]


type PlusOrMinus
    = Plus
    | Minus


{-| Show plus when nothing is set and a minus when something is set.

Pass a default value to define what gets emitted when clicking '+'

-}
viewPlusOrMinusToggleButton_ : { default : a, current : Maybe a } -> Element.Element (Maybe a)
viewPlusOrMinusToggleButton_ { default, current } =
    case current of
        Nothing ->
            viewHeaderButton "+" (Just default)

        Just _ ->
            viewHeaderButton "-" Nothing


{-| Like above with slightly different API Design. TODO: Remove this version
-}
viewPlusOrMinusToggleButton : PlusOrMinus -> Element.Element PlusOrMinus
viewPlusOrMinusToggleButton plusOrMinus =
    case plusOrMinus of
        Plus ->
            viewHeaderButton "+" Plus

        Minus ->
            viewHeaderButton "-" Minus


trueIsPlus : Bool -> PlusOrMinus
trueIsPlus bool =
    if bool then
        Plus

    else
        Minus


trueIsMinus : Bool -> PlusOrMinus
trueIsMinus bool =
    if bool then
        Minus

    else
        Plus


viewHeaderButton =
    viewHeaderButtonHelp ""


viewHeaderButtonHelp : String -> String -> msg -> Element.Element msg
viewHeaderButtonHelp ariaLabel_ label action =
    let
        ariaLabel =
            case ariaLabel_ of
                "" ->
                    label

                _ ->
                    ariaLabel_
    in
    Element.el
        [ Element.alignRight
        , Element.htmlAttribute (Html.Attributes.attribute "aria-label" ariaLabel)
        , EventsExtra.onClickStopPropagation action
        , Element.mouseOver
            [ Element.Background.color Ui.Style.slightAccent
            ]
        , Element.width (Element.px 17)
        , Element.height (Element.px 17)
        , Element.Border.rounded 2
        ]
        (Element.el [ Element.centerX, Element.centerY ] <| Element.text label)


viewTabButton : String -> msg -> Bool -> Element.Element msg
viewTabButton label action isActive =
    Element.el
        [ EventsExtra.onClickStopPropagation action
        , Element.width Element.shrink
        , Element.height Element.shrink
        , Element.Border.rounded 2
        , case isActive of
            True ->
                Element.Font.bold

            False ->
                Element.Font.regular
        ]
        (Element.text label)
