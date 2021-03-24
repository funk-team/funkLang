module Canvas.Tool exposing (..)

{-| The tool module provides means to the user for interacting with things (screens, elements etc.) shown on the infinite canvas.

Each tool has its own lifecycle and only exposes to the outside world whether to commit a mutation or not.
Mutation in the sense of redux / graphql where the subject is the funk spec.

side note:

  - should canvas be called screen? "infinite-scene"
  - is screen on scene??? whaaat

-}

import Canvas.Camera.Model
import Canvas.Selection
import Canvas.Tool.AugmentationParams
import Canvas.Tool.Cut
import Canvas.Tool.Debug
import Canvas.Tool.Draw
import Canvas.Tool.Draw.Model
import Canvas.Tool.Model
import Canvas.Tool.Msg
import Canvas.Tool.Transform
import Canvas.Tool.Transform.Model
import Element
import Element.Background
import Element.Font
import Element.Input
import Model.Model
import Random
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Ui.Boxicons
import Ui.Component
import Ui.Style


active : Canvas.Tool.Model.ActiveState
active =
    { simpleElement = False
    , simpleTextInput = False
    , simpleButton = False
    , simpleText = False
    , transform = False
    , cut = False
    , debug = False
    , evolve = False
    }


{-| Render the dropdown toolbar for drawing mode tools. The Event emitted is a Tool which can
then be wrapped into the appropiate message like `ToolPicked Tool`
-}
drawingModeDropDown : Canvas.Tool.Model.DrawingDropDownPrams -> Element.Element Canvas.Tool.Model.Tool
drawingModeDropDown drawingDropDownPrams =
    let
        activePrams =
            checkIfToolIsActive drawingDropDownPrams.selectedTool

        buttonStyles onToolBar toolTip activePrams_ =
            [ Element.paddingXY 10 0
            , Element.height Element.fill
            , case onToolBar of
                True ->
                    Element.width Element.fill

                False ->
                    Element.width (Element.px 200)
            , Ui.Component.tooltip toolTip
            ]
                ++ markActive activePrams_

        drawBoxTool : Bool -> Element.Element Canvas.Tool.Model.Tool
        drawBoxTool onToolBar =
            let
                ( msg, icon ) =
                    let
                        justDraw =
                            ( Canvas.Tool.Model.Draw { drawState = Canvas.Tool.Draw.Model.NotDrawing, sticky = False, mode = Canvas.Tool.Draw.Model.Box }
                            , Ui.Boxicons.bxRectangle
                            )
                    in
                    case drawingDropDownPrams.selectedTool of
                        Canvas.Tool.Model.Draw d ->
                            case d.mode of
                                Canvas.Tool.Draw.Model.Box ->
                                    -- toggle sticky if already in draw mode
                                    ( Canvas.Tool.Model.Draw { d | sticky = not d.sticky }
                                    , if d.sticky then
                                        Ui.Boxicons.bxsRectangle

                                      else
                                        Ui.Boxicons.bxRectangle
                                    )

                                Canvas.Tool.Draw.Model.TextInput ->
                                    justDraw

                                Canvas.Tool.Draw.Model.Text ->
                                    justDraw

                                Canvas.Tool.Draw.Model.Button ->
                                    justDraw

                        _ ->
                            justDraw
            in
            Element.Input.button
                (buttonStyles onToolBar "Blank Element (E)" activePrams.simpleElement)
                { onPress = Just msg
                , label =
                    case onToolBar of
                        True ->
                            Element.row [ Element.centerY, Element.moveUp 1 ]
                                [ Ui.Component.icon icon ]

                        False ->
                            Element.row [ Element.centerY, Element.moveUp 1, Element.width Element.fill ]
                                [ Ui.Component.icon icon
                                , Element.el [ Element.moveRight 5 ] (Element.text "Element")
                                , Element.el [ Element.alignRight ] (Element.text "E")
                                ]
                }

        drawTextTool : Bool -> Element.Element Canvas.Tool.Model.Tool
        drawTextTool onToolBar =
            let
                ( msg, icon ) =
                    let
                        justDraw =
                            ( Canvas.Tool.Model.Draw { drawState = Canvas.Tool.Draw.Model.NotDrawing, sticky = False, mode = Canvas.Tool.Draw.Model.Text }
                            , Ui.Boxicons.bxFont
                            )
                    in
                    case drawingDropDownPrams.selectedTool of
                        Canvas.Tool.Model.Draw d ->
                            case d.mode of
                                Canvas.Tool.Draw.Model.Box ->
                                    justDraw

                                Canvas.Tool.Draw.Model.TextInput ->
                                    justDraw

                                Canvas.Tool.Draw.Model.Text ->
                                    -- toggle sticky if already in draw mode
                                    ( Canvas.Tool.Model.Draw { d | sticky = not d.sticky }
                                    , if d.sticky then
                                        Ui.Boxicons.bxFontColor

                                      else
                                        Ui.Boxicons.bxFont
                                    )

                                Canvas.Tool.Draw.Model.Button ->
                                    justDraw

                        _ ->
                            justDraw
            in
            Element.Input.button
                (buttonStyles onToolBar "Text (T)" activePrams.simpleText)
                { onPress = Just msg
                , label =
                    case onToolBar of
                        True ->
                            Element.row [ Element.centerY, Element.moveUp 1 ]
                                [ Ui.Component.icon icon ]

                        False ->
                            Element.row [ Element.centerY, Element.moveUp 1, Element.width Element.fill ]
                                [ Ui.Component.icon icon
                                , Element.el [ Element.moveRight 5 ] (Element.text "Text")
                                , Element.el [ Element.alignRight ] (Element.text "T")
                                ]
                }

        drawInputTool : Bool -> Element.Element Canvas.Tool.Model.Tool
        drawInputTool onToolBar =
            let
                ( msg, icon ) =
                    let
                        justDraw =
                            ( Canvas.Tool.Model.Draw { drawState = Canvas.Tool.Draw.Model.NotDrawing, sticky = False, mode = Canvas.Tool.Draw.Model.TextInput }
                            , Ui.Boxicons.bxPencil
                            )
                    in
                    case drawingDropDownPrams.selectedTool of
                        Canvas.Tool.Model.Draw d ->
                            case d.mode of
                                Canvas.Tool.Draw.Model.Box ->
                                    justDraw

                                Canvas.Tool.Draw.Model.TextInput ->
                                    -- toggle sticky if already in draw mode
                                    ( Canvas.Tool.Model.Draw { d | sticky = not d.sticky }
                                    , if d.sticky then
                                        Ui.Boxicons.bxsPencil

                                      else
                                        Ui.Boxicons.bxPencil
                                    )

                                Canvas.Tool.Draw.Model.Button ->
                                    justDraw

                                Canvas.Tool.Draw.Model.Text ->
                                    justDraw

                        _ ->
                            justDraw
            in
            Element.Input.button
                (buttonStyles onToolBar "Text Input (I)" activePrams.simpleTextInput)
                { onPress = Just msg
                , label =
                    case onToolBar of
                        True ->
                            Element.row [ Element.centerY, Element.moveUp 1 ]
                                [ Ui.Component.icon icon ]

                        False ->
                            Element.row [ Element.centerY, Element.moveUp 1, Element.width Element.fill ]
                                [ Ui.Component.icon icon
                                , Element.el [ Element.moveRight 5 ] (Element.text "Text Input")
                                , Element.el [ Element.alignRight ] (Element.text "I")
                                ]
                }

        cutTool : Bool -> Element.Element Canvas.Tool.Model.Tool
        cutTool onToolBar =
            Element.Input.button
                ([ Element.paddingXY 10 0
                 , Element.width Element.fill
                 , Element.height Element.fill
                 , Ui.Component.tooltip "Splice Element (S)"
                 ]
                    ++ markActive activePrams.cut
                    ++ highlightOnHover activePrams.cut
                )
                { onPress = Just (Canvas.Tool.Model.Cut Canvas.Tool.Cut.NotHovering)
                , label =
                    case onToolBar of
                        True ->
                            Element.row [ Element.centerY, Element.moveUp 1 ]
                                [ Ui.Component.icon Ui.Boxicons.bxCut ]

                        False ->
                            Element.row [ Element.centerY, Element.moveUp 1, Element.width Element.fill ]
                                [ Ui.Component.icon Ui.Boxicons.bxCut
                                , Element.el [ Element.moveRight 5 ] (Element.text "Cut")
                                , Element.el [ Element.alignRight ] (Element.text "S")
                                ]
                }

        drawButtonTool : Bool -> Element.Element Canvas.Tool.Model.Tool
        drawButtonTool onToolBar =
            let
                ( msg, icon ) =
                    let
                        justDraw =
                            ( Canvas.Tool.Model.Draw { drawState = Canvas.Tool.Draw.Model.NotDrawing, sticky = False, mode = Canvas.Tool.Draw.Model.Button }
                            , Ui.Boxicons.bxMouse
                            )
                    in
                    case drawingDropDownPrams.selectedTool of
                        Canvas.Tool.Model.Draw d ->
                            case d.mode of
                                Canvas.Tool.Draw.Model.Box ->
                                    justDraw

                                Canvas.Tool.Draw.Model.TextInput ->
                                    justDraw

                                Canvas.Tool.Draw.Model.Text ->
                                    justDraw

                                Canvas.Tool.Draw.Model.Button ->
                                    ( Canvas.Tool.Model.Draw { d | sticky = not d.sticky }
                                    , if d.sticky then
                                        Ui.Boxicons.bxsMouse

                                      else
                                        Ui.Boxicons.bxMouse
                                    )

                        _ ->
                            justDraw
            in
            Element.Input.button
                (buttonStyles onToolBar "Draw Button (B)" activePrams.simpleButton)
                { onPress = Just msg
                , label =
                    case onToolBar of
                        True ->
                            Element.row [ Element.centerY, Element.moveUp 1 ]
                                [ Ui.Component.icon icon ]

                        False ->
                            Element.row [ Element.centerY, Element.moveUp 1, Element.width Element.fill ]
                                [ Ui.Component.icon icon
                                , Element.el [ Element.moveRight 5 ] (Element.text "Button")
                                , Element.el [ Element.alignRight ] (Element.text "B")
                                ]
                }

        -- highlight the currently selected tool
        markActive isActive =
            if isActive then
                [ Element.Background.color Ui.Style.highlightColorSolid, Element.Font.color Ui.Style.white ]

            else
                []

        highlightOnHover isActive =
            if isActive then
                []

            else
                [ Element.mouseOver [ Element.Background.color Ui.Style.highlightColorLow ] ]

        renderToolInDropDown tool_ =
            Element.row [ Element.width (Element.px 200), Element.height (Element.px 40) ] [ tool_ ]

        dropDown =
            let
                dropDownstyles =
                    [ Element.width (Element.px 200)
                    , Element.height Element.fill
                    , Element.mouseOver [ Element.Background.color Ui.Style.highlightColorLow ]
                    ]
            in
            Element.column
                [ Element.Background.color Ui.Style.black
                , Element.moveDown 5
                , Element.width (Element.px 200)
                , Element.moveLeft 25
                , Element.height Element.shrink
                ]
                (List.map renderToolInDropDown
                    [ Element.row dropDownstyles
                        [ drawBoxTool False, viewSticky Box drawingDropDownPrams.selectedTool ]
                    , Element.row dropDownstyles
                        [ drawButtonTool False, viewSticky Button drawingDropDownPrams.selectedTool ]
                    , Element.row dropDownstyles
                        [ drawInputTool False, viewSticky TextInput drawingDropDownPrams.selectedTool ]
                    , Element.row dropDownstyles
                        [ drawTextTool False, viewSticky Text drawingDropDownPrams.selectedTool ]

                    -- , cutTool False
                    ]
                )

        previousTool_ =
            case drawingDropDownPrams.previousTool of
                Canvas.Tool.Model.Draw { mode } ->
                    case mode of
                        Canvas.Tool.Draw.Model.Box ->
                            drawBoxTool True

                        Canvas.Tool.Draw.Model.TextInput ->
                            drawInputTool True

                        Canvas.Tool.Draw.Model.Text ->
                            drawTextTool True

                        Canvas.Tool.Draw.Model.Button ->
                            drawButtonTool True

                Canvas.Tool.Model.Cut _ ->
                    cutTool True

                _ ->
                    drawBoxTool True

        activeTool_ =
            case drawingDropDownPrams.selectedTool of
                Canvas.Tool.Model.Draw { mode } ->
                    case mode of
                        Canvas.Tool.Draw.Model.Box ->
                            drawBoxTool True

                        Canvas.Tool.Draw.Model.TextInput ->
                            drawInputTool True

                        Canvas.Tool.Draw.Model.Text ->
                            drawTextTool True

                        Canvas.Tool.Draw.Model.Button ->
                            drawButtonTool True

                Canvas.Tool.Model.Cut _ ->
                    cutTool True

                _ ->
                    previousTool_

        viewActiveTool =
            Element.el [ Element.width (Element.px 75), Element.height (Element.px 40) ] activeTool_
    in
    case drawingDropDownPrams.dropDownOpen of
        True ->
            Element.column [ Element.width (Element.px 75), Element.height Element.fill ]
                [ viewActiveTool
                , dropDown
                ]

        False ->
            viewActiveTool


type DrawingOption
    = Box
    | TextInput
    | Button
    | Text


viewSticky :
    DrawingOption
    -> Canvas.Tool.Model.Tool
    -> Element.Element Canvas.Tool.Model.Tool
viewSticky drawingOption selectedTool =
    let
        ( msg_, icon ) =
            let
                justDraw =
                    ( Canvas.Tool.Model.Draw
                        { drawState = Canvas.Tool.Draw.Model.NotDrawing
                        , sticky = True
                        , mode =
                            case drawingOption of
                                Box ->
                                    Canvas.Tool.Draw.Model.Box

                                TextInput ->
                                    Canvas.Tool.Draw.Model.TextInput

                                Text ->
                                    Canvas.Tool.Draw.Model.Text

                                Button ->
                                    Canvas.Tool.Draw.Model.Button
                        }
                    , Ui.Boxicons.bxPin
                    )

                justSticky d =
                    ( Canvas.Tool.Model.Draw { d | sticky = not d.sticky }
                    , if d.sticky then
                        Ui.Boxicons.bxsPin

                      else
                        Ui.Boxicons.bxPin
                    )
            in
            -- Get the correct mode icon, sticky icon and message for each drawing mode
            case selectedTool of
                Canvas.Tool.Model.Draw d ->
                    case drawingOption of
                        Box ->
                            case d.mode of
                                Canvas.Tool.Draw.Model.Box ->
                                    justSticky d

                                Canvas.Tool.Draw.Model.TextInput ->
                                    justDraw

                                Canvas.Tool.Draw.Model.Text ->
                                    justDraw

                                Canvas.Tool.Draw.Model.Button ->
                                    justDraw

                        TextInput ->
                            case d.mode of
                                Canvas.Tool.Draw.Model.Box ->
                                    justDraw

                                Canvas.Tool.Draw.Model.TextInput ->
                                    justSticky d

                                Canvas.Tool.Draw.Model.Text ->
                                    justDraw

                                Canvas.Tool.Draw.Model.Button ->
                                    justDraw

                        Text ->
                            case d.mode of
                                Canvas.Tool.Draw.Model.Box ->
                                    justDraw

                                Canvas.Tool.Draw.Model.TextInput ->
                                    justDraw

                                Canvas.Tool.Draw.Model.Text ->
                                    justSticky d

                                Canvas.Tool.Draw.Model.Button ->
                                    justDraw

                        Button ->
                            case d.mode of
                                Canvas.Tool.Draw.Model.Box ->
                                    justDraw

                                Canvas.Tool.Draw.Model.TextInput ->
                                    justDraw

                                Canvas.Tool.Draw.Model.Text ->
                                    justDraw

                                Canvas.Tool.Draw.Model.Button ->
                                    justSticky d

                _ ->
                    justDraw
    in
    Element.Input.button
        [ Element.paddingEach { top = 0, left = 5, bottom = 0, right = 0 }
        , Element.height Element.fill
        , Element.width Element.shrink
        , Element.scale 0.7
        , Ui.Component.tooltip
            (case drawingOption of
                Box ->
                    "Element Sticky (Shift+E)"

                TextInput ->
                    "Text Input Sticky (Shift+I)"

                Button ->
                    "Button Sticky (Shift+B)"

                Text ->
                    "Text Sticky (Shift+T)"
            )
        , Element.moveLeft 50
        ]
        { onPress = Just msg_
        , label =
            Element.row [ Element.centerY, Element.width Element.fill, Element.moveLeft 2 ]
                [ Ui.Component.icon icon
                ]
        }


{-| Render the select mode toolbar. The Event emitted is a Tool which can
then be wrapped into the appropiate message like `ToolPicked Tool`
-}
selectModetoolbar : Canvas.Tool.Model.Tool -> Element.Element Canvas.Tool.Model.Tool
selectModetoolbar tool =
    let
        activePrams =
            checkIfToolIsActive tool

        -- highlight the currently selected tool
        markActive isActive =
            if isActive then
                [ Element.Background.color Ui.Style.highlightColorSolid, Element.Font.color Ui.Style.white ]

            else
                []
    in
    Element.row [ Element.height Element.fill ]
        [ Element.Input.button
            ([ Element.paddingXY 10 0
             , Element.height Element.fill
             , Element.moveLeft 30
             , Ui.Component.tooltip "Select & Transform (V)"
             ]
                ++ markActive activePrams.transform
            )
            { onPress =
                case activePrams.transform of
                    False ->
                        Just (Canvas.Tool.Model.Transform Canvas.Tool.Transform.Model.NotTransforming)

                    True ->
                        Nothing
            , label = Ui.Component.icon Ui.Boxicons.bxsPointer |> Element.el [ Element.centerY, Element.moveUp 1 ]
            }
        ]


checkIfToolIsActive : Canvas.Tool.Model.Tool -> Canvas.Tool.Model.ActiveState
checkIfToolIsActive selectedTool =
    let
        activeTools =
            case selectedTool of
                Canvas.Tool.Model.Draw { mode } ->
                    case mode of
                        Canvas.Tool.Draw.Model.Box ->
                            { active | simpleElement = True }

                        Canvas.Tool.Draw.Model.TextInput ->
                            { active | simpleTextInput = True }

                        Canvas.Tool.Draw.Model.Text ->
                            { active | simpleText = True }

                        Canvas.Tool.Draw.Model.Button ->
                            { active | simpleButton = True }

                Canvas.Tool.Model.Cut _ ->
                    { active | cut = True }

                Canvas.Tool.Model.Transform _ ->
                    { active | transform = True }

                Canvas.Tool.Model.Debug _ ->
                    { active | debug = True }
    in
    activeTools


{-| Render additional attributes for the scene depending on which tool is currently isActive.

It should only return event listeners, nothing else.
It only returns attributes because

  - Events are attributes by default
  - Elm-Ui Elements that are positioned absolute (or onLeft etc.) are attributes too.

-}
augmentScene :
    Canvas.Camera.Model.Model
    -> Canvas.Tool.Model.Tool
    -> List (Element.Attribute Canvas.Tool.Msg.Msg)
augmentScene camera tool =
    (::) Spec.Element.Id.htmlRootId <|
        case tool of
            Canvas.Tool.Model.Cut _ ->
                Canvas.Tool.Cut.augmentScene
                    |> List.map (Element.mapAttribute Canvas.Tool.Msg.CutMsg)

            Canvas.Tool.Model.Draw state ->
                Canvas.Tool.Draw.augmentScene camera state
                    |> List.map (Element.mapAttribute Canvas.Tool.Msg.DrawMsg)

            Canvas.Tool.Model.Transform state ->
                Canvas.Tool.Transform.augmentScene camera state
                    |> List.map (Element.mapAttribute Canvas.Tool.Msg.TransformMsg)

            Canvas.Tool.Model.Debug state ->
                Canvas.Tool.Debug.augmentScene camera state
                    |> List.map (Element.mapAttribute Canvas.Tool.Msg.DebugMsg)


augmentCanvas :
    Canvas.Camera.Model.Model
    -> Canvas.Tool.Model.Tool
    -> List (Element.Attribute Canvas.Tool.Msg.Msg)
augmentCanvas camera tool =
    (::) Spec.Element.Id.htmlRootId <|
        case tool of
            Canvas.Tool.Model.Draw state ->
                Canvas.Tool.Draw.augmentCanvas camera state
                    |> List.map (Element.mapAttribute Canvas.Tool.Msg.DrawMsg)

            Canvas.Tool.Model.Debug state ->
                Canvas.Tool.Debug.augmentCanvas camera state
                    |> List.map (Element.mapAttribute Canvas.Tool.Msg.DebugMsg)

            _ ->
                []


{-| Additional event listeners for an element on the Canvas and NOT the Canvas itself
-}
augmentElement :
    Model.Model.UserModel
    -> Maybe Spec.Element.Model.EitherElement
    -> Canvas.Tool.AugmentationParams.AugmentationParams
    -> Canvas.Tool.Model.Tool
    -> Canvas.Tool.Model.OverridesOrDecoration
augmentElement userModel parent augmentationParams tool =
    case tool of
        Canvas.Tool.Model.Cut state ->
            let
                other =
                    Canvas.Tool.Cut.augment userModel augmentationParams state
                        |> List.map (Element.mapAttribute Canvas.Tool.Msg.CutMsg)
            in
            { other = other
            , layout = []
            , element = augmentationParams.element
            }

        Canvas.Tool.Model.Draw state ->
            Canvas.Tool.Draw.augmentElement
                augmentationParams
                state
                |> (\other ->
                        { other = List.map (Element.mapAttribute Canvas.Tool.Msg.DrawMsg) other
                        , layout = []
                        , element = augmentationParams.element
                        }
                   )

        Canvas.Tool.Model.Transform state ->
            Canvas.Tool.Transform.augmentEitherElement
                userModel
                parent
                augmentationParams
                state
                |> (\{ layout, other, element } ->
                        { other = List.map (Element.mapAttribute Canvas.Tool.Msg.TransformMsg) other
                        , layout = List.map (Element.mapAttribute Canvas.Tool.Msg.TransformMsg) layout
                        , element = element
                        }
                   )

        Canvas.Tool.Model.Debug state ->
            Canvas.Tool.Debug.augmentElement
                augmentationParams
                state
                |> (\other ->
                        { other = List.map (Element.mapAttribute Canvas.Tool.Msg.DebugMsg) other
                        , layout = []
                        , element = augmentationParams.element
                        }
                   )


update :
    Model.Model.Model
    -> Model.Model.UserModel
    -> Random.Seed
    -> Canvas.Camera.Model.Model
    -> Canvas.Selection.Selection
    -> Canvas.Tool.Msg.Msg
    -> Canvas.Tool.Model.Tool
    -> Canvas.Tool.Model.Return
update model userModel seed camera selection msg tool =
    case ( msg, tool ) of
        ( Canvas.Tool.Msg.CutMsg cutMsg, Canvas.Tool.Model.Cut cutState ) ->
            let
                ( state, mutation, newSeed ) =
                    Canvas.Tool.Cut.update seed cutMsg cutState
            in
            Canvas.Tool.Model.Return (Canvas.Tool.Model.Cut state) mutation newSeed selection

        ( Canvas.Tool.Msg.DrawMsg drawMsg, Canvas.Tool.Model.Draw drawState ) ->
            let
                return =
                    Canvas.Tool.Draw.update seed camera drawMsg drawState
            in
            Canvas.Tool.Model.Return (Canvas.Tool.Model.Draw return.state)
                return.mutation
                return.seed
                (case return.selection of
                    Just _ ->
                        return.selection

                    Nothing ->
                        selection
                )

        ( Canvas.Tool.Msg.TransformMsg transformMsg, Canvas.Tool.Model.Transform transformState ) ->
            let
                ( state, mutation, newSelection ) =
                    Canvas.Tool.Transform.update
                        model.pressedKeys
                        userModel
                        camera
                        selection
                        transformMsg
                        transformState
            in
            Canvas.Tool.Model.Return (Canvas.Tool.Model.Transform state) mutation seed newSelection

        ( Canvas.Tool.Msg.DebugMsg state, Canvas.Tool.Model.Debug transformState ) ->
            Canvas.Tool.Model.Return (Canvas.Tool.Model.Debug state) Nothing seed selection

        _ ->
            Canvas.Tool.Model.Return tool Nothing seed selection
