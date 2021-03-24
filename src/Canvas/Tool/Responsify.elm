module Canvas.Tool.Responsify exposing (..)

-- only imported for some types

import Canvas.Camera.Convert
import Canvas.Camera.Model
import Canvas.Events
import Canvas.Selection
import Canvas.Tool.AugmentationParams
import Canvas.Tool.Draw.Help
import Canvas.Tool.Responsify.Engine exposing (runResponsifyEngine)
import Canvas.Tool.Responsify.Model
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Events.Extra
import Element.Font
import File.Download as Download
import Html.Attributes
import Http
import Interface.Model
import Json.Decode as Decode
import Json.Encode as Encode
import Model.Model
import Random
import Rectangle
import Renderer.InteractivityAttribs
import Renderer.Layout
import Renderer.StyleCompositor
import Sha256 exposing (sha256)
import Spec
import Spec.Element
import Spec.Element.Controls.Layout
import Spec.Element.Controls.Layout.Context
import Spec.Element.Id
import Spec.Element.Layout
import Spec.Element.Layout.Padding exposing (Padding_(..))
import Spec.Element.Model
import Spec.Model
import Spec.Mutation
import Ui.Component
import Ui.Help
import Ui.Style
import Url


viewContextControls : Spec.Element.Model.EitherElement -> Element.Element Msg
viewContextControls element =
    let
        shared =
            element.shared

        setPadding =
            Element.map
                (\padding -> UpdatePrediction { element | shared = { shared | padding = padding } })

        setFlow =
            Element.map
                (\flow -> UpdatePrediction { element | shared = { shared | flow = flow } })

        setSpacing =
            Element.map
                (\spacing -> UpdatePrediction { element | shared = { shared | spacing = spacing } })
    in
    case element.shared.children of
        Spec.Element.Model.FlowChildren manyChildren ->
            Element.column []
                [ Spec.Element.Controls.Layout.Context.paddingSelector shared.padding |> setPadding
                , Spec.Element.Controls.Layout.Context.arrangementSelector shared.flow |> setFlow
                , Spec.Element.Controls.Layout.Context.spacingControls shared
                    |> setSpacing
                ]

        _ ->
            Element.none


init =
    Canvas.Tool.Responsify.Model.Closed


{-| Event handlers and drawing preview for new screen
-}
augmentScene : Canvas.Camera.Model.Model -> Canvas.Tool.Responsify.Model.State -> List (Element.Attribute Msg)
augmentScene camera state =
    [ Decode.succeed
        ScenePress
        |> Canvas.Events.onPress
    ]


detectEnvironment url =
    case
        Url.toString url
            |> (\s ->
                    ( String.contains "localhost" s
                    , False
                    )
               )
    of
        ( True, _ ) ->
            Dev

        ( _, True ) ->
            Preview

        _ ->
            Prod


renderPanel :
    Environment
    -> Model.Model.UserModel
    -> Canvas.Tool.AugmentationParams.AugmentationParams
    -> Canvas.Tool.Responsify.Model.State
    -> Element.Element Msg
renderPanel environment userModel augmentationParams state =
    (makeAugmentations environment userModel augmentationParams state).panel


responsifyPanel :
    Canvas.Tool.AugmentationParams.AugmentationParams
    -> Canvas.Tool.Responsify.Model.State
    -> Element.Element Msg
responsifyPanel { camera, selection, element, selectionItem } state =
    let
        ( hasNoAbsoluteChildren, hasNoFlowChildren ) =
            case element.shared.children of
                Spec.Element.Model.FlowChildren _ ->
                    ( True, False )

                Spec.Element.Model.AbsoluteChildren [] ->
                    ( True, True )

                Spec.Element.Model.AbsoluteChildren _ ->
                    ( False, True )

        noActionAvailable =
            Element.column [ Element.spacing 15 ]
                [ Element.paragraph [ Element.Font.color Ui.Style.grey ] [ Element.text "Nothing to Responsify. Draw an element inside your selection to start working with responsify." ]
                , Element.el (Ui.Help.noPointerEvents :: Element.alpha 0.25 :: styleButton) (Element.text "Responsify")
                ]

        responsifyAvailable =
            Element.column [ Element.spacing 15 ]
                [ Element.paragraph [ Element.Font.color Ui.Style.grey ] [ Element.text "An element can be made responsive when it contains other elements or content." ]
                , responsifyButton
                ]

        responsifyButton =
            Element.el [ forElement element, Element.Events.Extra.onMouseUp event ] <|
                Element.el
                    (forElement element :: styleButton)
                    (Element.text <| "Responsify")

        event : Decode.Decoder Msg
        event =
            readLayoutData camera element.shared.id
                |> Decode.map
                    (SmartLayoutButtonClicked ( selectionItem, element ))
    in
    if hasNoAbsoluteChildren then
        noActionAvailable

    else if hasNoFlowChildren then
        responsifyAvailable

    else
        Element.text "Can not mix flow and absolute elements. Coming soon."


forElement element =
    Element.htmlAttribute (Html.Attributes.attribute "for" (Spec.Element.Id.toHtmlIdRaw element.shared.id))


styleButton =
    Ui.Component.buttonStyle


{-| Display guides etc. if the user is currently drawing in this element
-}
augmentElement :
    Environment
    -> Model.Model.UserModel
    -> Canvas.Tool.AugmentationParams.AugmentationParams
    -> Canvas.Tool.Responsify.Model.State
    -> List (Element.Attribute Msg)
augmentElement environment userModel augmentationParams state =
    if not (Canvas.Selection.isSelectedElementId augmentationParams.selection augmentationParams.element.shared.id) then
        [ Canvas.Events.onPress
            (Decode.succeed (Select augmentationParams.selectionItem))
        ]

    else
        (makeAugmentations environment userModel augmentationParams state).hud


viewPanelContents :
    Canvas.Tool.AugmentationParams.AugmentationParams
    -> Canvas.Tool.Responsify.Model.State
    -> Element.Element Msg
viewPanelContents augmentationParams state =
    let
        { camera, selection, element, selectionItem } =
            augmentationParams
    in
    Element.column [] []


type Environment
    = Dev
    | Preview
    | Prod


type alias Augmentations =
    { hud : List (Element.Attribute Msg)
    , panel : Element.Element Msg
    }


makeAugmentations :
    Environment
    -> Model.Model.UserModel
    -> Canvas.Tool.AugmentationParams.AugmentationParams
    -> Canvas.Tool.Responsify.Model.State
    -> Augmentations
makeAugmentations environment userModel augmentationParams state =
    case state of
        Canvas.Tool.Responsify.Model.LayingOut { predictionParams, targetSelectionItem, layoutData, prediction, element } ->
            if augmentationParams.element.shared.id == Canvas.Selection.getTargetId targetSelectionItem then
                case prediction of
                    Nothing ->
                        { hud = []
                        , panel = Element.text "Could not predict layout"
                        }

                    Just prediction_ ->
                        let
                            hud : List (Element.Attribute Msg)
                            hud =
                                previewPrediction userModel augmentationParams.scope prediction_

                            isActualTargetOfResponsifyProcess =
                                augmentationParams.element.shared.id == Tuple.first layoutData.parent

                            panel =
                                if isActualTargetOfResponsifyProcess then
                                    layoutPanel
                                        environment
                                        userModel
                                        augmentationParams
                                        prediction_
                                        predictionParams
                                        element

                                else
                                    Element.none
                        in
                        { panel = panel, hud = hud }

            else
                { hud = [], panel = responsifyPanel augmentationParams state }

        Canvas.Tool.Responsify.Model.Closed ->
            { hud = [], panel = responsifyPanel augmentationParams state }



-- clusters found, draw box around them
-- PREVIEW FUNCTIONS
-- display both size and alignment


allControls :
    Model.Model.UserModel
    -> Canvas.Selection.SelectionItem
    -> Spec.Element.Model.EitherElement
    -> Element.Element Msg
allControls userModel selectionItem eitherElement =
    let
        shared =
            eitherElement.shared

        sizeAndAlignmenControlsOfAllChildren : Element.Element (List Spec.Element.Model.FlowElement)
        sizeAndAlignmenControlsOfAllChildren =
            case eitherElement.shared.children of
                Spec.Element.Model.FlowChildren children ->
                    Spec.Element.Controls.Layout.viewSizeAndAlignmentControlsForMultipleFlowElements
                        userModel
                        selectionItem
                        children

                _ ->
                    Element.none

        -- THIS WILL NEVER HAPPEN BECAUSE WE ARE INSIDE A PREDICTION
        sizeAndAlignmenControlsOfAllChildrenInEither : Element.Element Msg
        sizeAndAlignmenControlsOfAllChildrenInEither =
            sizeAndAlignmenControlsOfAllChildren
                |> Element.map (\x -> UpdatePrediction { eitherElement | shared = { shared | children = Spec.Element.Model.FlowChildren x } })
    in
    Element.column [] [ viewContextControls eitherElement, sizeAndAlignmenControlsOfAllChildrenInEither ]


edges =
    { bottom = 0, left = 0, right = 0, top = 0 }


layoutPanel :
    Environment
    -> Model.Model.UserModel
    -> Canvas.Tool.AugmentationParams.AugmentationParams
    -> Spec.Element.Model.EitherElement -- parent
    -> Canvas.Tool.Responsify.Model.PredictionParams
    -> Spec.Element.Model.EitherElement
    -> Element.Element Msg
layoutPanel environment userModel augmentationParams prediction predictionParams originalElement =
    let
        exportButton =
            case environment of
                Prod ->
                    Element.none

                _ ->
                    Ui.Component.buttonOnClick
                        (ExportPredictionParamsAndPrediction
                            (environment == Preview)
                            (Canvas.Tool.Responsify.Model.ResponsifyExport
                                predictionParams
                                prediction
                            )
                        )
                        "Export"
                        False

        processControls =
            Element.row
                []
                [ Ui.Component.buttonOnClick (AcceptClicked ( augmentationParams.selectionItem, prediction )) "Accept" False
                , Ui.Component.buttonOnClick RejectClicked "Reject" False
                    |> Element.el [ Element.scale 0.9 ]
                , exportButton
                ]
    in
    Element.column
        [ Element.spacing 20
        , Element.width Element.fill
        ]
        [ Element.paragraph [] [ Element.text "Previewing Responsify results." ]
        , Element.column
            [ Element.spacing 10 ]
            [ processControls
            , allControls userModel augmentationParams.selectionItem prediction
            ]
        ]


makePredictionParams :
    Canvas.Camera.Model.Model
    -> Spec.Element.Model.EitherElement
    -> Canvas.Tool.Responsify.Model.LayoutData
    -> Canvas.Tool.Responsify.Model.PredictionParams
makePredictionParams camera element { parent, children } =
    let
        ( parentId, _ ) =
            parent

        parentRel =
            Canvas.Camera.Convert.normalizeParentRectangle (Tuple.second parent)

        params =
            -- @@TODO: fix this
            { element = element
            , parentDimensions = ( parentId, parentRel )
            , siblingsDimensions = children
            }
    in
    params


previewSceneRectangle : Canvas.Camera.Model.Model -> Canvas.Events.SceneRectangle -> List (Element.Element Msg)
previewSceneRectangle camera rect =
    let
        styles =
            Renderer.StyleCompositor.render Renderer.InteractivityAttribs.schematicStyle

        (Canvas.Events.SceneRectangle sceneRect) =
            rect

        (Canvas.Events.AbsoluteRectangle absRect) =
            Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera rect

        siblingsDimensions =
            Rectangle.render sceneRect

        measurements =
            [ Element.el
                [ Element.height (Element.px 1)
                , Element.width Element.fill
                , Element.Background.color Ui.Style.highlightColorSolid
                , Element.inFront
                    (Element.el
                        [ Element.centerX, Ui.Help.noPointerEvents ]
                        (Element.text (String.fromFloat (Rectangle.width absRect)))
                    )
                , Ui.Help.noPointerEvents
                ]
                Element.none
            , Element.el
                [ Element.width (Element.px 1)
                , Element.height Element.fill
                , Element.Background.color Ui.Style.highlightColorSolid
                , Element.inFront
                    (Element.el
                        [ Element.centerY, Ui.Help.noPointerEvents ]
                        (Element.text (String.fromFloat (Rectangle.height absRect)))
                    )
                , Ui.Help.noPointerEvents
                ]
                Element.none
            ]
                |> List.map Element.inFront

        attribs =
            Ui.Help.noPointerEvents :: siblingsDimensions ++ styles ++ measurements
    in
    Element.el
        attribs
        Element.none
        |> List.singleton


type alias Return =
    { newState : Canvas.Tool.Responsify.Model.State
    , mutation : Maybe Spec.Mutation.Mutation
    , newSelection : Canvas.Selection.Selection
    , newSeed : Random.Seed
    , cmd : Cmd Msg
    }


type alias UpdateParms =
    { selection : Canvas.Selection.Selection
    , camera : Canvas.Camera.Model.Model
    , state : Canvas.Tool.Responsify.Model.State
    , msg : Msg
    , seed : Random.Seed
    }


update : Spec.Model.WithSpec userModel -> UpdateParms -> Return
update userModel { selection, camera, state, msg, seed } =
    case msg of
        NoOp ->
            Return state Nothing selection seed Cmd.none

        ScenePress ->
            Return Canvas.Tool.Responsify.Model.Closed Nothing Nothing seed Cmd.none

        Select newSelection ->
            Return Canvas.Tool.Responsify.Model.Closed Nothing (Just newSelection) seed Cmd.none

        SmartLayoutButtonClicked ( selectionItem, element ) layoutData ->
            let
                predictionParams =
                    layoutData
                        |> makePredictionParams camera element

                ( maybeRule, _, rulesPrediction ) =
                    runResponsifyEngine userModel predictionParams

                prediction =
                    case maybeRule of
                        Nothing ->
                            Nothing

                        Just _ ->
                            Just rulesPrediction

                state_ =
                    Canvas.Tool.Responsify.Model.LayingOut
                        { layoutData = layoutData
                        , targetSelectionItem = selectionItem
                        , prediction = prediction
                        , predictionParams = predictionParams
                        , element = element
                        }
            in
            Return state_ Nothing selection seed Cmd.none

        AcceptClicked ( selectionItem, element ) ->
            let
                mutation =
                    Spec.Mutation.UpdateShared
                        selectionItem
                        element.shared
            in
            Return Canvas.Tool.Responsify.Model.Closed (Just mutation) selection seed Cmd.none

        RejectClicked ->
            Return Canvas.Tool.Responsify.Model.Closed Nothing selection seed Cmd.none

        ExportPredictionParamsAndPrediction saveLocal data ->
            let
                cmd =
                    if saveLocal then
                        save data

                    else
                        write DumpCompleted data
            in
            Return state Nothing selection seed cmd

        UpdatePrediction shared ->
            let
                state_ =
                    case state of
                        Canvas.Tool.Responsify.Model.LayingOut layingOutData ->
                            Canvas.Tool.Responsify.Model.LayingOut { layingOutData | prediction = Just shared }

                        Canvas.Tool.Responsify.Model.Closed ->
                            state
            in
            Return state_ Nothing selection seed Cmd.none

        DumpCompleted _ ->
            Return state Nothing selection seed Cmd.none


renderChildPreview : Model.Model.UserModel -> Interface.Model.ScopeData -> Spec.Element.Layout.Flow -> Int -> Spec.Element.Model.FlowElement -> Element.Element msg
renderChildPreview userModel scope flow index element =
    let
        hints =
            case flow of
                Spec.Element.Layout.Column ->
                    []

                Spec.Element.Layout.Row ->
                    []

                Spec.Element.Layout.WrappedRow ->
                    []

        -- the layout that the element currently has
        currentLayout =
            Renderer.Layout.renderShared
                element.shared

        style =
            Spec.getStyle element.shared.id userModel

        renderedSize =
            Renderer.Layout.makeSizeAttribs userModel element.shared.id scope element.outerGeometry.size

        renderedAlignment =
            Renderer.Layout.makeAlignmentAttribs element.outerGeometry.alignment

        renderedLabel =
            [ Element.inFront <|
                Element.el
                    [ Element.Font.bold
                    , Element.padding 5
                    , Element.Font.color Ui.Style.highlightColorSolidImportant
                    ]
                <|
                    Element.text <|
                        String.fromInt (index + 1)
            ]

        attribs =
            borderHint
                ++ currentLayout.attribs
                ++ renderedSize
                ++ renderedAlignment
                ++ renderedLabel

        --        renderedChildren : List (Element.Element msg)
        --        renderedChildren =
        --            List.map
        --                renderChild_
        --                (Spec.Element.getChildren element)
    in
    currentLayout.el { outer = attribs, inner = [] } []


renderChild :
    Spec.Element.Model.Element a
    -> Element.Element msg
renderChild element =
    let
        -- the layout that the element currently has
        currentLayout =
            Renderer.Layout.renderShared element.shared

        attribs =
            borderHint ++ currentLayout.attribs

        renderedChildren : List (Element.Element msg)
        renderedChildren =
            List.map
                renderChild_
                (Spec.Element.getChildren element)
    in
    currentLayout.el { outer = attribs, inner = [] } []


renderChild_ =
    renderChild


borderHint =
    [ Ui.Style.class "outline"
    ]



-- What kinds of lines we draw to show the padding type.


type PaddingLines
    = NoLines
    | LRLines
    | TBLines
    | AllLines



-- The padding annotations that should appear on each side of the
-- element we're annotating.


type alias PaddingAnnotations =
    { leftRight : PaddingLines
    , topBottom : PaddingLines
    }



-- Generate padding annotations from an element's padding type.


paddingLines : Spec.Element.Layout.Padding.Padding -> PaddingAnnotations
paddingLines padding =
    case padding of
        PaddingEach _ ->
            { leftRight = NoLines, topBottom = NoLines }

        PaddingXY _ _ ->
            { leftRight = LRLines, topBottom = TBLines }

        EqualPadding _ ->
            { leftRight = AllLines, topBottom = AllLines }



-- Draw padding type annotations.


paddingBox : PaddingLines -> Element.Element Msg -> Element.Element Msg
paddingBox lines el =
    case lines of
        NoLines ->
            el

        AllLines ->
            Element.el [ Element.Border.width 2 ] el

        LRLines ->
            Element.el [ Element.Border.widthXY 2 0 ] el

        TBLines ->
            Element.el [ Element.Border.widthXY 0 2 ] el


{-| View the prediciton in the shape of guides to explain to the user what will be the result of their operation
-}
previewPrediction :
    Model.Model.UserModel
    -> Interface.Model.ScopeData
    -> Spec.Element.Model.EitherElement
    -> List (Element.Attribute Msg)
previewPrediction userModel scope element =
    let
        { padding, spacing, flow } =
            element.shared

        children =
            case element.shared.children of
                Spec.Element.Model.FlowChildren fl ->
                    fl

                -- a responsified element usually has no absolute children
                Spec.Element.Model.AbsoluteChildren _ ->
                    []

        -- Add text describing the size of the padding, along with
        -- annotations to show whether it's EqualPadding, PaddingXY or
        -- PaddingEach.
        paddingMeasures =
            let
                { top, bottom, left, right } =
                    derivedPadding

                lines =
                    paddingLines padding
            in
            [ Element.text (String.fromInt right)
                |> paddingBox lines.leftRight
                |> Element.el [ Element.centerY, Element.moveRight 5 ]
                |> Element.onRight
            , Element.text (String.fromInt left)
                |> paddingBox lines.leftRight
                |> Element.el [ Element.centerY, Element.moveLeft 5 ]
                |> Element.onLeft
            , Element.text (String.fromInt top)
                |> paddingBox lines.topBottom
                |> Element.el [ Element.centerX, Element.moveUp 5 ]
                |> Element.above
            , Element.text (String.fromInt bottom)
                |> paddingBox lines.topBottom
                |> Element.el [ Element.centerX, Element.moveDown 5 ]
                |> Element.below
            ]

        derivedPadding =
            Renderer.Layout.formatPadding padding

        spacingHint =
            Element.el
                [ Element.width Element.fill
                , Element.height (Element.px (Maybe.withDefault 0 spacing))
                , Element.Background.color Ui.Style.importantHighlightColor
                , Element.inFront <| Element.el [] <| Element.text (String.fromInt <| Maybe.withDefault 0 spacing)
                ]
                Element.none

        -- add a google-chrome like hint that describes the padding
        paddingHint =
            Element.el
                (Element.Border.widthEach (Renderer.Layout.formatPadding padding)
                    :: Element.Border.color Ui.Style.highlightColor
                    :: Ui.Help.noPointerEvents
                    :: Element.width Element.fill
                    :: Element.height Element.fill
                    :: paddingMeasures
                )
                Element.none
                |> Element.inFront

        -- row or column?
        layout : List (Element.Attribute msg) -> List (Element.Element msg) -> Element.Element msg
        layout =
            Renderer.Layout.getElmUiElementByLayout_ flow

        renderedChildren =
            List.indexedMap (renderChildPreview userModel scope flow) children

        -- all the guides are rendered as a layer on top of the current element
        outline =
            layout
                outlineAttrebs
                renderedChildren
                |> Element.inFront

        -- elm-ui attribs for the layer on top
        -- padding here pushes elements inside
        outlineAttrebs =
            -- make the layer span across
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing (Maybe.withDefault 0 spacing)

            -- padding and spacing relevant for recursive preview
            , Element.paddingEach derivedPadding

            -- , Element.spacing spacing handled by spacing hint
            -- don't catch pointer events because it would interfere
            -- @@TODO - think about if we should disable pointer events per default
            , Ui.Help.noPointerEvents

            -- google-chrome like padding
            , paddingHint
            , Element.Font.size 12
            , Element.Font.color Ui.Style.white
            ]
                ++ borderHint
                ++ Ui.Help.initFontStyles
    in
    [ outline ]


{-| Predict the resulting layout, i.e. find out which combination of layout parameters
result in the layout the user is drawing.
-}
predictForElement : ElementPredictionParams -> Maybe Canvas.Tool.Draw.Help.ResultingData
predictForElement { camera, from, to } =
    let
        sceneElementData =
            from.sourceElement

        newChildDimensions : Canvas.Events.AbsoluteRectangle
        newChildDimensions =
            Canvas.Camera.Convert.absoluteRectangleFromElementPoints
                camera
                from.point
                to.point

        params =
            { newChildDimensions = newChildDimensions
            , sceneElementData = sceneElementData
            , absoluteElementContextRect = absoluteElementContextRect
            , allSiblings = allSiblings
            }

        -- move each element to the top left corner and only keep height and width
        -- because the new child is relative to 0|0 origin
        normalizedSiblings : List Canvas.Events.AbsoluteRectangle
        normalizedSiblings =
            from.siblingsDimensions
                |> List.map (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera)

        absoluteElementContextRect : Canvas.Events.AbsoluteRectangle
        absoluteElementContextRect =
            Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle
                camera
                (Tuple.second from.point.context)

        allSiblings : List Canvas.Events.AbsoluteRectangle
        allSiblings =
            newChildDimensions
                :: normalizedSiblings
                |> List.sortBy
                    (\(Canvas.Events.AbsoluteRectangle r) ->
                        Rectangle.x1 r + Rectangle.y1 r
                    )
    in
    Nothing


makeMutationForElement :
    Random.Seed
    -> ElementPredictionParams
    -> ( Canvas.Tool.Responsify.Model.State, Maybe Spec.Mutation.Mutation, Random.Seed )
makeMutationForElement seed params =
    let
        ( id, newSeed ) =
            Spec.Element.Id.random seed

        mutation =
            predictForElement params
                |> always Nothing
    in
    ( Canvas.Tool.Responsify.Model.Closed, mutation, newSeed )


readLayoutData : Canvas.Camera.Model.Model -> Spec.Element.Id.Id -> Decode.Decoder Canvas.Tool.Responsify.Model.LayoutData
readLayoutData camera elementId =
    (Decode.at
        [ "target", "parentElement", "funk_forElement" ]
        Canvas.Events.readIdAndSceneRectangle
        |> Decode.map (parentToAbs camera)
    )
        |> Decode.andThen
            (\parentAbs ->
                Decode.at
                    [ "target", "parentElement", "funk_forElement" ]
                    Canvas.Events.readSiblings
                    |> Decode.map
                        (List.map (childrenToRel camera parentAbs))
                    |> Decode.map (Canvas.Tool.Responsify.Model.LayoutData parentAbs)
            )


parentToAbs : Canvas.Camera.Model.Model -> ( Spec.Element.Id.Id, Canvas.Events.SceneRectangle ) -> ( Spec.Element.Id.Id, Canvas.Events.AbsoluteRectangle )
parentToAbs camera =
    Tuple.mapSecond (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera)


childrenToRel :
    Canvas.Camera.Model.Model
    -> ( Spec.Element.Id.Id, Canvas.Events.AbsoluteRectangle )
    -> ( Spec.Element.Id.Id, Canvas.Events.SceneRectangle )
    -> ( Spec.Element.Id.Id, Canvas.Events.ElementRectangle )
childrenToRel camera parentAbs =
    Tuple.mapSecond
        (Canvas.Camera.Convert.absoluteRectangleFromSceneRectangle camera
            >> (\childAbs -> Canvas.Camera.Convert.elementRectangleFromAbsoluteRectangle (Tuple.second parentAbs) childAbs)
        )


type alias ElementPredictionParams =
    { from : Canvas.Tool.Responsify.Model.ElementPressData
    , to : Canvas.Tool.Responsify.Model.ElementPressData
    , camera : Canvas.Camera.Model.Model
    }


write : (Result Http.Error () -> msg) -> Canvas.Tool.Responsify.Model.ResponsifyExport -> Cmd msg
write msg responsifyExport =
    let
        content =
            Canvas.Tool.Responsify.Model.encodeResponsifyExport responsifyExport

        encodedContent =
            Encode.encode 4 content

        hash =
            sha256 encodedContent

        json =
            Encode.object
                [ ( "path", Encode.string (hash ++ ".json") )
                , ( "content", Encode.string encodedContent )
                ]
    in
    Http.post
        { url = "/api/responsify-examples"
        , body = Http.jsonBody json
        , expect = Http.expectWhatever msg
        }


save : Canvas.Tool.Responsify.Model.ResponsifyExport -> Cmd msg
save responsifyExport =
    let
        content =
            Canvas.Tool.Responsify.Model.encodeResponsifyExport responsifyExport

        encodedContent =
            Encode.encode 4 content
    in
    Download.string "responsify-export.json" "application/json" encodedContent


type Msg
    = ScenePress
    | SmartLayoutButtonClicked ( Canvas.Selection.SelectionItem, Spec.Element.Model.EitherElement ) Canvas.Tool.Responsify.Model.LayoutData
    | Select Canvas.Selection.SelectionItem
    | AcceptClicked ( Canvas.Selection.SelectionItem, Spec.Element.Model.EitherElement )
    | RejectClicked
    | ExportPredictionParamsAndPrediction Bool Canvas.Tool.Responsify.Model.ResponsifyExport
    | DumpCompleted (Result Http.Error ())
    | UpdatePrediction Spec.Element.Model.EitherElement
    | NoOp
