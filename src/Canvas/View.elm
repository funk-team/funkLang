module Canvas.View exposing
    ( Context
    , getBackgroundColorForScreen
    , initContext
    , renderShared
    , renderStyles
    , view
    )

import ApiExplorer
import Canvas.AttributesPanel
import Canvas.Camera
import Canvas.Events
import Canvas.Msg
import Canvas.Selection
import Canvas.StructurePanel
import Canvas.Tool
import Canvas.Tool.AugmentationParams
import Canvas.Tool.Model
import Canvas.Tool.Responsify
import Canvas.Tool.Transform.Handles
import CodeEditor
import Color.Extra
import DeployEditor
import DesignSystem
import DesignSystem.Color
import DesignSystem.Color.Model
import Element
import Element.Background
import Element.Border
import Element.Events.Extra
import Element.Font
import Element.Input
import FileSystem
import FileSystem.CliConnection
import Google.Fonts.LinkTag
import Html
import Html.Attributes
import Interface.Model
import Interface.Scope
import Model
import Model.Model
import ModelEditor
import Persistence
import Rectangle
import RemoteData
import Renderer.Content
import Renderer.InteractivityAttribs
import Renderer.Layout
import Renderer.StyleCompositor
import Route
import Runtime.Css
import Shortcut
import Slug
import Spec
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Spec.Element.Render
import Spec.Element.Style
import Spec.Mutation
import SupportSection
import SupportSection.Feedback
import SupportSection.Msg
import Ui.Boxicons
import Ui.Component
import Ui.Dropdown
import Ui.Help
import Ui.Style exposing (edges)
import Viewport



-- EVENTS
-- VIEW CODE


view editorMode project model =
    let
        body =
            [ Runtime.Css.render
            , cssVariables
            , additionalHead
            , editor
            ]

        editor =
            renderEditor editorMode project model

        cssVariables =
            Ui.Style.exposeCssVariables

        additionalHead =
            Google.Fonts.LinkTag.displayedFontLinkTags
                (Model.latest model)
    in
    { title = "Editor"
    , body = body
    }


{-| Info: The elements need this exact structure for the camera projections to work!
-}
renderEditor : Route.EditorMode -> Persistence.ProjectMeta -> Model.Model.Model -> Html.Html Canvas.Msg.RootMsg
renderEditor editorMode project model =
    let
        latest =
            Model.latest model

        renderedElementsOnCanvas =
            viewElementsOnCanvas project model
                |> List.map Element.inFront

        viewContent : Element.Element Canvas.Msg.RootMsg
        viewContent =
            Element.el
                (renderedElementsOnCanvas ++ augmentCanvas)
                Element.none
                |> Element.map Canvas.Msg.EditorMsg

        augmentCanvas =
            Canvas.Tool.augmentCanvas
                latest.camera
                latest.tool
                |> List.map (Element.mapAttribute Canvas.Msg.ToolMsg)

        main =
            case model.project of
                RemoteData.Success _ ->
                    viewLoaded

                RemoteData.Failure f ->
                    Element.column [ Element.width Element.fill ]
                        [ Element.text "We could not read the project. Reason: "
                        , Element.paragraph [ Ui.Style.monospace, Element.padding 20, Element.Background.color Ui.Style.slightAccent ]
                            [ Element.text f
                            ]
                        ]

                RemoteData.NotAsked ->
                    Element.el [ Element.centerX, Element.centerY, Element.Font.size 50 ] <|
                        Element.text "Loading metadata"

                RemoteData.Loading ->
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        , Element.Font.size 50
                        , Element.paddingEach { edges | bottom = 300 }
                        ]
                    <|
                        Element.column [ Element.Font.center, Element.spacing 20 ]
                            [ Element.text "Loading project"
                            , Element.el [ Element.Font.size 20 ] (Element.text "Please bear with us. We are still in beta.")
                            , Element.image [ Element.centerX ] { src = "/loading.gif", description = "" }
                            ]

        viewMode =
            case editorMode of
                Route.Code ->
                    CodeEditor.view project model
                        |> Element.map (Canvas.Msg.CodeEditorMsg >> Canvas.Msg.EditorMsg)

                Route.Canvas ->
                    viewEditor model viewContent

                Route.Files ->
                    FileSystem.view model
                        |> Element.map Canvas.Msg.FileSystemMsg

                Route.Api ->
                    case Route.getProjectData model.mode model.url of
                        Nothing ->
                            Element.none

                        Just projectMeta ->
                            ApiExplorer.view
                                projectMeta
                                latest.apiExplorer
                                |> Element.map (Canvas.Msg.ApiExplorerMsg >> Canvas.Msg.EditorMsg)

                Route.Model ->
                    case Route.getProjectData model.mode model.url of
                        Nothing ->
                            Element.none

                        Just projectMeta ->
                            ModelEditor.view
                                projectMeta
                                latest
                                |> Element.map Canvas.Msg.EditorMsg

                Route.DesignSystem ->
                    DesignSystem.view
                        model.googleFonts
                        latest.designSystem
                        |> Element.map (Canvas.Msg.DesignSystemMsg >> Canvas.Msg.EditorMsg)

                Route.Deploy ->
                    DeployEditor.view
                        model.mode
                        latest.deployEditor
                        |> Element.map (Canvas.Msg.DeployEditorMsg >> Canvas.Msg.EditorMsg)

        viewLoaded =
            Element.el
                [ Element.inFront (viewHeader project editorMode model)
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.paddingEach
                    { top =
                        if editorMode == Route.Canvas then
                            0

                        else
                            Ui.Style.headerHeight
                    , bottom = 0
                    , left = 0
                    , right = 0
                    }
                ]
                viewMode

        saveIndicator =
            Element.inFront <|
                Element.el [ Element.alignBottom ] <|
                    case (Model.latest model).specSaved of
                        True ->
                            Element.text "saved"

                        False ->
                            Element.text "saving..."
    in
    Element.layout
        [ Ui.Style.interFont
        , Element.Font.size 16
        , Element.width Element.fill
        , Element.height Element.fill
        , Shortcut.viewDebugger model.pressedKeys
        , SupportSection.Feedback.viewModal model.socialSection
            |> Element.mapAttribute Canvas.Msg.SupportSection
        , SupportSection.viewModalDocs model.socialSection
            |> Element.mapAttribute Canvas.Msg.SupportSection
        , SupportSection.viewModalCommunity model.socialSection
            |> Element.mapAttribute Canvas.Msg.SupportSection
        , SupportSection.viewModalRoadMap model.socialSection
            |> Element.mapAttribute Canvas.Msg.SupportSection
        , SupportSection.viewModalChangelog model.socialSection
            |> Element.mapAttribute Canvas.Msg.SupportSection
        , saveIndicator
        ]
        main


viewHeader :
    Persistence.ProjectMeta
    -> Route.EditorMode
    -> Model.Model.Model
    -> Element.Element Canvas.Msg.RootMsg
viewHeader projectMeta editorMode model =
    let
        modeSpecificSection =
            case editorMode of
                Route.Canvas ->
                    toolbar model

                _ ->
                    Element.none

        buttonPadding =
            Element.paddingXY (Ui.Style.headerHeight // 2) 0

        viewMode mode =
            let
                isActive =
                    mode == editorMode

                label =
                    Element.text (Model.modeToTabLabel mode)
                        |> Element.el [ Element.centerY ]

                syncBadge =
                    case mode of
                        Route.Files ->
                            case model.cliConnection of
                                FileSystem.CliConnection.Connected ->
                                    let
                                        tooltip =
                                            Ui.Component.tooltip "Synced with local filesystem"

                                        icon =
                                            Ui.Component.icon Ui.Boxicons.bxSync

                                        indicator =
                                            Element.el
                                                [ tooltip, Element.alignRight, Element.scale 0.6, Element.alpha 0.8 ]
                                                icon
                                    in
                                    [ Element.inFront indicator ]

                                _ ->
                                    []

                        _ ->
                            []

                attribs =
                    [ buttonPadding
                    , Element.height Element.fill
                    , Element.Background.color
                        (if isActive then
                            Ui.Style.highlightColorSolid

                         else
                            Ui.Style.transparent
                        )
                    ]
                        ++ syncBadge
            in
            Element.link
                attribs
                { label = label
                , url =
                    Route.makeUrl model.mode projectMeta (Route.Editor mode)
                }

        modes =
            [ Route.Canvas
            , Route.DesignSystem
            , Route.Api
            , Route.Model
            , Route.Code
            , Route.Deploy
            , Route.Files
            ]

        feedbackButton =
            SupportSection.viewOpenButton

        modeSelector =
            Element.row [ Element.height Element.fill, Element.alignRight ]
                (List.map viewMode modes)

        headerExpanded =
            case model.socialSection.isExpanded of
                True ->
                    let
                        feedbackSection =
                            Element.row
                                [ Element.height (Element.px Ui.Style.headerHeight)
                                , Element.Background.color Ui.Style.black
                                , Element.Font.color Ui.Style.white
                                , Element.width Element.fill
                                , Element.Events.Extra.onClickoutside SupportSection.Msg.ToggleExpanded
                                ]
                                [ feedbackButton
                                , SupportSection.docsButton
                                , SupportSection.communityButton model.socialSection.isExpanded
                                , SupportSection.roadMapButton
                                , SupportSection.changelogButton
                                , SupportSection.viewExpandOrCloseButton model.socialSection.isExpanded
                                ]
                    in
                    Element.row
                        [ Element.height (Element.px Ui.Style.headerHeight)
                        , Element.Background.color Ui.Style.black
                        , Element.Font.color Ui.Style.white
                        , Element.width Element.fill
                        ]
                        [ feedbackSection
                            |> Element.map Canvas.Msg.SupportSection
                        , modeSpecificSection
                            |> Element.map Canvas.Msg.EditorMsg
                        , modeSelector
                            |> Element.map Canvas.Msg.EditorMsg
                        ]

                False ->
                    Element.row
                        [ Element.height (Element.px Ui.Style.headerHeight)
                        , Element.Background.color Ui.Style.black
                        , Element.Font.color Ui.Style.white
                        , Element.width Element.fill
                        ]
                        [ feedbackButton
                            |> Element.map Canvas.Msg.SupportSection
                        , SupportSection.docsButton
                            |> Element.map Canvas.Msg.SupportSection
                        , SupportSection.viewExpandOrCloseButton model.socialSection.isExpanded
                            |> Element.map Canvas.Msg.SupportSection
                        , modeSpecificSection
                            |> Element.map Canvas.Msg.EditorMsg
                        , modeSelector
                            |> Element.map Canvas.Msg.EditorMsg
                        ]
    in
    headerExpanded


type alias Context =
    { model : Model.Model.Model
    , selectionItem : Canvas.Selection.SelectionItem
    , scope : Interface.Model.ScopeData
    , parent : Maybe Spec.Element.Model.EitherElement
    }


toolbar : Model.Model.Model -> Element.Element Canvas.Msg.Msg
toolbar model =
    let
        userModel =
            Model.latest model

        centerElement =
            Element.row
                [ Element.height Element.fill
                , Element.centerX
                ]
                [ cameraValue
                , hudVisibilityToggle
                ]

        cameraValue =
            Element.Input.button
                [ Element.padding 5
                , Element.height Element.fill
                , Ui.Help.allPointerEvents
                ]
                { label =
                    Element.text
                        (String.fromInt (round (userModel.camera.zoom * 100))
                            ++ "%"
                        )
                , onPress =
                    Just (Canvas.Msg.ResetCamera 1.0)
                }

        hudVisibilityToggle =
            Element.Input.checkbox
                [ Element.padding 5
                , Element.height Element.fill
                ]
                { onChange = Canvas.Msg.ChangeHudVisibility
                , icon =
                    \b ->
                        Element.el
                            [ Element.centerY
                            , Element.padding 6
                            , Element.Border.rounded 3
                            , if b then
                                Element.Background.color Ui.Style.highlightColorSolid

                              else
                                Element.Background.color (Element.rgb255 64 64 64)
                            ]
                            (Element.text "HUD")
                , checked = userModel.hudVisibility
                , label =
                    Element.Input.labelHidden "HUD visibility"
                }

        drawingToolVisibilityToggle =
            Element.Input.button
                [ Element.padding 5
                , Element.height Element.fill
                , Ui.Help.allPointerEvents
                , Element.moveLeft 35
                ]
                { label = Ui.Component.icon Ui.Boxicons.bxChevronDown |> Element.el [ Element.centerY, Element.moveUp 1 ]
                , onPress =
                    Just Canvas.Msg.ClickToolDropDown
                }

        drawingDropDownPrams : Canvas.Tool.Model.DrawingDropDownPrams
        drawingDropDownPrams =
            { dropDownOpen = userModel.drawingDropDownVisibility
            , selectedTool = userModel.tool
            , previousTool = userModel.previousTool
            }

        closeDropDownOnClickOutSide =
            case drawingDropDownPrams.dropDownOpen of
                True ->
                    [ Element.Events.Extra.onClickoutside Canvas.Msg.ClickToolDropDown ]

                False ->
                    []

        controls =
            Element.row
                ([ Ui.Help.allPointerEvents
                 , Element.alignLeft
                 , Element.alignTop
                 , Element.width Element.fill
                 , Element.height Element.fill
                 ]
                    ++ closeDropDownOnClickOutSide
                )
                [ Canvas.Tool.drawingModeDropDown drawingDropDownPrams
                    |> Element.map Canvas.Msg.ChangeTool
                , drawingToolVisibilityToggle
                , Canvas.Tool.selectModetoolbar userModel.tool
                    |> Element.map Canvas.Msg.ChangeTool
                , centerElement
                ]
    in
    controls


{-| View elements that are not transformed by the camera like
sidebars, tool controls, camera debugger
-}
editorPanels : Model.Model.Model -> Element.Attribute Canvas.Msg.RootMsg
editorPanels model =
    let
        latest =
            Model.latest model

        structurePanel =
            Canvas.StructurePanel.viewAllScreens
                latest

        panels =
            Element.row
                [ Element.alignTop
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.paddingEach { top = Ui.Style.headerHeight, bottom = 0, left = 0, right = 0 }
                ]
                [ structurePanel |> Element.map Canvas.Msg.EditorMsg
                , Canvas.AttributesPanel.view model latest
                ]
    in
    panels
        |> Element.el [ Element.height Element.fill, Element.width Element.fill, Ui.Help.noPointerEvents ]
        |> Element.inFront


{-| Draw an outline around selected elements
-}
markSelected : Model.Model.UserModel -> Bool -> Element.Attribute msg
markSelected userModel isSelected =
    if isSelected then
        Canvas.Tool.Transform.Handles.outline userModel.camera

    else
        Element.none |> Element.behindContent


viewElementsOnCanvas : Persistence.ProjectMeta -> Model.Model.Model -> List (Element.Element Canvas.Msg.Msg)
viewElementsOnCanvas project model =
    Model.latest model
        |> Spec.getScreensWithUniqueSlugs
        |> List.indexedMap (viewScreen project model)
        |> List.concatMap identity


makeAugmentationParams :
    Context
    -> Spec.Element.Model.EitherElement
    -> Canvas.Tool.AugmentationParams.AugmentationParams
makeAugmentationParams { model, selectionItem, scope } element =
    Canvas.Tool.AugmentationParams.AugmentationParams
        (Model.latest model).camera
        selectionItem
        (Model.latest model).selection
        element
        scope
        model.pressedKeys


renderChildren :
    Context
    -> Spec.Element.Model.EitherElement
    ->
        { absolute : List (Element.Attribute Canvas.Msg.Msg)
        , flow : List (Element.Element Canvas.Msg.Msg)
        }
renderChildren context element =
    let
        updateContext child =
            { context
                | parent = Just element
                , selectionItem = Canvas.Selection.addOne child.shared.id context.selectionItem
            }

        ( flow, absolute ) =
            case element.shared.children of
                Spec.Element.Model.AbsoluteChildren abs ->
                    ( []
                    , abs
                        |> List.map
                            (\child ->
                                renderAbsoluteElement
                                    (updateContext child)
                                    child
                                    |> List.take 1
                                    |> List.map Element.inFront
                            )
                        |> List.concatMap identity
                    )

                Spec.Element.Model.FlowChildren fl ->
                    ( fl
                        |> List.map
                            (\child -> renderFlowElement (updateContext child) child)
                        |> List.concatMap identity
                    , []
                    )
    in
    { absolute = absolute
    , flow = flow
    }


initContext : Spec.Element.Model.Screen -> Model.Model.Model -> Context
initContext rootElement model =
    { model = model
    , selectionItem = Canvas.Selection.fresh rootElement.shared.id
    , scope = Interface.Scope.empty
    , parent = Nothing
    }


type alias Shared msg =
    List (Element.Attribute msg)
    -> List (Element.Element msg)


renderShared : Context -> Spec.Element.Model.EitherElement -> Shared Canvas.Msg.Msg
renderShared context element_ =
    let
        userModel =
            Model.latest context.model

        padding_ =
            Element.paddingEach (Renderer.Layout.formatPadding padding)

        { flow, padding, spacing } =
            element_.shared

        id =
            Spec.Element.Id.toHtmlId element_.shared.id

        renderFunction : Renderer.Content.RenderFunction Canvas.Msg.Msg
        renderFunction =
            Renderer.Content.renderAndUpdateContextOnCanvas
                element_
                (Model.latest context.model)
                context.scope

        elmUiElement :
            List (Element.Attribute Canvas.Msg.Msg)
            -> List (Element.Element Canvas.Msg.Msg)
        elmUiElement =
            renderFunction
                { layout = Renderer.Layout.getElmUiElementByLayout flow
                , render = renderInListContext
                }

        -- this function renders basically everything. For things to work properly we also need the list context as it is passed to the augmentations
        renderInListContext : Maybe Int -> Interface.Model.ScopeData -> Renderer.Content.ChildrenAndParentAttribs Canvas.Msg.Msg
        renderInListContext maybeDataIndex updatedDataContext =
            let
                augmentationParams =
                    makeAugmentationParams
                        withDataIndex
                        element_

                responsifyEnhancements =
                    Canvas.Tool.Responsify.augmentElement
                        (Canvas.Tool.Responsify.detectEnvironment withDataIndex.model.url)
                        userModel
                        augmentationParams
                        userModel.evolveState
                        |> List.map (Element.mapAttribute Canvas.Msg.ResponsifyToolMsg)

                withDataIndex =
                    { context
                        | selectionItem =
                            Canvas.Selection.setIndex
                                maybeDataIndex
                                context.selectionItem
                    }

                otherEnhancements =
                    augmentations.other
                        |> List.map (Element.mapAttribute Canvas.Msg.ToolMsg)

                -- add stuff required to make the rendered element interactive
                augmentations =
                    Canvas.Tool.augmentElement
                        userModel
                        context.parent
                        augmentationParams
                        userModel.tool

                element =
                    augmentations.element

                children =
                    renderChildren
                        { withDataIndex
                            | scope = updatedDataContext
                        }
                        element

                attributes =
                    { other =
                        id
                            :: selectionHighlight
                            :: responsifyEnhancements
                            ++ otherEnhancements
                            ++ styles
                    , geometry = { outer = outerGeometry, inner = innerGeometry }
                    }

                -- tool can override geometry
                outerGeometry =
                    case augmentations.layout of
                        [] ->
                            renderOuterGeometry userModel withDataIndex element_

                        _ ->
                            augmentations.layout |> List.map (Element.mapAttribute Canvas.Msg.ToolMsg)

                styles =
                    let
                        hud =
                            case ( isHUDVisible context.model, element_.outerGeometry ) of
                                -- do not override background colors of screens
                                ( _, Spec.Element.Model.ScreenGeometry _ ) ->
                                    []

                                ( True, _ ) ->
                                    Renderer.InteractivityAttribs.schematicStyle
                                        |> Renderer.StyleCompositor.render

                                _ ->
                                    []
                    in
                    case
                        renderStyles
                            withDataIndex.scope
                            userModel
                            element_
                    of
                        -- if there are no styles defined and no content, draw an outline :)
                        Nothing ->
                            hud

                        Just s ->
                            -- the style priority is given to the last element in the list
                            -- hud has grey background to distinguish elements
                            -- whereas s can have a different background color set by the
                            -- user which will overwrite hud one
                            hud ++ s
            in
            { children = children
            , attribs = attributes
            }

        isSelected =
            Just context.selectionItem == userModel.selection

        selectionHighlight =
            markSelected
                userModel
                isSelected

        innerGeometry =
            padding_ :: Spec.Element.Render.spacing userModel element_ spacing
    in
    elmUiElement


renderOuterGeometry : Model.Model.UserModel -> Context -> Spec.Element.Model.EitherElement -> List (Element.Attribute Canvas.Msg.Msg)
renderOuterGeometry userModel context element =
    case element.outerGeometry of
        Spec.Element.Model.ScreenGeometry screenSize ->
            let
                (Canvas.Events.AbsoluteRectangle size) =
                    Spec.Mutation.screenSizeToAbsoluteRectangle screenSize

                outerGeometry =
                    Rectangle.renderEach size
                        |> (\each ->
                                [ Element.width (Element.shrink |> Element.minimum each.width)
                                , Element.height (Element.shrink |> Element.minimum each.height)
                                ]
                                    ++ each.offset
                           )

                renderedGeometry =
                    outerGeometry
                        |> List.map (Element.mapAttribute Canvas.Msg.ToolMsg)
            in
            renderedGeometry

        Spec.Element.Model.FlowElementGeometry outerGeometry ->
            let
                -- the layout that the element currently has
                currentLayout =
                    Renderer.Layout.renderShared element.shared

                currentSize =
                    Renderer.Layout.makeSizeAttribs
                        userModel
                        element.shared.id
                        context.scope
                        outerGeometry.size

                currentAlignment =
                    Renderer.Layout.makeAlignmentAttribs outerGeometry.alignment

                finalLayout =
                    currentLayout.attribs
            in
            finalLayout
                ++ currentSize
                ++ currentAlignment

        Spec.Element.Model.AbsoluteElementGeometry rect ->
            Spec.getAbsoluteElementRectangle userModel context.scope rect
                |> (\(Canvas.Events.ElementRectangle size) -> size)
                |> Rectangle.render


viewScreen :
    Persistence.ProjectMeta
    -> Model.Model.Model
    -> Int
    -> ( Slug.Slug, Spec.Element.Model.Screen )
    -> List (Element.Element Canvas.Msg.Msg)
viewScreen projectMeta model index ( slug, screen ) =
    let
        id =
            screen.shared.id

        maybeStyles =
            Spec.Element.Id.getFromDict id userModel.elementStyles

        backgroundColor =
            getBackgroundColorForScreen maybeStyles designSystem.colorEditor

        userModel =
            Model.latest model

        context =
            initContext screen model

        shared =
            renderShared
                context
                (Spec.Element.wrapScreen screen)

        designSystem =
            userModel.designSystem

        textColor =
            designSystem.colorEditor.text
                |> .value
                |> Color.Extra.toElmUi
                |> Element.Font.color

        label =
            Element.el
                [ Element.Font.color Ui.Style.highlightColorSolid ]
                (Element.text (Canvas.Selection.screenIndexToLabel screen.shared.label index))

        labelAndPreviewButton =
            Element.row
                ([ Element.spacing 10
                 , Element.moveUp 5
                 , Element.Font.size 16
                 ]
                    ++ Ui.Help.initFontStyles
                )
                [ label
                , previewButton
                , viewScreenSizeDropdown screen context
                    |> Element.el [ Element.alignRight ]
                ]
                |> Element.above

        previewUrl =
            Route.makeUrl model.mode projectMeta (Route.Preview slug)

        previewButton =
            Element.newTabLink
                [ Element.paddingEach { top = 1, bottom = 2, left = 2, right = 7 }
                , Element.Border.rounded 2
                , Element.Border.width 1
                , Element.Border.color Ui.Style.highlightColorSolid
                , Element.Font.color Ui.Style.highlightColorSolid
                , Element.mouseOver
                    [ Element.Font.color Ui.Style.white
                    , Element.Background.color Ui.Style.highlightColorSolid
                    ]
                ]
                { label =
                    Element.row []
                        [ Ui.Component.icon Ui.Boxicons.bxPlay
                        , Element.text "Preview"
                        ]
                , url = previewUrl
                }

        pageAttributes =
            [ Element.htmlAttribute (Html.Attributes.class "funk-canvas-screen")
            , labelAndPreviewButton
            ]

        attribs =
            pageAttributes
                ++ backgroundColor
                ++ [ textColor ]
    in
    shared
        attribs


viewScreenSizeDropdown element context =
    let
        screenSizePresetSelection =
            let
                maybeDeviceName =
                    case element.outerGeometry of
                        Spec.Element.Model.Custom _ ->
                            Nothing

                        Spec.Element.Model.Preset device _ ->
                            Just device.name
            in
            Ui.Dropdown.view
                [ Element.Font.color Ui.Style.highlightColorSolid
                , Element.Font.size 16
                ]
                { label =
                    case element.outerGeometry of
                        Spec.Element.Model.Custom absRect ->
                            absRectToString absRect

                        Spec.Element.Model.Preset device _ ->
                            device.name
                , contents =
                    [ customDropdownRow ]
                        :: (Viewport.groupedPresets
                                |> List.map Tuple.second
                                |> List.map (List.map (presetToDropdownRow maybeDeviceName))
                           )
                        |> List.intersperse [ Ui.Dropdown.dividingLine ]
                        |> List.concat
                }

        customDropdownRow =
            Ui.Dropdown.viewRow
                { isSelected = maybeToBool maybeAbsRect
                , label = Ui.Dropdown.Description "Custom"
                , detail = Ui.Dropdown.NoDetail
                , sideNote =
                    case maybeAbsRect of
                        Just absRect ->
                            Ui.Dropdown.Description (absRectToString absRect)

                        Nothing ->
                            Ui.Dropdown.NoDetail
                , onSelect =
                    { element | outerGeometry = Spec.Element.Model.Custom (Spec.Mutation.screenSizeToAbsoluteRectangle element.outerGeometry) }
                        |> Spec.Mutation.UpdateScreen context.selectionItem
                        |> Canvas.Msg.GotMutation
                , rightHandText = Nothing
                }

        maybeAbsRect =
            case element.outerGeometry of
                Spec.Element.Model.Custom absRect ->
                    Just absRect

                Spec.Element.Model.Preset _ _ ->
                    Nothing

        absRectToString absRect =
            let
                (Canvas.Events.AbsoluteRectangle rect) =
                    absRect

                width =
                    Rectangle.width rect

                height =
                    Rectangle.height rect
            in
            String.fromInt (round width)
                ++ " x "
                ++ String.fromInt (round height)

        presetToDropdownRow maybeDeviceName preset =
            let
                device =
                    Viewport.presetToDevice preset
            in
            Ui.Dropdown.viewRow
                { isSelected =
                    case maybeDeviceName of
                        Nothing ->
                            False

                        Just name ->
                            name == device.name
                , label = Viewport.presetToString preset |> Ui.Dropdown.Description
                , detail = Ui.Dropdown.NoDetail
                , sideNote =
                    Ui.Dropdown.Description
                        (String.fromInt device.width
                            ++ " x "
                            ++ String.fromInt device.height
                        )
                , onSelect =
                    let
                        (Canvas.Events.AbsoluteRectangle rect) =
                            Spec.Mutation.screenSizeToAbsoluteRectangle element.outerGeometry

                        outerGeometry =
                            Spec.Element.Model.Preset device { x = Rectangle.x1 rect, y = Rectangle.y1 rect }
                    in
                    { element | outerGeometry = outerGeometry }
                        |> Spec.Mutation.UpdateScreen context.selectionItem
                        |> Canvas.Msg.GotMutation
                , rightHandText = Nothing
                }
    in
    screenSizePresetSelection


getBackgroundColorForScreen : Maybe Spec.Element.Style.Style -> DesignSystem.Color.Model.Model -> List (Element.Attribute msg)
getBackgroundColorForScreen maybeStyles colorEditor =
    let
        maybeBackgroundColor =
            case maybeStyles of
                Nothing ->
                    Nothing

                Just { background } ->
                    case background of
                        Nothing ->
                            Nothing

                        Just background_ ->
                            Just background_
    in
    case Spec.Element.Render.renderBackground maybeBackgroundColor colorEditor of
        [] ->
            DesignSystem.Color.white |> Color.Extra.toElmUi |> Element.Background.color |> List.singleton

        backgroundColor_ ->
            backgroundColor_


renderStyles :
    Interface.Model.ScopeData
    -> Model.Model.UserModel
    -> Spec.Element.Model.EitherElement
    -> Maybe (List (Element.Attribute msg))
renderStyles scopeData userModel element =
    Spec.Element.Render.style True scopeData userModel element
        |> Maybe.map Renderer.StyleCompositor.render


isHUDVisible : Model.Model.Model -> Bool
isHUDVisible model =
    Model.latest model
        |> .hudVisibility


renderFlowElement :
    Context
    -> Spec.Element.Model.FlowElement
    -> List (Element.Element Canvas.Msg.Msg)
renderFlowElement context element =
    renderEitherChild
        context
        (Spec.Element.wrapFlow element)


renderAbsoluteElement :
    Context
    -> Spec.Element.Model.AbsoluteElement
    -> List (Element.Element Canvas.Msg.Msg)
renderAbsoluteElement context element =
    renderEitherChild
        context
        (Spec.Element.wrapAbsolute element)


renderEitherChild :
    Context
    -> Spec.Element.Model.EitherElement
    -> List (Element.Element Canvas.Msg.Msg)
renderEitherChild context element =
    renderShared context element []


{-| We inject the the message currently to make it compatible with our main app
-}
viewEditor :
    Model.Model.Model
    -> Element.Element Canvas.Msg.RootMsg
    -> Element.Element Canvas.Msg.RootMsg
viewEditor model content =
    let
        internalAttribs : List (Element.Attribute Canvas.Msg.RootMsg)
        internalAttribs =
            interactiveAttribs (Model.latest model).camera
                |> List.map (Element.mapAttribute Canvas.Msg.EditorMsg)

        allAttribs =
            editorPanels model :: internalAttribs

        interactiveAttribs camera =
            [ Element.clip
            , Element.width Element.fill
            , Element.height Element.fill
            , sceneBackground
            ]
                ++ augmentScene
                ++ Canvas.Camera.events Canvas.Msg.CameraMoved

        latest =
            Model.latest model

        augmentScene =
            Canvas.Tool.augmentScene
                latest.camera
                latest.tool
                |> List.map (Element.mapAttribute Canvas.Msg.ToolMsg)

        canvasAttribs : List (Element.Attribute Canvas.Msg.RootMsg)
        canvasAttribs =
            Canvas.Camera.canvasCodes (Model.latest model).camera

        canvas =
            Element.el
                canvasAttribs
                content
    in
    Element.el allAttribs <|
        canvas


sceneBackground =
    Element.Background.color (Element.rgba 0 0 0 0.1)



---- UTILS ----


maybeToBool maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False
