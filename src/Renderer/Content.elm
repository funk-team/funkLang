module Renderer.Content exposing
    ( ChildrenAndParentAttribs
    , RenderFunction
    , SharedAttribs
    , Wrapper
    , mergeSharedAttribs
    , noWrapper
    , renderAndUpdateContext
    , renderAndUpdateContextOnCanvas
    )

{-| This module is responsible for

  - fetching data from scopes
  - updating scopes
  - @@TODO: identifying missing pieces of data from the scope
    we recurse through the render tree.

attribsInjectedByCanvasOrPreviewRenderer get injected by the elmUiParams: for example then augmentations from the canvas

There are two render functions which need to be updated to correctly get the preview and canvas to update these are

  - renderAndUpdateContext - Preview
  - renderAndUpdateContextOnCanvas - Canvas

-}

import Canvas.Msg
import DesignSystem.IconBrowser
import DesignSystem.IconBrowser.Model
import DesignSystem.Typography
import Dict
import Dynamic.Data
import Element
import Element.Background
import Element.Border
import Element.Input
import Html
import Html.Attributes
import Interface.Data
import Interface.Model
import Interface.Scope
import Json.Decode as Decode
import Model
import Model.Model
import Renderer.Help
import Renderer.Layout
import Spec.DataConnection
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Spec.Element.Render
import Spec.Element.Style
import Ui.Boxicons
import Ui.Component
import Ui.Style


type alias SpecificRenderParams msg =
    { scope : Interface.Model.ScopeData
    , elmUiParams : ElmUiParams msg
    , attribsFromRendererForOuterElement : List (Element.Attribute msg)
    , wrapper : Wrapper msg
    , backgroundImage : Maybe String
    }


{-| this can be used to add link capabilities to an element

takes attributes and the element to render as a render function

returns the rendered element

Internally it can wrap the element another level or augment the attributes

-}
type alias Wrapper msg =
    Renderer.Layout.Attribs msg
    ->
        { wrap : Element.Element msg -> Element.Element msg
        , innerAttribs : Renderer.Layout.Attribs msg
        }


noWrapper : Wrapper msg
noWrapper attribs =
    { innerAttribs = attribs, wrap = identity }


type alias RenderFunction msg =
    ElmUiParams msg
    -> List (Element.Attribute msg)
    -> List (Element.Element msg)


mergeSharedAttribs { geometry, other } =
    { geometry
        | outer = geometry.outer ++ other
    }


mergeSharedAttribsWithStylesOuter { geometry, other } elementStyles =
    { geometry
        | outer = geometry.outer ++ other ++ elementStyles
    }


{-| The paramters for an element being rendered
-}
type alias ElmUiParams msg =
    { layout : Renderer.Layout.ElmUiRenderFunction msg
    , render :
        Maybe Int
        -> Interface.Model.ScopeData
        -> ChildrenAndParentAttribs msg
    }


{-| Attributes and children need to be used separately depending on the type of element
-}
type alias ChildrenAndParentAttribs msg =
    { attribs : SharedAttribs msg
    , children :
        { absolute : List (Element.Attribute msg)
        , flow : List (Element.Element msg)
        }
    }


{-| Because some elm-ui elements (input which is composed of multiple elements) actually need a wrapper with outer geometry attribs applied to them
to work properly we need to pull thema apart
-}
type alias SharedAttribs msg =
    { geometry :
        Renderer.Layout.Attribs msg
    , other : List (Element.Attribute msg)
    }



-- @@TODO: merge new scopes


type alias OnTextInput msg =
    Int -> String -> msg


{-| Render for preview and runtime
-}
renderAndUpdateContext :
    { element : Spec.Element.Model.EitherElement
    , wrap : Wrapper msg
    , userModel : Model.Model.UserModel
    , scope : Interface.Model.ScopeData
    , onTextInput : OnTextInput msg
    }
    -> RenderFunction msg
renderAndUpdateContext { element, wrap, userModel, scope, onTextInput } =
    let
        content =
            Renderer.Help.getContent
                element.shared.id
                userModel
                scope

        preventInteraction =
            False

        maybeStyle =
            Spec.Element.Id.getFromDict element.shared.id userModel.elementStyles

        renderFunction : RenderFunction msg
        renderFunction =
            case element.outerGeometry of
                Spec.Element.Model.ScreenGeometry _ ->
                    \params attribsInjectedByCanvasOrPreviewRenderer ->
                        viewWithChildren
                            { scope = scope
                            , elmUiParams = params
                            , attribsFromRendererForOuterElement = attribsInjectedByCanvasOrPreviewRenderer
                            , wrapper = noWrapper
                            , backgroundImage = Nothing
                            }

                _ ->
                    case element.shared.kind of
                        Spec.Element.Model.TextInput { modelField, placeholder } ->
                            \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                                let
                                    sharedAttribs =
                                        (elmUiParams.render Nothing scope).attribs

                                    inputAttribs =
                                        Element.Border.rounded 0
                                            :: Element.Border.width 0
                                            :: Element.Background.color Ui.Style.transparent
                                            :: sharedAttribs.other
                                            ++ sharedAttribs.geometry.inner
                                            ++ [ Element.height Element.fill, Element.width Element.fill ]

                                    value =
                                        case Dict.get modelField userModel.runtimeModel of
                                            Just (Dynamic.Data.StringInstance str) ->
                                                str

                                            _ ->
                                                ""

                                    typoPlaceholderStyles =
                                        case maybeStyle of
                                            Nothing ->
                                                DesignSystem.Typography.defaultPlaceholderStyles

                                            Just elementStyles ->
                                                case elementStyles.placeholderText of
                                                    Spec.Element.Style.TypoFromDesignSystem id_ ->
                                                        case Dict.get id_ userModel.designSystem.typoEditor.typos of
                                                            Just typo ->
                                                                DesignSystem.Typography.typoToAttributes userModel.designSystem.colorEditor typo

                                                            Nothing ->
                                                                DesignSystem.Typography.defaultPlaceholderStyles

                                                    Spec.Element.Style.CustomTypo typo ->
                                                        DesignSystem.Typography.typoToAttributes userModel.designSystem.colorEditor typo

                                                    Spec.Element.Style.NoTypo ->
                                                        DesignSystem.Typography.defaultPlaceholderStyles

                                    shouldWrap_ =
                                        case maybeStyle of
                                            Nothing ->
                                                False

                                            Just elementStyles ->
                                                case elementStyles.elementText of
                                                    Spec.Element.Style.TypoFromDesignSystem id_ ->
                                                        case Dict.get id_ userModel.designSystem.typoEditor.typos of
                                                            Just typo ->
                                                                typo.shouldWrap

                                                            Nothing ->
                                                                False

                                                    Spec.Element.Style.CustomTypo typo ->
                                                        typo.shouldWrap

                                                    Spec.Element.Style.NoTypo ->
                                                        False

                                    viewPlaceholder =
                                        Element.Input.placeholder typoPlaceholderStyles (Element.text placeholder)
                                in
                                case shouldWrap_ of
                                    True ->
                                        Element.Input.multiline
                                            inputAttribs
                                            { label = Element.Input.labelHidden ""
                                            , text = value
                                            , onChange = onTextInput modelField
                                            , placeholder = Just viewPlaceholder
                                            , spellcheck = False
                                            }
                                            |> Element.el (attribsInjectedByCanvasOrPreviewRenderer ++ sharedAttribs.geometry.outer)
                                            |> List.singleton

                                    False ->
                                        Element.Input.text
                                            inputAttribs
                                            { label = Element.Input.labelHidden ""
                                            , text = value
                                            , onChange = onTextInput modelField
                                            , placeholder = Just viewPlaceholder
                                            }
                                            |> Element.el (attribsInjectedByCanvasOrPreviewRenderer ++ sharedAttribs.geometry.outer)
                                            |> List.singleton

                        Spec.Element.Model.Button ->
                            -- TODO use actual button element
                            case content of
                                Nothing ->
                                    \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                                        viewWithChildren
                                            { scope = scope
                                            , backgroundImage = Nothing
                                            , elmUiParams = elmUiParams
                                            , attribsFromRendererForOuterElement = attribsInjectedByCanvasOrPreviewRenderer
                                            , wrapper = wrap
                                            }

                                Just { value } ->
                                    renderContent value

                        Spec.Element.Model.Box ->
                            case content of
                                Nothing ->
                                    \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                                        viewWithChildren
                                            { scope = scope
                                            , elmUiParams = elmUiParams
                                            , attribsFromRendererForOuterElement = attribsInjectedByCanvasOrPreviewRenderer
                                            , wrapper = wrap
                                            , backgroundImage = Nothing
                                            }

                                Just { value } ->
                                    renderContent value

        renderContent : Interface.Data.RefinedValue -> RenderFunction msg
        renderContent value =
            case value of
                Interface.Data.ParagraphText txt ->
                    \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                        let
                            attribs =
                                (elmUiParams.render Nothing scope).attribs
                                    |> mergeSharedAttribs
                                    |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer

                            wrapper =
                                wrap attribs
                        in
                        viewText (wrapper.innerAttribs |> Renderer.Layout.flattenAttribs) txt
                            |> wrapper.wrap
                            |> List.singleton

                Interface.Data.Icon icon ->
                    \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                        let
                            attribs =
                                (elmUiParams.render Nothing scope).attribs
                                    |> mergeSharedAttribs
                                    |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer

                            wrapper =
                                wrap attribs
                        in
                        viewIcon (wrapper.innerAttribs |> Renderer.Layout.flattenAttribs) icon
                            |> wrapper.wrap
                            |> List.singleton

                Interface.Data.YoutubeEmbed embedInfo ->
                    \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                        let
                            attribs =
                                (elmUiParams.render Nothing scope).attribs
                                    |> mergeSharedAttribs
                                    |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer

                            wrapper =
                                wrap attribs
                        in
                        viewYoutubeEmbed (wrapper.innerAttribs |> Renderer.Layout.flattenAttribs) embedInfo preventInteraction
                            |> wrapper.wrap
                            |> List.singleton

                Interface.Data.Media { kind, meta } ->
                    case kind of
                        -- render a paragraph if it's text content
                        Interface.Data.Image ->
                            \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                                case Spec.Element.hasChildren element of
                                    True ->
                                        viewWithChildren
                                            { scope = scope
                                            , elmUiParams = elmUiParams
                                            , attribsFromRendererForOuterElement = attribsInjectedByCanvasOrPreviewRenderer
                                            , wrapper = wrap
                                            , backgroundImage = Just meta.src
                                            }

                                    False ->
                                        let
                                            wrapper =
                                                wrap attribs

                                            attribs =
                                                (elmUiParams.render Nothing scope).attribs
                                                    |> mergeSharedAttribs
                                                    |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer
                                        in
                                        viewImage (wrapper.innerAttribs |> Renderer.Layout.flattenAttribs) meta.src meta.title
                                            |> wrapper.wrap
                                            |> List.singleton

                        Interface.Data.Pdf ->
                            \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                                let
                                    attribs =
                                        (elmUiParams.render Nothing scope).attribs
                                            |> mergeSharedAttribs
                                            |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer

                                    wrapper =
                                        wrap attribs
                                in
                                viewPdf (wrapper.innerAttribs |> Renderer.Layout.flattenAttribs) meta preventInteraction
                                    |> wrapper.wrap
                                    |> List.singleton

                        Interface.Data.Audio ->
                            \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                                let
                                    attribs =
                                        (elmUiParams.render Nothing scope).attribs
                                            |> mergeSharedAttribs
                                            |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer

                                    wrapper =
                                        wrap attribs
                                in
                                viewAudio (wrapper.innerAttribs |> Renderer.Layout.flattenAttribs) meta preventInteraction
                                    |> wrapper.wrap
                                    |> List.singleton

                        Interface.Data.Video ->
                            \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                                let
                                    attribs =
                                        (elmUiParams.render Nothing scope).attribs
                                            |> mergeSharedAttribs
                                            |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer

                                    wrapper =
                                        wrap attribs
                                in
                                viewVideo element (wrapper.innerAttribs |> Renderer.Layout.flattenAttribs) maybeStyle meta preventInteraction
                                    |> wrapper.wrap
                                    |> List.singleton

                Interface.Data.InterfaceDataList list ->
                    \params attribsInjectedByCanvasOrPreviewRenderer ->
                        viewInterfaceDataList
                            { scope = scope
                            , elmUiParams = params
                            , attribsFromRendererForOuterElement = attribsInjectedByCanvasOrPreviewRenderer
                            , wrapper = wrap
                            , backgroundImage = Nothing
                            }
                            list
    in
    renderFunction


viewInterfaceDataList :
    SpecificRenderParams msg
    -> Interface.Data.ListData
    -> List (Element.Element msg)
viewInterfaceDataList { scope, elmUiParams, attribsFromRendererForOuterElement, wrapper } apiListData =
    let
        { layout, render } =
            elmUiParams

        viewOneEntry : Int -> Decode.Value -> Element.Element msg
        viewOneEntry index apiListDataEntry =
            let
                updatedContext =
                    Interface.Scope.add apiListData apiListDataEntry scope

                { children, attribs } =
                    render (Just index) updatedContext

                combinedAttribs =
                    Renderer.Layout.addToOuter
                        attribsFromRendererForOuterElement
                        (mergeSharedAttribs attribs)

                { innerAttribs, wrap } =
                    wrapper combinedAttribs
            in
            layout (Renderer.Layout.addToOuter children.absolute innerAttribs) children.flow
                |> wrap
    in
    List.indexedMap
        viewOneEntry
        apiListData.values


viewWithChildren : SpecificRenderParams msg -> List (Element.Element msg)
viewWithChildren { backgroundImage, scope, elmUiParams, attribsFromRendererForOuterElement, wrapper } =
    let
        { layout, render } =
            elmUiParams

        { children, attribs } =
            render Nothing scope

        combinedAttribs =
            Renderer.Layout.addToOuter attribsFromRendererForOuterElement (mergeSharedAttribs attribs)
                |> (case backgroundImage of
                        Nothing ->
                            identity

                        Just imgSrc ->
                            Renderer.Layout.addToOuter
                                [ Ui.Style.style "background-image" ("url(" ++ imgSrc ++ ")")
                                , Ui.Style.style "background-position" "center"
                                , Ui.Style.style "background-repeat" "no-repeat"
                                ]
                   )

        { innerAttribs, wrap } =
            wrapper combinedAttribs
    in
    [ layout (Renderer.Layout.addToOuter children.absolute innerAttribs) children.flow |> wrap ]


{-| Render for the canvas: A separate render function for when content should not be interacted with but when a funk editor user is the canvas editing an app
For example, videos, buttons etc. should not be clickable.
-}
renderAndUpdateContextOnCanvas :
    Spec.Element.Model.EitherElement
    -> Model.Model.UserModel
    -> Interface.Model.ScopeData
    -> RenderFunction Canvas.Msg.Msg
renderAndUpdateContextOnCanvas element userModel scope =
    let
        preventInteraction =
            True

        content =
            Renderer.Help.getContent id userModel scope

        id =
            element.shared.id

        isSelected =
            Model.isSelected userModel element

        maybeStyle =
            Spec.Element.Id.getFromDict id userModel.elementStyles

        renderFunction : RenderFunction Canvas.Msg.Msg
        renderFunction =
            case element.outerGeometry of
                Spec.Element.Model.ScreenGeometry _ ->
                    \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                        viewWithChildren
                            { scope = scope
                            , elmUiParams = elmUiParams
                            , attribsFromRendererForOuterElement = attribsInjectedByCanvasOrPreviewRenderer
                            , wrapper = noWrapper
                            , backgroundImage = Nothing
                            }

                _ ->
                    case element.shared.kind of
                        Spec.Element.Model.TextInput { placeholder, modelField } ->
                            -- TODO: render value from model
                            \{ render } attribsInjectedByCanvasOrPreviewRenderer ->
                                let
                                    { attribs } =
                                        render Nothing scope

                                    typoPlaceholderStyles =
                                        case maybeStyle of
                                            Nothing ->
                                                DesignSystem.Typography.defaultPlaceholderStyles

                                            Just elementStyles ->
                                                case elementStyles.placeholderText of
                                                    Spec.Element.Style.TypoFromDesignSystem id_ ->
                                                        case Dict.get id_ userModel.designSystem.typoEditor.typos of
                                                            Just typo ->
                                                                DesignSystem.Typography.typoToAttributes userModel.designSystem.colorEditor typo

                                                            Nothing ->
                                                                DesignSystem.Typography.defaultPlaceholderStyles

                                                    Spec.Element.Style.CustomTypo typo ->
                                                        DesignSystem.Typography.typoToAttributes userModel.designSystem.colorEditor typo

                                                    Spec.Element.Style.NoTypo ->
                                                        DesignSystem.Typography.defaultPlaceholderStyles
                                in
                                Element.paragraph
                                    -- TODO Render only styles for placeholder in canvas
                                    (Renderer.Layout.addToOuter [ Element.clip ] (mergeSharedAttribsWithStylesOuter attribs typoPlaceholderStyles) |> Renderer.Layout.flattenAttribs)
                                    [ Element.text placeholder ]
                                    |> List.singleton

                        Spec.Element.Model.Button ->
                            case content of
                                Nothing ->
                                    \params attribsInjectedByCanvasOrPreviewRenderer ->
                                        viewWithChildren
                                            { scope = scope
                                            , elmUiParams = params
                                            , attribsFromRendererForOuterElement = attribsInjectedByCanvasOrPreviewRenderer
                                            , wrapper = noWrapper
                                            , backgroundImage = Nothing
                                            }

                                Just { value, inlineEditable } ->
                                    renderContent value inlineEditable

                        Spec.Element.Model.Box ->
                            case content of
                                Nothing ->
                                    \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                                        viewWithChildren
                                            { scope = scope
                                            , elmUiParams = elmUiParams
                                            , attribsFromRendererForOuterElement = attribsInjectedByCanvasOrPreviewRenderer
                                            , wrapper = noWrapper
                                            , backgroundImage = Nothing
                                            }

                                Just { value, inlineEditable } ->
                                    renderContent value inlineEditable

        renderContent value inlineEditable =
            case ( value, inlineEditable ) of
                -- render a paragraph if it's text content
                ( Interface.Data.ParagraphText txt, True ) ->
                    \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                        let
                            attribs =
                                (elmUiParams.render Nothing scope).attribs
                                    |> mergeSharedAttribs
                                    |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer
                                    |> Renderer.Layout.flattenAttribs
                        in
                        viewTextInteractive id isSelected attribs txt
                            |> List.singleton

                ( Interface.Data.ParagraphText txt, _ ) ->
                    \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                        let
                            attribs =
                                (elmUiParams.render Nothing scope).attribs
                                    |> mergeSharedAttribs
                                    |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer
                                    |> Renderer.Layout.flattenAttribs
                        in
                        viewText attribs txt
                            |> List.singleton

                --                ( Interface.Data.RichText txt, _ ) ->
                --                    \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                --                        let
                --                            attribs =
                --                                mergeSharedAttribs
                --                                    (elmUiParams.render Nothing scope).attribs
                --                                    ++ attribsInjectedByCanvasOrPreviewRenderer
                --                        in
                --                        viewRichText attribs txt
                --                            |> List.singleton
                ( Interface.Data.Icon icon, _ ) ->
                    \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                        let
                            attribs =
                                (elmUiParams.render Nothing scope).attribs
                                    |> mergeSharedAttribs
                                    |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer
                                    |> Renderer.Layout.flattenAttribs
                        in
                        viewIcon attribs icon
                            |> List.singleton

                ( Interface.Data.YoutubeEmbed embedInfo, _ ) ->
                    \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                        let
                            attribs =
                                (elmUiParams.render Nothing scope).attribs
                                    |> mergeSharedAttribs
                                    |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer
                                    |> Renderer.Layout.flattenAttribs
                        in
                        viewYoutubeEmbed attribs embedInfo preventInteraction
                            |> List.singleton

                {-
                   type alias ElementSketch =
                       { content : Children (Maybe FillType Image) | Replace (Image | Text ....)
                       , action : Maybe Action
                       , injected : Maybe injectedForOuter
                       }
                -}
                -- MEDIA
                ( Interface.Data.Media { kind, meta }, _ ) ->
                    case kind of
                        Interface.Data.Image ->
                            \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                                case Spec.Element.hasChildren element of
                                    True ->
                                        viewWithChildren
                                            { scope = scope
                                            , elmUiParams = elmUiParams
                                            , attribsFromRendererForOuterElement = attribsInjectedByCanvasOrPreviewRenderer
                                            , wrapper = noWrapper
                                            , backgroundImage = Just meta.src
                                            }

                                    False ->
                                        let
                                            attribs =
                                                mergeSharedAttribs
                                                    (elmUiParams.render Nothing scope).attribs
                                                    |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer
                                                    |> Renderer.Layout.flattenAttribs
                                        in
                                        viewImage attribs meta.src meta.title
                                            |> List.singleton

                        Interface.Data.Pdf ->
                            \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                                let
                                    attribs =
                                        mergeSharedAttribs
                                            (elmUiParams.render Nothing scope).attribs
                                            |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer
                                            |> Renderer.Layout.flattenAttribs
                                in
                                viewPdf attribs meta preventInteraction
                                    |> List.singleton

                        Interface.Data.Audio ->
                            \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                                let
                                    attribs =
                                        mergeSharedAttribs
                                            (elmUiParams.render Nothing scope).attribs
                                            |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer
                                            |> Renderer.Layout.flattenAttribs
                                in
                                viewAudio attribs meta preventInteraction
                                    |> List.singleton

                        Interface.Data.Video ->
                            \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                                let
                                    attribs =
                                        mergeSharedAttribs
                                            (elmUiParams.render Nothing scope).attribs
                                            |> Renderer.Layout.addToOuter attribsInjectedByCanvasOrPreviewRenderer
                                            |> Renderer.Layout.flattenAttribs
                                in
                                viewVideo element attribs maybeStyle meta preventInteraction
                                    |> List.singleton

                --                ( Interface.Data.StaticList list, _ ) ->
                --                    \elmUiParams attribsInjectedByCanvasOrPreviewRenderer ->
                --                        viewList
                --                            { scope = scope
                --                            , elmUiParams = elmUiParams
                --                            , attribsFromRendererForOuterElement = attribsInjectedByCanvasOrPreviewRenderer
                --                            , wrapper = noWrapper
                --                            }
                --                            list
                ( Interface.Data.InterfaceDataList list, _ ) ->
                    \params attribsInjectedByCanvasOrPreviewRenderer ->
                        viewInterfaceDataList
                            { scope = scope
                            , elmUiParams = params
                            , attribsFromRendererForOuterElement = attribsInjectedByCanvasOrPreviewRenderer
                            , wrapper = noWrapper
                            , backgroundImage = Nothing
                            }
                            list
    in
    renderFunction


viewPdf : List (Element.Attribute msg) -> { src : String, title : String } -> Bool -> Element.Element msg
viewPdf attribs { src, title } preventInteraction =
    let
        label =
            Element.row
                [ Element.spacing 5, Element.height Element.fill, Element.width Element.fill ]
                [ Element.el [] <| Ui.Component.icon Ui.Boxicons.bxsFilePdf
                , Element.el [ Element.moveDown 2 ] <| Element.text title
                ]

        el =
            if preventInteraction then
                Element.el attribs label

            else
                Element.newTabLink
                    attribs
                    { url = src
                    , label = label
                    }
    in
    el


viewVideo :
    Spec.Element.Model.Element a
    -> List (Element.Attribute msg)
    -> Maybe Spec.Element.Style.Style
    -> { src : String, title : String }
    -> Bool
    -> Element.Element msg
viewVideo element combinedAttribs maybeStyle { src, title } preventInteraction =
    let
        style =
            Maybe.withDefault (Spec.Element.Style.default Spec.Element.Model.Box) maybeStyle

        nopointerEvents =
            if preventInteraction then
                [ Html.Attributes.style "pointer-events" "none" ]

            else
                []

        controls =
            style.videoStyle.controls

        autoplay =
            not preventInteraction && style.videoStyle.autoplay

        -- do not autoplay if it's on the canvas
        loop =
            style.videoStyle.loop

        player =
            Element.html <|
                Html.video
                    ([ Html.Attributes.src src
                     , Html.Attributes.controls controls -- html renders empty string per default which means it won't get accepted by firefox
                     , Html.Attributes.autoplay autoplay -- html renders empty string per default which means it won't get accepted by firefox
                     , Html.Attributes.loop loop -- html renders empty string per default which means it won't get accepted by firefox
                     , Html.Attributes.style "width" "100%"
                     , Html.Attributes.style "height" "100%"
                     ]
                        ++ nopointerEvents
                        ++ (case Spec.Element.Render.renderImageCrop element style of
                                Nothing ->
                                    []

                                Just crop ->
                                    [ crop ]
                           )
                    )
                    []
    in
    Element.el (combinedAttribs ++ [ Element.spacing 10 ])
        player


viewAudio : List (Element.Attribute msg) -> { src : String, title : String } -> Bool -> Element.Element msg
viewAudio combinedAttribs { src, title } preventInteraction =
    let
        nopointerEvents =
            if preventInteraction then
                [ Html.Attributes.style "pointer-events" "none" ]

            else
                []

        player =
            Element.html <|
                Html.audio
                    ([ Html.Attributes.src src
                     , Html.Attributes.controls True
                     ]
                        ++ nopointerEvents
                    )
                    []
    in
    Element.column (combinedAttribs ++ [ Element.spacing 10 ])
        [ Element.el [ Element.width Element.fill ] player
        ]


viewYoutubeEmbed : List (Element.Attribute msg) -> { videoId : String } -> Bool -> Element.Element msg
viewYoutubeEmbed attribs embedInfo preventInteraction =
    let
        src =
            Html.Attributes.src <| "https://www.youtube-nocookie.com/embed/" ++ embedInfo.videoId

        noBorder =
            Html.Attributes.attribute "frameborder" "0"

        allow =
            Html.Attributes.attribute "allow" "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"

        allowFullscreen =
            Html.Attributes.attribute "allowfullscreen" ""

        height =
            Html.Attributes.style "height" "100%"

        nopointerEvents =
            if preventInteraction then
                [ Html.Attributes.style "pointer-events" "none" ]

            else
                []

        iframe =
            Html.iframe (nopointerEvents ++ [ height, src, noBorder, allow, allowFullscreen ]) []
    in
    Element.el attribs (Element.html iframe)


viewIcon :
    List (Element.Attribute msg)
    -> DesignSystem.IconBrowser.Model.LocallyCachedIcon
    -> Element.Element msg
viewIcon attribsInjectedByCanvasOrPreviewRenderer icon =
    DesignSystem.IconBrowser.viewLocallyCachedIcon icon
        |> Element.el attribsInjectedByCanvasOrPreviewRenderer


viewTextInteractive :
    Spec.Element.Id.Id
    -> Bool
    -> List (Element.Attribute Canvas.Msg.Msg)
    -> String
    -> Element.Element Canvas.Msg.Msg
viewTextInteractive id isSelected attribs txt =
    let
        whiteSpace =
            Element.htmlAttribute (Html.Attributes.class "funk-text")

        msg =
            Interface.Data.ParagraphText
                >> Spec.DataConnection.Static
                >> Canvas.Msg.MakeDataConnection id

        rendered =
            Element.paragraph
                (whiteSpace :: attribs)
                [ Ui.Component.contenteditable
                    { text = txt, placeholder = "Lorem ipsum", enabled = isSelected }
                    |> Element.map msg
                ]
    in
    rendered


viewImage : List (Element.Attribute msg) -> String -> String -> Element.Element msg
viewImage attribs src title =
    Element.image
        attribs
        { src = src, description = title }


viewText : List (Element.Attribute msg) -> String -> Element.Element msg
viewText attribs txt =
    let
        whiteSpace =
            Element.htmlAttribute (Html.Attributes.class "funk-text")

        rendered =
            Element.paragraph
                (whiteSpace :: attribs)
                [ Element.text txt ]
    in
    rendered
