module Preview exposing (..)

{-| A live preview for the spec currently edited by the user, usually launched in a separate browser window
Whole module just gets passed a single element on the canvas.

Rendering an element does include

  - layout
  - styles
  - connected content
  - connected actions

-}

import Action
import ApiExplorer.Api
import ApiExplorer.Api.Param
import ApiExplorer.Api.UrlParser
import ApiExplorer.Model
import Browser
import Canvas.Events
import Canvas.View
import Color.Extra
import Dict
import Dict.Any
import Dict.Extra
import Dynamic.Data
import Element
import Element.Background
import Element.Events
import Element.Font
import Google.Fonts.LinkTag
import Interface.Model
import Interface.Scope
import Json.Decode as Decode
import List.Extra
import Model
import Model.Model
import Model.Product
import Persistence
import Preview.Msg
import Rectangle
import RemoteData
import RemoteData.Http
import Renderer.Content
import Renderer.Layout
import Renderer.StyleCompositor
import Route
import Runtime.Css
import ScrollTo
import Slug
import Spec
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Spec.Element.Render
import Spec.Model
import Spec.Mutation
import Ui.Style
import Url


{-| The scope gets passed to each render function
-}
type alias Context =
    { model : Model.Model.UserModel
    , scope : Interface.Model.ScopeData
    }


viewField : Model.Model.UserModel -> ( Int, Dynamic.Data.Instance ) -> Maybe (Element.Element msg)
viewField { modelEditor } ( key, val ) =
    case Dict.get key modelEditor.fields of
        Nothing ->
            Nothing

        Just { name } ->
            Element.row
                []
                [ Element.el [ Element.width <| Element.px 100, Element.alpha 0.5 ] (Element.text name)
                , Dynamic.Data.print val
                ]
                |> Just


{-| Shared attributes that are rendered regardless of element kind (in flow element or a fixed element etc.)
-}
type alias Shared msg =
    List (Element.Attribute msg)
    -> List (Element.Element msg)


{-| Take a single canvas element and render it full screen with all its contents
-}
view : (Slug.Slug -> String) -> Slug.Slug -> RemoteData.RemoteData a Model.Model.UserModel -> Browser.Document Preview.Msg.Msg
view baseUrl slug model =
    let
        ( title, content ) =
            case model of
                RemoteData.Success userModel ->
                    viewLoaded baseUrl slug userModel

                RemoteData.Loading ->
                    Element.text "Checking out repo"
                        |> Element.layout []
                        |> List.singleton
                        |> Tuple.pair "Funk preview"

                RemoteData.NotAsked ->
                    Element.text "Waiting for data // not checking out repo yet"
                        |> Element.layout []
                        |> List.singleton
                        |> Tuple.pair "Funk preview"

                RemoteData.Failure _ ->
                    Element.text "Could not fetch repo"
                        |> Element.layout []
                        |> List.singleton
                        |> Tuple.pair "Funk preview"
    in
    { title = title, body = content }


viewLoaded baseUrl slug userModel =
    let
        screen =
            Spec.getScreensWithUniqueSlugs userModel
                |> List.Extra.find (\( screenSlug, _ ) -> screenSlug == slug)
                |> Maybe.map Tuple.second

        -- render title and html body
        res =
            case screen of
                -- screen was found
                Just s ->
                    let
                        title =
                            case s.shared.label of
                                "" ->
                                    "Previewing unnamed screen"

                                _ ->
                                    s.shared.label ++ " | Funk Preview"

                        body =
                            viewScreen
                                userModel
                                baseUrl
                                s
                    in
                    { body = body, title = title }

                Nothing ->
                    let
                        title =
                            "screen not found"
                    in
                    { body = Element.text title, title = title }

        -- dynamic / SPA part of the application
        -- only inits if there is no runtime model yet
        currentRuntimeModel =
            userModel.runtimeModel

        -- current state of the model which can change over time
        -- debug the dynamic model
        debugger =
            let
                fields =
                    Dict.toList currentRuntimeModel
                        |> List.filterMap (viewField userModel)

                viewFields =
                    Element.column [ Element.spacing 5 ] fields
            in
            Element.column
                [ Element.padding 10
                , Element.spacing 10
                , Element.Background.color Ui.Style.lightGrey
                , Element.alignBottom
                , Element.alignRight
                ]
                [ Element.el [ Element.Font.bold ] <| Element.text "State", viewFields ]
                |> Element.inFront

        -- the actual view
        content =
            [ Element.layout [ debugger ] res.body
            , Runtime.Css.render
            , additionalHead
            ]

        additionalHead =
            Google.Fonts.LinkTag.displayedFontLinkTags
                userModel
    in
    ( res.title, content )


viewScreen :
    Model.Model.UserModel
    -> (Slug.Slug -> String)
    -> Spec.Element.Model.Screen
    -> Element.Element Preview.Msg.Msg
viewScreen userModel baseUrl element =
    let
        scope =
            { model = userModel
            , scope = Interface.Scope.empty
            }

        -- shared
        spacing_ =
            Spec.Element.Render.spacing
                userModel
                (Spec.Element.wrapScreen element)
                spacing

        padding_ =
            Element.paddingEach (Renderer.Layout.formatPadding padding)

        { flow, padding, spacing } =
            element.shared

        -- design settings
        designSystem =
            userModel.designSystem

        -- render background from design settings
        styles =
            Spec.Element.Render.style
                False
                scope.scope
                userModel
                (element |> Spec.Element.wrapScreen)
                |> Maybe.map Renderer.StyleCompositor.render
                |> Maybe.withDefault []

        textColor =
            designSystem.colorEditor.text
                |> .value
                |> Color.Extra.toElmUi
                |> Element.Font.color

        -- rendering the content and also retrieving the scope
        size =
            [ Element.width Element.fill
            , Element.height Element.fill
            ]

        attributes =
            { outer = textColor :: size ++ styles ++ children.absolute
            , inner = padding_ :: spacing_
            }

        children =
            renderChildren baseUrl scope element

        elmUiElement =
            Renderer.Layout.getElmUiElementByLayout
                element.shared.flow
    in
    elmUiElement
        attributes
        children.flow



-- when the wrapper should not do anything, that is, not add a link or interactivity


noopWrap =
    Renderer.Content.noWrapper


{-| For example if you are connected to an image, return an Element.image tag
in a function which ignores all children because images can not have children
-}
getElmUiElementWithContext :
    (Slug.Slug -> String)
    -> Context
    -> Spec.Element.Model.EitherElement
    -> Renderer.Content.SharedAttribs Preview.Msg.Msg
    -> Shared Preview.Msg.Msg
getElmUiElementWithContext baseUrl scope element attribs =
    let
        renderFunction : Renderer.Content.RenderFunction Preview.Msg.Msg
        renderFunction =
            Renderer.Content.renderAndUpdateContext
                { element = element
                , wrap = onClickWrap
                , userModel = userModel
                , scope = scope.scope
                , onTextInput =
                    \targetField stringInput ->
                        Dict.singleton targetField (Just <| Dynamic.Data.StringInstance stringInput)
                            |> Preview.Msg.DispatchUpdate
                }

        layout =
            Renderer.Layout.getElmUiElementByLayout
                element.shared.flow

        onClick =
            userModel.actions
                |> Spec.Element.Id.getFromDict element.shared.id
                |> Maybe.andThen .onClick

        -- add link capabilities
        onClickWrap : Renderer.Content.Wrapper Preview.Msg.Msg
        onClickWrap =
            case onClick of
                Nothing ->
                    noopWrap

                Just clickSettings ->
                    implementClick
                        { baseUrl = baseUrl
                        , clickSettings = clickSettings
                        , userModel = userModel
                        }

        renderParams =
            { layout = layout
            , render = \maybeDataIndex updatedContext -> { children = renderChildren baseUrl { scope | scope = updatedContext } element, attribs = attribs }
            }

        withContent =
            renderFunction renderParams

        userModel =
            scope.model
    in
    withContent


implementClick :
    { baseUrl : Slug.Slug -> String
    , clickSettings : Action.ClickAction
    , userModel : Model.Model.UserModel
    }
    -> Renderer.Content.Wrapper Preview.Msg.Msg
implementClick { baseUrl, clickSettings, userModel } =
    case clickSettings of
        Action.Unspecified ->
            Renderer.Content.noWrapper

        Action.UpdateModel updateModelSettings ->
            let
                updateWrap : Renderer.Content.Wrapper Preview.Msg.Msg
                updateWrap attribs =
                    let
                        event =
                            Element.Events.onClick (Preview.Msg.DispatchUpdate updateModelSettings)
                    in
                    { wrap = identity
                    , innerAttribs = { attribs | outer = Element.pointer :: event :: attribs.outer }
                    }
            in
            updateWrap

        Action.MakeApiCall apiCallSpec ->
            let
                makeApiCallWrap : Renderer.Content.Wrapper Preview.Msg.Msg
                makeApiCallWrap attribs =
                    let
                        event =
                            Element.Events.onClick
                                (Preview.Msg.DispatchApiCall apiCallSpec)
                    in
                    { wrap = identity
                    , innerAttribs = { attribs | outer = Element.pointer :: event :: attribs.outer }
                    }
            in
            makeApiCallWrap

        Action.Navigate goToSettings ->
            implementNavigateOnClick userModel baseUrl goToSettings


implementNavigateOnClick : Model.Model.UserModel -> (Slug.Slug -> String) -> Action.NavigateParams -> Renderer.Content.Wrapper Preview.Msg.Msg
implementNavigateOnClick userModel withBaseurl goToSettings =
    let
        linkWrap : Renderer.Content.Wrapper msg
        linkWrap attribs =
            { wrap = \label -> linkFn attribs { url = url, label = label }
            , innerAttribs =
                { -- the next child of the wrp should just fill the parent
                  outer = [ Element.width Element.fill, Element.height Element.fill ]
                , inner = attribs.inner
                }
            }

        url =
            case goToSettings.linkLocation of
                Action.Internal (Just screenId) ->
                    let
                        screen =
                            Spec.getScreensWithUniqueSlugs userModel
                                |> List.Extra.find (\( _, { shared } ) -> screenId == shared.id)
                    in
                    case screen of
                        Nothing ->
                            "/screen-not-defined"

                        Just ( slug, _ ) ->
                            withBaseurl slug

                Action.Internal Nothing ->
                    "/screen-not-defined"

                Action.Scroll (Just elementId) offset ->
                    (case offset of
                        Nothing ->
                            ""

                        Just o ->
                            "?offset=" ++ String.fromInt o
                    )
                        ++ "#"
                        ++ Spec.Element.Id.toString elementId

                Action.Scroll Nothing _ ->
                    "/element-not-defined"

                Action.External { valid } ->
                    case valid of
                        Nothing ->
                            "/invalid-url/"

                        Just validUrl ->
                            Url.toString validUrl

        linkFn =
            case goToSettings.openIn of
                Action.NewTab ->
                    \attribs -> Element.newTabLink attribs.outer

                Action.SameTab ->
                    \attribs -> Element.link attribs.outer
    in
    linkWrap


renderChildren :
    (Slug.Slug -> String)
    -> Context
    -> Spec.Element.Model.Element a
    ->
        { absolute : List (Element.Attribute Preview.Msg.Msg)
        , flow : List (Element.Element Preview.Msg.Msg)
        }
renderChildren baseUrl scope element =
    let
        updateContext _ =
            scope

        flow =
            children
                |> List.concatMap
                    (\el -> renderFlowElement baseUrl (updateContext el) el)

        absolute =
            absoluteChildren
                |> List.concatMap
                    (\el -> renderAbsoluteElement baseUrl (updateContext el) el |> List.map Element.inFront)

        ( children, absoluteChildren ) =
            case element.shared.children of
                Spec.Element.Model.AbsoluteChildren abs ->
                    ( [], abs )

                Spec.Element.Model.FlowChildren fl ->
                    ( fl, [] )
    in
    { absolute = absolute
    , flow = flow
    }


renderFlowElement :
    (Slug.Slug -> String)
    -> Context
    -> Spec.Element.Model.FlowElement
    -> List (Element.Element Preview.Msg.Msg)
renderFlowElement baseUrl scope element =
    renderEitherChild
        baseUrl
        scope
        (Spec.Element.wrapFlow element)


renderAbsoluteElement :
    (Slug.Slug -> String)
    -> Context
    -> Spec.Element.Model.AbsoluteElement
    -> List (Element.Element Preview.Msg.Msg)
renderAbsoluteElement baseUrl scope element =
    renderEitherChild
        baseUrl
        scope
        (Spec.Element.wrapAbsolute element)


renderEitherChild :
    (Slug.Slug -> String)
    -> Context
    -> Spec.Element.Model.EitherElement
    -> List (Element.Element Preview.Msg.Msg)
renderEitherChild baseUrl scope element =
    let
        shared =
            renderShared
                baseUrl
                scope
                element
    in
    shared []


renderShared : (Slug.Slug -> String) -> Context -> Spec.Element.Model.EitherElement -> Shared Preview.Msg.Msg
renderShared baseUrl scope element =
    let
        -- shared
        spacing_ =
            Spec.Element.Render.spacing
                userModel
                element
                spacing

        padding_ =
            Element.paddingEach (Renderer.Layout.formatPadding padding)

        { flow, padding, spacing } =
            element.shared

        -- layout =
        --     [ Element.width Element.fill
        --     , Element.height Element.fill
        --     ]
        --
        styles =
            Canvas.View.renderStyles
                scope.scope
                userModel
                element
                |> Maybe.withDefault []

        userModel =
            scope.model

        elmUiElement =
            getElmUiElementWithContext
                baseUrl
                scope
                element

        id =
            Spec.Element.Id.toHtmlId element.shared.id

        -- tool can override geometry
        outerGeometry =
            renderOuterGeometry userModel scope.scope element

        attribs =
            id :: styles

        innerGeometry =
            padding_ :: spacing_
    in
    elmUiElement { geometry = { inner = innerGeometry, outer = outerGeometry }, other = attribs }


renderOuterGeometry :
    Model.Model.UserModel
    -> Interface.Model.ScopeData
    -> Spec.Element.Model.EitherElement
    -> List (Element.Attribute Preview.Msg.Msg)
renderOuterGeometry userModel scope element =
    case element.outerGeometry of
        Spec.Element.Model.ScreenGeometry screenSize ->
            let
                (Canvas.Events.AbsoluteRectangle size) =
                    Spec.Mutation.screenSizeToAbsoluteRectangle screenSize

                outerGeometry =
                    Rectangle.render size

                renderedGeometry =
                    outerGeometry
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
                        scope
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
            Spec.getAbsoluteElementRectangle userModel scope rect
                |> (\(Canvas.Events.ElementRectangle size) -> size)
                |> Rectangle.render


type alias Return =
    { userModel : Model.Model.UserModel
    , cmd : Cmd Preview.Msg.Msg
    , scrollTo : ScrollTo.State
    }


compileApiCallData ({ modelToCallMap } as apiCallSpec) userModel =
    case Model.getCall apiCallSpec userModel of
        Nothing ->
            Err "API call not found"

        -- call not defined
        Just apiCall ->
            let
                collectedParams =
                    Model.collectParams userModel apiCallSpec

                -- did we find all the values that we need in the model?
                hasAllParams : Bool
                hasAllParams =
                    Dict.Any.size modelToCallMap == List.length collectedParams

                allParamsValid =
                    ApiExplorer.Api.Param.validateAll collectedParams
            in
            case hasAllParams of
                True ->
                    case allParamsValid of
                        False ->
                            Err "Some params are invalid"

                        True ->
                            Ok ( apiCall, collectedParams )

                False ->
                    Err "Not all params found"


update : Preview.Msg.Msg -> Model.Product.Mode -> Url.Url -> ScrollTo.State -> Model.Model.UserModel -> Return
update msg mode url scrollTo userModel =
    case msg of
        Preview.Msg.DispatchApiCall apiCallSpec ->
            case compileApiCallData apiCallSpec userModel of
                Err _ ->
                    Return userModel Cmd.none scrollTo

                Ok ( call, params ) ->
                    let
                        { method, requestBody } =
                            call

                        bakedUrl =
                            ApiExplorer.Api.UrlParser.urlToString call.url

                        payload =
                            ApiExplorer.Api.bakeBody
                                { body = requestBody
                                , params = params
                                }

                        cmd =
                            case ( method, payload ) of
                                ( ApiExplorer.Model.Post, Just bakedPayload ) ->
                                    RemoteData.Http.post
                                        bakedUrl
                                        (always Preview.Msg.NoOp)
                                        (Decode.succeed ())
                                        bakedPayload

                                ( ApiExplorer.Model.Get, _ ) ->
                                    RemoteData.Http.get
                                        bakedUrl
                                        (always Preview.Msg.NoOp)
                                        (Decode.succeed ())

                                _ ->
                                    Cmd.none
                    in
                    Return userModel cmd scrollTo

        Preview.Msg.ScrollToMsg scrollToMsg ->
            let
                ( scrollToModel, scrollToCmds ) =
                    ScrollTo.update
                        scrollToMsg
                        scrollTo
            in
            Return
                userModel
                (Cmd.map Preview.Msg.ScrollToMsg scrollToCmds)
                scrollToModel

        Preview.Msg.NoOp ->
            Return userModel Cmd.none scrollTo

        Preview.Msg.SpecUpdated ( updateFor, spec ) ->
            case Route.getProjectData mode url of
                Nothing ->
                    Return userModel Cmd.none scrollTo

                Just meta ->
                    if updateFor == meta then
                        let
                            runtimeModel =
                                userModel.runtimeModel

                            newUserModel =
                                spec
                                    |> Model.initUserModel
                                    |> Model.initRuntimeModel
                                    |> (\userModel_ -> { userModel_ | runtimeModel = Dict.union userModel_.runtimeModel runtimeModel })

                            -- preserve state on hot reload
                        in
                        Return
                            newUserModel
                            Cmd.none
                            scrollTo

                    else
                        Return userModel Cmd.none scrollTo

        Preview.Msg.DispatchUpdate updates ->
            let
                um =
                    { userModel
                        | runtimeModel =
                            Dict.union
                                (updates |> Dict.Extra.filterMap (always identity))
                                userModel.runtimeModel
                    }
            in
            { userModel = um, cmd = Cmd.none, scrollTo = scrollTo }


subscriptions : ScrollTo.State -> Sub Preview.Msg.Msg
subscriptions scrollTo =
    let
        scrollSubs =
            ScrollTo.subscriptions scrollTo
                |> Sub.map Preview.Msg.ScrollToMsg

        injectSub =
            Persistence.previewSpecUpdated parseGenericUpdate
    in
    Sub.batch [ injectSub, scrollSubs ]


parseGenericUpdate : ( Persistence.ProjectMeta, String ) -> Preview.Msg.Msg
parseGenericUpdate ( projectMeta, spec ) =
    case Decode.decodeString Spec.Model.decodeSpec spec of
        Err _ ->
            Preview.Msg.NoOp

        Ok savedState ->
            Preview.Msg.SpecUpdated ( projectMeta, savedState )
