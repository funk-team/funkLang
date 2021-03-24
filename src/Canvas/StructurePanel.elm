module Canvas.StructurePanel exposing (viewAllScreens)

{-| The structure panel recursively renders each element and allows emitting events for that specific element.
-}

import Canvas.Camera.Model
import Canvas.Msg
import Canvas.Selection
import Canvas.Tool.Responsify
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Events.Extra
import Element.Font
import Element.Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Keyboard
import List.Extra
import Model
import Model.Model
import Spec
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Ui.Boxicons
import Ui.Component
import Ui.Help
import Ui.Style



{-
   selectionDebug userModel =
       case userModel.selection of
           Nothing -> Element.none
           Just sel -> case sel of
               Canvas.Selection.SelectedRoot root -> Element.text (Debug.toString root)
               Canvas.Selection.SelectedShallow root el ->
                   Element.column
                       [Element.alignBottom]
                       [Element.text (Debug.toString root)
                       , Element.text (Debug.toString el)
                       ]
               Canvas.Selection.SelectedDeeper root {path, target, parent} ->
                   Element.column
                       [ Element.alignBottom
                       ]
                       [Element.text (Debug.toString root)
                       , Element.text (Debug.toString path)
                       , Element.text (Debug.toString parent)
                       , Element.text (Debug.toString target)
                       ]

-}


viewAllScreens :
    Model.Model.UserModel
    -> Element.Element Canvas.Msg.Msg
viewAllScreens userModel =
    let
        -- get the first screen that contains an element with the selected ID
        els =
            userModel.itemsOnCanvas
                |> List.filterMap
                    (\screen ->
                        case getAllExpanded screen userModel [] of
                            [] ->
                                -- element will not be marked as expanded if it is selected - however we want to show the structure panel if it is selected.
                                if Model.isSelected userModel screen then
                                    viewSelectedScreen userModel screen |> Just

                                else
                                    Nothing

                            _ ->
                                viewSelectedScreen userModel screen
                                    |> Just
                    )
                |> List.head
    in
    case els of
        Nothing ->
            let
                items =
                    List.indexedMap (viewScreen userModel) (List.map Spec.Element.wrapScreen userModel.itemsOnCanvas)
            in
            Element.column
                [ Ui.Help.allPointerEvents
                , Element.alignLeft
                , width
                , Ui.Style.style "height" "calc(100vh - 40px)"
                , Ui.Style.style "overflow-y" "scroll"
                , Ui.Style.class "sidebar"
                , Ui.Style.panelBackground
                , Element.Border.widthEach { left = 0, right = 1, bottom = 0, top = 0 }
                , Element.Border.color Ui.Style.slightAccent
                ]
                (structurePanelHeader Nothing
                    ++ items
                )

        Just el ->
            el


structurePanelHeader : Maybe String -> List (Element.Element Canvas.Msg.Msg)
structurePanelHeader title =
    let
        border =
            [ Element.Border.color Ui.Style.grey
            , Element.Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            , Element.width Element.fill
            , Element.height (Element.px 40)
            , Element.Font.color Ui.Style.black
            , Element.Font.size 14
            , Element.spacingXY 15 0
            ]

        onClickToggle =
            Html.Events.stopPropagationOn "click"
                (Decode.succeed ( Canvas.Msg.Select Nothing, True ))
                |> Element.htmlAttribute

        screenSelected =
            case title of
                Nothing ->
                    [ Element.Font.color Ui.Style.black ]

                Just _ ->
                    [ Element.Font.color Ui.Style.grey
                    , Element.mouseOver <|
                        [ Element.Font.color Ui.Style.black ]
                    , Element.pointer
                    , onClickToggle
                    ]
    in
    [ Element.row border
        [ Element.el ([ Element.centerY, Element.moveRight 5 ] ++ screenSelected)
            (Element.text "Screens")
        , Element.el [ Element.centerY, Element.moveRight 5 ]
            (Element.text (title |> Maybe.withDefault ""))
        ]
    ]


viewSelectedScreen : Model.Model.UserModel -> Spec.Element.Model.Screen -> Element.Element Canvas.Msg.Msg
viewSelectedScreen userModel element =
    let
        selectionItem =
            Canvas.Selection.fresh element.shared.id

        els =
            viewElement expanded userModel selectionItem (Spec.Element.wrapScreen element)

        expanded =
            getAllExpanded element userModel userModel.expanded

        children =
            [ Element.column [ Element.spacing 40, Element.width Element.fill ] [ els ] ]

        parent =
            -- TODO Fix when incorrect parent is highlighted
            getParent userModel userModel.expanded element

        panelStyles =
            [ Element.spacing 10
            , Ui.Help.allPointerEvents
            , Element.height Element.fill
            , Element.alignLeft
            , width
            , Ui.Style.panelBackground
            , Element.Border.widthEach { left = 0, right = 1, bottom = 0, top = 0 }
            , Element.Border.color Ui.Style.slightAccent

            -- , Ui.Style.style "flex-basis" "0"
            , Element.scrollbarY
            , Element.clipX
            , Ui.Style.style "height" "calc(100vh - 40px)"
            , Ui.Style.style "overflow-y" "scroll"
            , Ui.Style.class "sidebar"
            ]
    in
    Element.column
        panelStyles
        (structurePanelHeader (Just "Screen Structure")
            ++ children
        )


stopWheelPropagation =
    Decode.succeed ( Canvas.Msg.NoOp, True )
        |> Html.Events.stopPropagationOn "wheel"
        |> Element.htmlAttribute


{-| Find all IDs of parents whos children are selected
-}
getAllExpanded :
    Spec.Element.Model.Element a
    -> Model.Model.UserModel
    -> Canvas.Selection.ExpansionSet
    -> Canvas.Selection.ExpansionSet
getAllExpanded el model explicitExpanded =
    help model explicitExpanded el
        |> Tuple.second


help :
    Model.Model.UserModel
    -> Canvas.Selection.ExpansionSet
    -> Spec.Element.Model.Element a
    -> ( Bool, List Spec.Element.Id.Id )
help model explicitExpanded el =
    let
        selected =
            Model.isSelected model el

        expanded =
            List.member el.shared.id explicitExpanded

        children =
            el
                |> Spec.Element.getChildren

        childrenResult =
            List.map
                (help_ model explicitExpanded)
                children

        expandedChildren =
            List.concatMap Tuple.second childrenResult

        anyChildSelected =
            List.any Tuple.first childrenResult

        anyChildExpanded =
            not <| List.isEmpty <| List.concatMap Tuple.second <| childrenResult

        shouldExpand =
            expanded || anyChildSelected || anyChildExpanded
    in
    ( selected
    , if shouldExpand then
        el.shared.id :: expandedChildren

      else
        expandedChildren
    )


help_ =
    help


getParent :
    Model.Model.UserModel
    -> Canvas.Selection.ExpansionSet
    -> Spec.Element.Model.Element a
    -> Maybe Spec.Element.Id.Id
getParent model explicitExpanded el =
    let
        children =
            el
                |> Spec.Element.getChildren

        childrenResult =
            List.map
                (help model explicitExpanded)
                children

        parent =
            List.concatMap Tuple.second childrenResult |> List.Extra.last
    in
    parent


viewScreen :
    Model.Model.UserModel
    -> Int
    -> Spec.Element.Model.Element Spec.Element.Model.EitherOuterGeometry
    -> Element.Element Canvas.Msg.Msg
viewScreen userModel index element =
    let
        distilledMeta : DistilledMeta
        distilledMeta =
            DistilledMeta
                False
                False
                False
                False
                selectionItem
                False

        selectionItem =
            Canvas.Selection.SelectedRoot element.shared.id

        input =
            labelInput
                userModel
                distilledMeta
                selectionItem
                element
                (Just index)
                Screen
                userModel.camera
    in
    input
        |> Element.el [ Element.width Element.fill, Element.alignLeft ]


width =
    Element.width <| Element.px Ui.Style.sidebarWidth


renderBoth :
    Model.Model.UserModel
    -> Canvas.Selection.ExpansionSet
    -> Canvas.Selection.SelectionItem
    -> Spec.Element.Model.Element size
    ->
        { flow : List (Element.Element Canvas.Msg.Msg)
        , absolute : List (Element.Element Canvas.Msg.Msg)
        }
renderBoth userModel expanded selectionItem element =
    let
        ( flow, absolute ) =
            case element.shared.children of
                Spec.Element.Model.FlowChildren f ->
                    ( f, [] )

                Spec.Element.Model.AbsoluteChildren abs ->
                    ( [], abs )
    in
    { flow =
        List.map
            (\el -> viewElement expanded userModel (Canvas.Selection.addOne el.shared.id selectionItem) el)
            (List.map Spec.Element.wrapFlow flow)
    , absolute =
        List.map
            (\el -> viewElement expanded userModel (Canvas.Selection.addOne el.shared.id selectionItem) el)
            (List.map Spec.Element.wrapAbsolute absolute)
    }


viewElement :
    Canvas.Selection.ExpansionSet
    -> Model.Model.UserModel
    -> Canvas.Selection.SelectionItem
    -> Spec.Element.Model.Element Spec.Element.Model.EitherOuterGeometry
    -> Element.Element Canvas.Msg.Msg
viewElement expanded userModel selectionItem element =
    let
        { flow, absolute } =
            renderBoth userModel expanded selectionItem element

        isExpanded =
            List.member element.shared.id expanded

        isExplicitlyExpanded =
            List.member element.shared.id userModel.expanded

        isResponsified =
            Spec.Element.isResponsified element

        isSelected =
            Model.isSelected userModel element

        isEditingLabel =
            userModel.editingLabelOn == Just element.shared.id

        distilledMeta : DistilledMeta
        distilledMeta =
            DistilledMeta
                isExpanded
                isExplicitlyExpanded
                isEditingLabel
                isSelected
                selectionItem
                isResponsified

        children =
            if isExpanded && Spec.Element.hasChildren element then
                Element.column
                    [ Element.width Element.fill
                    , Element.paddingEach { left = 10, right = 0, bottom = 0, top = 0 }
                    ]
                    [ Element.row [ Element.width Element.fill ]
                        [ Element.column [ Element.width Element.fill ] (absolute ++ flow) ]
                    ]

            else
                Element.none

        labelAndExpander =
            labelInput userModel distilledMeta selectionItem element Nothing Element userModel.camera
    in
    Element.column
        [ Element.width (Element.fill |> Element.maximum 300)
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ labelAndExpander
            ]
        , children
        ]


responsifyIconToolTip =
    "Element not responsive yet. Click 'Responsify' in the Layout panel to make the element resize when the screen size changes'"


type alias DistilledMeta =
    { isExpanded : Bool
    , isExplicitlyExpanded : Bool
    , isEditingLabel : Bool
    , isSelected : Bool
    , selectionItem : Canvas.Selection.SelectionItem
    , isResponsified : Bool
    }


type ScreenOrElement
    = Screen
    | Element


{-| If the user is editing the label, render an input field
-}
labelInput :
    Model.Model.UserModel
    -> DistilledMeta
    -> Canvas.Selection.SelectionItem
    -> Spec.Element.Model.Element Spec.Element.Model.EitherOuterGeometry
    -> Maybe Int
    -> ScreenOrElement
    -> Canvas.Camera.Model.Model
    -> Element.Element Canvas.Msg.Msg
labelInput userModel distilledMeta selectionItem element index screenOrElement camera =
    let
        screenOrElementName =
            case screenOrElement of
                Screen ->
                    Canvas.Selection.screenIndexToLabel element.shared.label (index |> Maybe.withDefault 0)

                Element ->
                    Spec.Element.Id.toHtmlIdRaw element.shared.id

        screenOrElementName_ =
            if String.isEmpty element.shared.label then
                screenOrElementName

            else
                element.shared.label

        responsifyIcon =
            case ( distilledMeta.isResponsified, screenOrElement ) of
                ( True, _ ) ->
                    Element.none

                ( False, Element ) ->
                    responsifyButton element selectionItem camera

                ( False, Screen ) ->
                    Element.none

        placeholder =
            Element.Input.placeholder
                [ Element.Font.color Ui.Style.grey
                , if element.shared.label == "" then
                    Element.alpha 0.6

                  else
                    Element.transparent True
                , Element.Border.color Ui.Style.black
                , Element.Border.width 1
                ]
                (Element.text screenOrElementName_)

        completeOnClick : Element.Attribute Canvas.Msg.Msg
        completeOnClick =
            Canvas.Msg.FinishLabelEdit
                |> Element.Events.onClick

        result =
            case distilledMeta.isEditingLabel of
                True ->
                    Element.row
                        [ Element.width Element.fill
                        , Element.clip
                        ]
                        [ collapseButton_
                        , textInput
                        , Element.el [ completeOnClick, Element.pointer ] (Ui.Component.icon Ui.Boxicons.bxCheck)
                        ]

                False ->
                    textDisplay

        onClickSelect =
            Html.Events.stopPropagationOn "click"
                (Decode.succeed ( Canvas.Msg.Select (Just distilledMeta.selectionItem), True ))
                |> Element.htmlAttribute

        collapseButton_ =
            collapseButton
                distilledMeta
                element

        textDisplay =
            Element.row
                [ Element.width Element.fill
                , Element.Events.onDoubleClick (Canvas.Msg.StartLabelEdit element.shared.id)
                , Element.clip
                , Element.Background.color backgroundColor
                ]
                [ collapseButton_
                , typeIcon userModel element screenOrElement
                , Element.row
                    [ Element.height <| Element.px 30
                    , Element.width Element.fill
                    , Element.padding 4
                    , Element.pointer
                    , onClickSelect
                    ]
                    [ Element.el [] (Element.text screenOrElementName_)
                    , responsifyIcon
                    ]
                ]

        completeOnEnterKey : Element.Attribute Canvas.Msg.Msg
        completeOnEnterKey =
            (Keyboard.eventKeyDecoder
                |> Decode.map Keyboard.whitespaceKey
                |> Decode.andThen
                    (\key ->
                        case key of
                            Just Keyboard.Enter ->
                                Decode.succeed Canvas.Msg.FinishLabelEdit

                            _ ->
                                Decode.fail "Confirm only on enter key"
                    )
            )
                |> Html.Events.on "keydown"
                |> Element.htmlAttribute

        completeOnBlur : Element.Attribute Canvas.Msg.Msg
        completeOnBlur =
            Canvas.Msg.FinishLabelEdit
                |> Element.Events.onLoseFocus

        ( backgroundColor, fontColor ) =
            if distilledMeta.isSelected then
                ( Ui.Style.highlightColorMid, Ui.Style.black )

            else
                ( Ui.Style.white, Ui.Style.black )

        textInput =
            Element.Input.text
                [ Element.Events.onFocus <| Canvas.Msg.Select (Just selectionItem)
                , completeOnEnterKey
                , completeOnBlur
                , Element.height <| Element.px 30
                , Element.padding 4
                , Element.width Element.fill
                , Element.pointer
                ]
                { label =
                    Element.Input.labelAbove
                        [ Element.width Element.fill ]
                        Element.none
                , text = element.shared.label
                , onChange = Canvas.Msg.SetLabel selectionItem
                , placeholder =
                    Just placeholder
                }
    in
    result


responsifyButton :
    Spec.Element.Model.Element Spec.Element.Model.EitherOuterGeometry
    -> Canvas.Selection.SelectionItem
    -> Canvas.Camera.Model.Model
    -> Element.Element Canvas.Msg.Msg
responsifyButton element selectionItem camera =
    let
        event : Decode.Decoder Canvas.Msg.Msg
        event =
            Canvas.Tool.Responsify.readLayoutData camera element.shared.id
                |> Decode.map
                    (Canvas.Msg.ResponsifySidebarBtnClicked ( selectionItem, element ))

        style =
            [ Element.alignRight
            , Element.htmlAttribute <| Html.Attributes.title responsifyIconToolTip
            , Element.Border.width 1
            , Element.Border.rounded 2
            , Element.Border.color Ui.Style.black
            , Element.padding 5
            , Element.centerY
            , Canvas.Tool.Responsify.forElement element
            , Element.Font.size 10
            , Element.mouseOver
                [ Element.Background.color Ui.Style.black
                , Element.Font.color Ui.Style.white
                ]
            ]
    in
    case element.shared.children of
        Spec.Element.Model.FlowChildren _ ->
            Element.none

        Spec.Element.Model.AbsoluteChildren [] ->
            Element.none

        Spec.Element.Model.AbsoluteChildren _ ->
            Element.el [ Canvas.Tool.Responsify.forElement element, Element.Events.Extra.onMouseDown event, Element.alignRight ] <|
                Element.el
                    style
                    (Element.text <| "R")


typeIcon :
    Model.Model.UserModel
    -> Spec.Element.Model.EitherElement
    -> ScreenOrElement
    -> Element.Element Canvas.Msg.Msg
typeIcon userModel element screenOrElement =
    let
        attribs =
            [ Element.width (Element.px 15)
            , Element.height (Element.px 15)
            ]

        content =
            Element.row
                [ Element.centerY
                , Element.width Element.fill
                , Element.Font.center
                , Element.Font.color Ui.Style.grey
                , Element.height (Element.px 5)
                ]
                [ case screenOrElement of
                    Screen ->
                        Ui.Component.icon Ui.Boxicons.bxWindow

                    Element ->
                        case element.shared.kind of
                            Spec.Element.Model.Box ->
                                case Spec.hasTextConnection element userModel of
                                    False ->
                                        Ui.Component.icon Ui.Boxicons.bxCodeBlock

                                    True ->
                                        Ui.Component.icon Ui.Boxicons.bxFont

                            Spec.Element.Model.TextInput _ ->
                                Ui.Component.icon Ui.Boxicons.bxPencil

                            Spec.Element.Model.Button ->
                                Ui.Component.icon Ui.Boxicons.bxMouse
                ]
    in
    Element.el attribs content


collapseButton :
    DistilledMeta
    -> Spec.Element.Model.Element a
    -> Element.Element Canvas.Msg.Msg
collapseButton { isExplicitlyExpanded, isExpanded } element =
    let
        icon =
            case ( isImplicitlyExpanded, isExpanded ) of
                ( True, _ ) ->
                    Ui.Component.icon Ui.Boxicons.bxChevronDown

                ( False, True ) ->
                    Ui.Component.icon Ui.Boxicons.bxChevronDown

                ( False, False ) ->
                    Ui.Component.icon Ui.Boxicons.bxChevronRight

        attribs =
            [ Element.width (Element.px 15)
            , Element.height (Element.px 15)
            , Element.pointer
            ]
                ++ events

        content =
            Element.row
                [ Element.centerY
                , Element.width Element.fill
                , Element.Font.center
                , Element.Font.color Ui.Style.grey
                , Element.height (Element.px 5)
                ]
                [ if Spec.Element.hasChildren element then
                    icon

                  else
                    Element.none
                ]

        events =
            if isImplicitlyExpanded || (Spec.Element.hasChildren element |> not) then
                []

            else
                [ onClickToggle ]

        isImplicitlyExpanded =
            isExpanded && not isExplicitlyExpanded

        onClickToggle =
            Html.Events.stopPropagationOn "click"
                (Decode.succeed ( Canvas.Msg.ToggleExpand element.shared.id, True ))
                |> Element.htmlAttribute
    in
    Element.el attribs content
