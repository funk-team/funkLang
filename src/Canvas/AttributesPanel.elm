module Canvas.AttributesPanel exposing (view)

import Canvas.AttributesPanel.Actions
import Canvas.AttributesPanel.Content
import Canvas.AttributesPanel.Layout
import Canvas.AttributesPanel.Shared
import Canvas.AttributesPanel.Style
import Canvas.AttributesPanel.Tabs
import Canvas.Msg
import Canvas.Selection
import Element
import Element.Border
import Element.Events
import Element.Events.Extra
import Element.Font
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Model
import Model.Model
import Route
import Spec.Element
import Spec.Element.Model
import Spec.Mutation
import Ui.Help
import Ui.Style
import Ui.Tabs


{-| Render the sidebar that allows you to modify the values related to an element
-}
view :
    Model.Model.Model
    -> Model.Model.UserModel
    -> Element.Element Canvas.Msg.RootMsg
view model userModel =
    case userModel.selection of
        -- if nothing is selected, allow the user to connect to a CLI
        Nothing ->
            emptyPanel
                |> Element.map Canvas.Msg.EditorMsg

        -- if something is selected, find the element and display the panel
        Just selectionItem ->
            Element.map Canvas.Msg.EditorMsg <|
                case Spec.Mutation.drillDownAndGet selectionItem userModel of
                    Just el ->
                        viewElementOptions model ( selectionItem, el )

                    Nothing ->
                        emptyPanel


emptyPanel =
    Element.el
        panelStyles
        Element.none


{-| display all options related to an element. This is a helper to make other functions less cluttered.
-}
viewElementOptions :
    Model.Model.Model
    -> ( Canvas.Selection.SelectionItem, Spec.Element.Model.EitherElement )
    -> Element.Element Canvas.Msg.Msg
viewElementOptions model ( selectionItem, element ) =
    let
        userModel =
            Model.latest model

        id =
            element.shared.id

        selected : Bool
        selected =
            Model.isSelected userModel element
    in
    case element.outerGeometry of
        Spec.Element.Model.ScreenGeometry _ ->
            renderScreenPanel
                model
                ( selectionItem, element )

        _ ->
            renderElementPanel
                model
                ( selectionItem, element )


{-| View the editable overlay for editing element styling, content etc.
-}
renderScreenPanel :
    Model.Model.Model
    -> ( Canvas.Selection.SelectionItem, Spec.Element.Model.EitherElement )
    -> Element.Element Canvas.Msg.Msg
renderScreenPanel model ( selectionItem, element ) =
    let
        userModel =
            Model.latest model

        panelAttribs =
            Ui.Help.allPointerEvents
                :: Element.Events.Extra.preventClickOnElementsBehind Canvas.Msg.NoOp
                ++ panelStyles

        panelHeader =
            Ui.Tabs.tabs
                Canvas.AttributesPanel.Tabs.tabToString
                Canvas.AttributesPanel.Tabs.tabs
                userModel.attributesPanelTab
                |> Element.map Canvas.Msg.AttributesPanelTabChanged
                |> Element.el Canvas.AttributesPanel.Shared.sectionStyles

        content =
            case userModel.attributesPanelTab of
                Canvas.AttributesPanel.Tabs.Style ->
                    Canvas.AttributesPanel.Style.view
                        selectionItem
                        model
                        element

                Canvas.AttributesPanel.Tabs.Content ->
                    [ Element.text "You can not add content directly to a screen." ]
                        |> Element.paragraph Canvas.AttributesPanel.Shared.sectionStyles

                Canvas.AttributesPanel.Tabs.Layout ->
                    Canvas.AttributesPanel.Layout.view
                        model
                        ( selectionItem, element )

                Canvas.AttributesPanel.Tabs.Actions ->
                    [ Element.text "You can not add actions directly to a screen." ]
                        |> Element.paragraph Canvas.AttributesPanel.Shared.sectionStyles
    in
    Element.column
        panelAttribs
        [ panelHeader, content ]


{-| View the editable overlay for editing element styling, content etc.
-}
renderElementPanel :
    Model.Model.Model
    -> ( Canvas.Selection.SelectionItem, Spec.Element.Model.EitherElement )
    -> Element.Element Canvas.Msg.Msg
renderElementPanel model ( selectionItem, element ) =
    let
        userModel =
            Model.latest model

        panelHeader =
            Ui.Tabs.tabs
                Canvas.AttributesPanel.Tabs.tabToString
                Canvas.AttributesPanel.Tabs.tabs
                userModel.attributesPanelTab
                |> Element.map Canvas.Msg.AttributesPanelTabChanged
                |> Element.el Canvas.AttributesPanel.Shared.sectionStyles

        camera =
            userModel.camera

        panelAttribs =
            Ui.Help.allPointerEvents
                :: Element.Events.Extra.preventClickOnElementsBehind Canvas.Msg.NoOp
                ++ panelStyles

        panel =
            Element.column
                panelAttribs
                [ panelHeader, content ]

        id =
            element.shared.id

        imageStyles =
            [ Element.width (Element.px 650)
            , Element.Border.glow Ui.Style.grey 3
            , Element.moveDown 10
            ]

        content =
            case userModel.attributesPanelTab of
                Canvas.AttributesPanel.Tabs.Style ->
                    Canvas.AttributesPanel.Style.view
                        selectionItem
                        model
                        element

                Canvas.AttributesPanel.Tabs.Content ->
                    case Route.getProjectData model.mode model.url of
                        Nothing ->
                            Element.none

                        Just projectMeta ->
                            Canvas.AttributesPanel.Content.viewForElement
                                { id = id
                                , selectionItem = selectionItem
                                , userModel = userModel
                                , element = element
                                , projectMeta = projectMeta
                                }

                Canvas.AttributesPanel.Tabs.Layout ->
                    Canvas.AttributesPanel.Layout.view model ( selectionItem, element )

                Canvas.AttributesPanel.Tabs.Actions ->
                    Canvas.AttributesPanel.Actions.view selectionItem element userModel
    in
    panel


panelStyles =
    [ Element.Border.widthEach { left = 1, right = 0, bottom = 0, top = 0 }
    , Element.Border.color Ui.Style.slightAccent
    , Element.alignRight
    , Element.height Element.fill
    , Element.Font.color Ui.Style.black
    , Element.htmlAttribute (Html.Attributes.style "cursor" "default")
    , Ui.Style.panelBackground
    , Ui.Help.allPointerEvents
    , Element.width (Element.px Ui.Style.sidebarWidth)
    , Element.Font.size 12

    -- required to make it scroll
    , stopWheelPropagation
    , Ui.Style.style "flex-basis" "0"
    , Element.scrollbarY
    , Element.clipX
    ]


stopWheelPropagation =
    Decode.succeed ( Canvas.Msg.NoOp, True )
        |> Html.Events.stopPropagationOn "wheel"
        |> Element.htmlAttribute
